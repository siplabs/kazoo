%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_msg).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([process_route_req/3]).
-export([handle_sms_forward/2]).

-record(state, {
          node :: atom()
          ,options :: wh_proplist()
         }).

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'sms', []}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [
                     {{?MODULE, 'handle_sms_forward'}, [{<<"sms">>, <<"forward">>}]}
                    ]).

-define(QUEUE_NAME, <<"ecallmgr_fs_msg">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include_lib("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), wh_proplist()) -> startlink_ret().

start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE
                            ,[{'responders', ?RESPONDERS}
                              ,{'bindings', ?BINDINGS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[Node, Options]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    lager:debug("starting new ecallmgr msg process"),
    gproc:reg({'p', 'l', 'fs_msg'}),
    gen_server:cast(self(), 'bind_to_chatplan'),
    {'ok', #state{node=Node, options=Options}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast('bind_to_msg_events', #state{node=Node}=State) ->
    _ = case ecallmgr_config:get(<<"distribute_recv_message">>, 'false') of
            false -> ok;
            true ->
                gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"RECV_MESSAGE">>)}),
                gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"MESSAGE">>)}),
                lager:debug("bound to recv_message events on node ~s", [Node])
        end,
    {'noreply', State};
    
handle_cast('bind_to_chatplan', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'chatplan') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish route bindings: ~p", [Reason]),
            {'stop', Reason, State}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'fetch', 'chatplan', _Tag, _Key, _Value, FSId, ['undefined' | FSData] }, #state{node=Node}=State) ->
    lager:info("CHAT PLAN ~p",[FSData]),
    lager:info("CHAT PLAN ~p",[props:get_value(<<"Event-Name">>, FSData)]),
    lager:info("CHAT PLAN ~p",[props:get_value(<<"Caller-Context">>, FSData)]),
    case {props:get_value(<<"Event-Name">>, FSData)
          ,props:get_value(<<"context">>, FSData)
         }
    of
        {<<"MESSAGE">>, _Context} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing chatplan fetch request ~s from ~s", [FSId,  Node]),
            spawn(?MODULE, 'process_route_req', [Node, FSId, FSData]),
            {'noreply', State, 'hibernate'};
        {_Other, _Context} ->
            lager:debug("ignoring event ~s in context ~s from ~s", [_Other, _Context, Node]),
            {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, FSId, 'chatplan', Resp),
            {'noreply', State, 'hibernate'}
    end;
handle_info({'fetch', _Section, _Something, _Key, _Value, Id, ['undefined' | _Data]}, #state{node=Node}=State) ->
    lager:info("CHAT PLAN"),
    lager:warning("fetch unknown section from ~s: ~p So: ~p, K: ~p V: ~p Id: ~s Data: ~p"
                  ,[Node, _Section, _Something, _Key, _Value, Id, _Data]),
    {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, Id, 'chatplan', Resp),
    {'noreply', State};
handle_info({'event', [_ | Props]}, #state{node=Node}=State) ->
    lager:info("fs event"),
    _ = case props:get_value(<<"Event-Name">>, Props) of
            <<"RECV_MESSAGE">> -> spawn_link(?MODULE, process_msg, [Props, Node]);            
            <<"MESSAGE">> -> spawn_link(?MODULE, process_msg, [Props, Node]);            
            _ -> 'ok'
        end,
    {'noreply', State, 'hibernate'};
handle_info({'EXIT', _, _}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{node=Node}) ->
    lager:info("notify listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_target(To, Username) ->
    [ToUser, ToRealm] = binary:split(To , <<"@">>),
    lager:info("To Parts ~p / ~p",[ToUser, ToRealm]),
    Target = <<Username/binary, "@", ToRealm/binary>>.

get_caller_id(CID, From) ->
    [FromUser, FromRealm] = binary:split(From , <<"@">>),
    lager:info("From Parts ~p / ~p",[FromUser, FromRealm]),
    CallerID = <<CID/binary, "@", FromRealm/binary>>.
    

-spec handle_sms_forward(wh_json:object(), wh_proplist()) -> no_return().
handle_sms_forward(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    lager:info("process_message received SIP/SIMPLE Msg : ~p", [Props]),
    lager:info("process_message received SIP/SIMPLE Msg : ~p", [JObj]),
    Node = props:get_value('node', Props),
    From = wh_json:get_value(<<"From">>, JObj),
    CIDNumber = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    To = wh_json:get_value(<<"To">>, JObj),
    Body = wh_json:get_value(<<"Body">>, JObj),
    Contact = wh_json:get_value(<<"Contact">>, JObj),
    IP = wh_json:get_value(<<"Contact-IP">>, JObj),
    Port = wh_json:get_value(<<"Contact-Port">>, JObj),
    Username = wh_json:get_value(<<"Contact-Username">>, JObj),
    [ContactUser, ContactRealm] = binary:split(Contact , <<"@">>),
    lager:info("Contact Parts ~p / ~p",[ContactUser, ContactRealm]),
    [FromUser, FromRealm] = binary:split(From , <<"@">>),
    lager:info("From Parts ~p / ~p",[FromUser, FromRealm]),
    [ToUser, ToRealm] = binary:split(To , <<"@">>),
    lager:info("To Parts ~p / ~p",[ToUser, ToRealm]),
    Target = get_target(To, Username),
    CallerID = get_caller_id(CIDNumber, From),
    CallerIDFull = <<"<sip:", CallerID/binary, ">">>,
    
    [User, Realm] = binary:split(To, <<"@">>),
            Header2 = [
               {"sip_profile", ?DEFAULT_FS_PROFILE}
              ,{"proto", "sip"}
              ,{"dest_proto", "sip"}
              ,{"to", wh_util:to_list(Target)}
              ,{"to_sip_ip", wh_util:to_list(IP)}
              ,{"to_sip_port", wh_util:to_list(Port)}
              ,{"from", wh_util:to_list(CallerID)}
              ,{"from_full", wh_util:to_list(CallerIDFull)}
              ,{"content-length", wh_util:to_list(size(Body))}
              ,{"type", "text/plain"}
              ,{"body", Body}
            ],
    lager:info("SENDING ~p",[Header2]),
%            Resp = freeswitch:sendevent(Node, 'SEND_MESSAGE', Header),
            Resp = freeswitch:sendevent_custom(Node, 'SMS::SEND_MESSAGE', Header2),
            lager:info("sent SIP/SIMPLE Msg to '~s' via ~s: ~p", [To, Node, Resp]).

%   local event = freeswitch.Event("CUSTOM", "SMS::SEND_MESSAGE");
%   event:addHeader("proto", "sip");
%%    event:addHeader("dest_proto", "sip");
%%    event:addHeader("from", "sip:1001@192.168.0.81");
%%    event:addHeader("from_full", "sip:1003@192.168.0.81");
%%    event:addHeader("to", "1004@192.168.0.81");
%%    event:addHeader("subject", "sip:1004@192.168.0.81");
%%    event:addHeader("type", "text/html");
%%    event:addHeader("hint", "the hint");
%%    event:addHeader("replying", "true");
%%    event:addBody("Hello from Seven Du! Have fun!");
   

-spec reply_affirmative(atom(), ne_binary(), wh_proplist()) -> 'ok'.
reply_affirmative(Node, FetchId, Props) ->
    XML1 = ["<document type='freeswitch/xml'><section name='chatplan' description='Chat Response'><context name='context_2'>
       <extension name='demo'>
         <condition field='to' expression='^321(.*)$'>
         <action application='set' data='to_sip_ip=192.168.0.21' />
         <action application='set' data='to_sip_port=5062' />
         <action application='set' data='to=IMSI001010000000001@mobile.rangenetworks.com' />
         <action application='set' data='from_full=sip:2002@mobile.rangenetworks.com' />
         <action application='send' />
         </condition>
       </extension>
       <extension name='demo2'>
         <condition field='to' expression='^(.*)$'>
         <action application='reply' data='Your Message to ${to_user} was not delivered: ${_body}'/>
         </condition>
       </extension>
     </context></section></document>"],
    XML = ["<document type='freeswitch/xml'><section name='chatplan' description='Chat Response'><context name='context_2'>
       <extension name='stop'>
         <condition field='to' expression='^(.*)$'>
           <action application='stop' data='stored'/>
         </condition>
       </extension>
     </context></section></document>"],
    
    case freeswitch:fetch_reply(Node, FetchId, 'chatplan', iolist_to_binary(XML), 3000) of
        {'error', _Reason} -> lager:debug("node ~s rejected our route response: ~p", [Node, _Reason]);
        'ok' ->
            lager:info("node ~s accepted route response for request ~s", [Node, FetchId])
    end.

-spec process_route_req(atom(), ne_binary(), wh_proplist()) -> 'ok'.
process_route_req(Node, FetchId, Props) ->
    put('callid', FetchId),
    props:to_log(Props,<<"CHAT PROPS">>),
    CallId = props:get_value(<<"Core-UUID">>, Props),
    reply_affirmative(Node, FetchId, Props),
    Props1 = ecallmgr_fs_trusted:pre_process_routes(Node, FetchId, CallId , Props),
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                       ,message(FetchId, Props1, Node)
                       ,fun wapi_sms:publish_message/1).


-spec message(ne_binary(), wh_proplist(), atom()) -> wh_proplist().
message(FetchId, Props, Node) ->
    %props:to_log(Props, <<"SMS REQ">>),
    props:filter_undefined([{<<"Msg-ID">>, FetchId}
     ,{<<"Caller-ID-Name">>, props:get_first_defined([<<"variable_effective_caller_id_name">>
                                                      ,<<"Caller-Caller-ID-Name">>
                                                      ,<<"from_user">>
                                                     ], Props, <<"Unknown">>)}
     ,{<<"Caller-ID-Number">>, props:get_first_defined([<<"variable_effective_caller_id_number">>
                                                        ,<<"Caller-Caller-ID-Number">>
                                                       ,<<"from_user">>
                                                       ], Props, <<"0000000000">>)}
     ,{<<"From-Network-Addr">>, props:get_first_defined([<<"variable_sip_h_X-AUTH-IP">>
                                                         ,<<"variable_sip_received_ip">>
                                                        ], Props)}
     ,{<<"Access-Network-Info">>, props:get_value(<<"variable_sip_h_P-Access-Network-Info">>, Props)}
     ,{<<"Physical-Info">>, props:get_value(<<"variable_sip_h_P-Phy-Info">>, Props)}
     ,{<<"User-Agent">>, props:get_value(<<"variable_sip_user_agent">>, Props)}    
     ,{<<"Call-ID">>,  props:get_value(<<"Core-UUID">>, Props)}
     ,{<<"To">>,  props:get_value(<<"to">>, Props)}
     ,{<<"From">>,  props:get_value(<<"from">>, Props)}
     ,{<<"Request">>, props:get_value(<<"to">>, Props)}
     ,{<<"Body">>, props:get_value(<<"body">>, Props)}
     ,{<<"SIP-Request-Host">>, props:get_value(<<"variable_sip_req_host">>, Props)}
     ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
     ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(message_ccvs(FetchId, Props))}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ]).

-spec message_ccvs(ne_binary(), wh_proplist()) -> wh_proplist().
message_ccvs(FetchId, Props) ->
    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
       | ecallmgr_util:custom_channel_vars(Props)
      ]
     ).
