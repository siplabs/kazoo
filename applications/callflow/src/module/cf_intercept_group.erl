%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz Inc
%%% @doc
%%%
%%% Intercept earliest call from any group which caller belongs to
%%%
%%% @end
%%%
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_intercept_group).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(_, Call) ->
    AccountDb = whapps_call:account_db(Call),
    AuthId = whapps_call:authorizing_id(Call),
    AuthType = whapps_call:authorizing_type(Call),
    UserId = case AuthType of
                 <<"device">> ->
                     {'ok', JObj} = couch_mgr:open_cache_doc(AccountDb, AuthId),
                     wh_json:get_value(<<"owner_id">>, JObj);
                 <<"user">> -> AuthId
             end,
    Devices = cf_util:find_user_endpoints(UserId, [], Call),
    AllGroups = get_group_ids(Call, [UserId | Devices]),
    Endpoints = lists:flatten([cf_util:find_group_endpoints(GroupId, Call) || GroupId <- AllGroups]),
    Usernames = [get_sipname_by_id(Id, Call) || Id <- Endpoints],
    Channels = [Channel || Channel <-cf_util:find_channels(Usernames, Call),
                           wh_json:is_false(<<"answered">>, Channel)
                           andalso wh_util:is_not_empty(wh_json:get_value(<<"other_leg">>, Channel))
                           andalso wh_json:get_value(<<"authorizing_id">>, Channel) =/= AuthId
               ],
    ChannelsCount = lists:flatlength(Channels),
    case ChannelsCount > 0 of
        'true' ->
            Channel = get_earliest_channel(Channels),
            UUID = wh_json:get_value(<<"other_leg">>, Channel),
            lager:info("Pickup a ~s", [UUID]),
            {'ok', Ev} = whapps_call_command:b_pickup(UUID, Call),
            case whapps_util:get_event_type(Ev) of
                {<<"call_event">>, <<"CHANNEL_UNBRIDGE">>} -> whapps_call_command:wait_for_hangup();
                _ -> lager:info("Channel destroyed before pickup")
            end;
        'false' ->
            no_channels_ringing(Call),
            lager:info("Nothing to pickup")
    end,
    cf_exe:stop(Call).

-spec get_earliest_channel(wh_json:objects()) -> wh_json:object().
get_earliest_channel([Channel]) ->
    Channel;
get_earliest_channel([Channel | Channels]) ->
    lists:foldl(fun (Chan, Acc) ->
                    T1 = wh_json:get_value(<<"timestamp">>, Chan),
                    T2 = wh_json:get_value(<<"timestamp">>, Acc),
                    case T1 < T2 of
                        'true' -> Chan;
                        'false' -> Acc
                    end
    end, Channel, Channels).

-spec get_sipname_by_id(ne_binary(), whapps_call:call()) -> ne_binary().
get_sipname_by_id(EndpointId, Call) ->
    {'ok', JObj} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), EndpointId),
    wh_json:get_value([<<"sip">>, <<"username">>], JObj).

-spec no_channels_ringing(whapps_call:call()) -> any().
no_channels_ringing(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_prompt(<<"pickup_any_group-no_channels">>, Call).

-spec get_group_ids(whapps_call:call(), [ne_binary()])-> [ne_binary()].
get_group_ids(Call, MemberIds) ->
    [wh_json:get_value(<<"id">>, GroupJObj)|| GroupJObj <- cf_attributes:group_membership(Call, MemberIds)].