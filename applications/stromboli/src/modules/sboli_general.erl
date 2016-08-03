%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2016 13:43
%%%-------------------------------------------------------------------
-module(sboli_general).
-author("Ivan Romanyuk").

%% API
-export([get_raw_data/2,
         gen_conf/2]).

-include("../stromboli.hrl").

-spec get_raw_data(cowboy_req:req(), stromboli:state()) ->
                    {cowboy_req:req(), stromboli:state()}.
get_raw_data(Req, State) ->
    Data = get_all_data(State#sboli_req.unit_json),
    Data1 = wh_json:encode(wh_json:get_value(<<"config">>, Data)),
    {Req, State#sboli_req{body = Data1}}.

-spec gen_conf(cowboy_req:req(), stromboli:state()) ->
                {cowboy_req:req(), stromboli:state()}
                | erlydtl:err_ret().
gen_conf(Req, State) ->
    AccDb = wh_util:format_account_db(State#sboli_req.account_id),
    {'ok', UnitJson} = couch_mgr:open_doc(AccDb, State#sboli_req.unit_id),
    Data = get_all_data(UnitJson),
    ReqConfFile = lists:last(stromboli:path(Req)),
    case ReqConfFile of
        <<_Mac:12/binary, "-site.cfg">> ->
            gen_conf_with_xmerl(Req, State, Data, ReqConfFile);
        _ ->
            gen_conf_with_dtl(Req, State, Data, ReqConfFile)
    end.

-spec gen_conf_with_dtl(cowboy_req:req(), stromboli:state(), wh_json:object(), binary()) ->
                            {cowboy_req:req(), stromboli:state()}
                            | erlydtl:err_ret().
gen_conf_with_dtl(Req, State, Data, ReqConfFile) ->
    {'ok', DtlFile} = sboli_polycom:try_get_dtl_file(ReqConfFile),
    Data1 = wh_json:recursive_to_proplist(Data),
    DtlFile1 = binary_to_list(iolist_to_binary([stromboli:priv_dir(), <<"/dtl/">>, DtlFile])),
    lager:debug([{trace, true}],"Dtl file is ~p", [DtlFile1]),
    case erlydtl:compile_file(DtlFile1, 'dtl_file', [{'out_dir', "./"}]) of
        {'ok', _ } ->  case dtl_file:render(Data1) of
                           {'ok', R} -> {Req, State#sboli_req{body = R}};
                           {'error', E} = Err ->
                               lager:warning([{trace, true}],"Can't render dtl file, error: ~p", [E]),
                               Err
                       end;
        {'error', E} = Err ->
            lager:warning([{trace, true}],"Can't compile dtl file, error: ~p", [E]),
            Err;
        _ = E ->
            lager:warning([{trace, true}],"Unexpected dtl file error error ~p: ", [E]),
            E
    end.

-spec gen_conf_with_xmerl(cowboy_req:req(), stromboli:state(), wh_json:object(), binary()) ->
                            {cowboy_req:req(), stromboli:state()}.
gen_conf_with_xmerl(Req, State, Data, ReqConfFile) ->
    Config = sboli_polycom:get_config_file(ReqConfFile, Data),
    {Req, State#sboli_req{body = Config}}.

-spec get_all_data(wh_json:object()) -> wh_json:objects().
get_all_data(UnitJson) ->
    AccId = sboli_unit:get_account_id(UnitJson),
    AccDb = wh_util:format_account_db(AccId),
    {'ok', AccountData} = couch_mgr:open_cache_doc(AccDb, AccId),
    {AccountConfig, _Locks} = sboli_account:get_account_config(AccId),

    Profiles = sboli_unit:get_profiles(UnitJson),
    ProfilesConfig = sboli_profiles:get_profiles_config(Profiles, AccId),
    UnitConfig = sboli_unit:get_config(UnitJson),
    lager:debug([{trace, true}],"Unit config is ~p", [UnitConfig]),
    ResultConfig = wh_json:merge_recursive([UnitConfig, ProfilesConfig, AccountConfig]),
    Lines = sboli_unit:get_lines(UnitJson),
    {[{<<"lines">>, Lines}, {<<"account">>, AccountData }
     ,{<<"mac_address">>, wh_json:get_value(<<"mac_address">>, UnitJson)}
     ,{<<"config">>, ResultConfig}
    ]}.
