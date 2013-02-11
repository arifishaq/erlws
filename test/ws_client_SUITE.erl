%%%-------------------------------------------------------------------
%%% @author  Arif Ishaq
%%% @copyright (C) 2013 Arif Ishaq
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ws_client_SUITE).

-compile(export_all).

-include("websocket.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = ct:require(url),
    [{pause, ct:get_config(pause, 1)}, {url, ct:get_config(url)} | Config]. 
%% there is some problem with the fragmented delivery tests.
%% if you don't pause at least one millisecond between each fragment,
%% it doesn't work

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(open_close, Config) ->
    Config;
init_per_testcase(_, Config) ->
    {ok, Websocket} = ws_client:connect(?config(url, Config)),    
    [{websocket, Websocket} | Config].


%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(open_close, _Config) ->
    ok;
end_per_testcase(_, Config) ->
    Websocket = ?config(websocket, Config),
    {normal, _Code, _OptionalReason} = ws_client:close(Websocket, ?CLOSE_NORMAL),    
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [open_close, ping, text_delivery, bin_delivery,
     fragmented_text_delivery, fragmented_bin_delivery].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
%%text_delivery() -> 
%%    [{text, "Hello there!"}].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
open_close(Config) -> 
    {ok, Websocket} = ws_client:connect(?config(url, Config)),
    {normal, _Code, _OptionalReason} = ws_client:close(Websocket, ?CLOSE_NORMAL),
    ok.

text_delivery(Config) ->
    Websocket = ?config(websocket,Config),
    ok = ws_client:send_text(Websocket, "Text delivery"),
    {frame, text, <<"text ok: ", "Text delivery">>} = ws_client:recv(Websocket),
    ok.

bin_delivery(Config) ->
    Websocket = ?config(websocket,Config),
    ok = ws_client:send_binary(Websocket, <<"Bin delivery">>),
    {frame, binary, <<"binary ok: ", "Bin delivery">>} = ws_client:recv(Websocket),
    ok.

fragmented_text_delivery(Config) ->
    Websocket = ?config(websocket,Config),
    ok = ws_client:send_first_text_fragment(Websocket, "Fragm"),
    timer:sleep(?config(pause, Config)),
    ok = ws_client:send_text_fragment(Websocket, "ented text d"),
    timer:sleep(?config(pause, Config)),
    ok = ws_client:send_text_fragment(Websocket, "eli"),
    timer:sleep(?config(pause, Config)),
    ok = ws_client:send_last_text_fragment(Websocket, "very!"),
    timer:sleep(?config(pause, Config)),
    {frame, text, <<"text ok: ", "Fragmented text delivery!">>} = ws_client:recv(Websocket),
    ok.

fragmented_bin_delivery(Config) ->
    Websocket = ?config(websocket,Config),
    ok = ws_client:send_first_binary_fragment(Websocket, <<"Fragmen">>),
    timer:sleep(?config(pause, Config)),
    ok = ws_client:send_binary_fragment(Websocket, <<"ted bi">>),
    timer:sleep(?config(pause, Config)),
    ok = ws_client:send_binary_fragment(Websocket, <<"n deliv">>),
    timer:sleep(?config(pause, Config)),
    ok = ws_client:send_last_binary_fragment(Websocket, <<"ery!">>),
    timer:sleep(?config(pause, Config)),
    {frame, binary, <<"binary ok: ", "Fragmented bin delivery!">>} = ws_client:recv(Websocket),
    ok.

ping(Config) ->
    Websocket = ?config(websocket,Config),
    {pong, "Alive"} = ws_client:ping(Websocket, "Alive"),
    ok.

