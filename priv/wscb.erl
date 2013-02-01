-module(wscb).
-compile(export_all).

handle_message({text, Data}) ->
    %% io:format("Got ~p~n", [Data]),
    {reply, {text, <<"text ok: ", Data/binary>>}};
handle_message({binary, Data}) ->
    %% io:format("Got ~p~n", [Data]),
    {reply, {binary, <<"binary ok: ",Data/binary>>}};
handle_message({close, _Code, _Reason}) ->
    %% io:format("Browser closed socket with code ~p and reason ~p~n", [Code, Reason]),
    {close, ""};
handle_message(_Msg) ->
    %% io:format("Unable to handle ~p~n", [Msg]),
    noreply.


