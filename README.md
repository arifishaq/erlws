<<<<<<< HEAD
erlws
=====

Websocket functions in Erlang, mostly for a client

The functions here help you write a websocket client in Erlang.

Limitations
-----------

The only available protocol is http, so the URI must be 

...
http://host:[port]/[path]
...

API
---

...
connect(URI) -> {ok, Websocket}
%% e.g. connect("http://localhost:8081/websock.yaws").

close(Websocket) -> {normal, Code, Reason} | {abnormal, Error} %% code 1000
close(Websocket, Code) -> {normal, Code, Reason} | {abnormal, Error}
close(Websocket, Code, Reason) -> {normal, Code, Reason} | {abnormal, Error}

ping(Websocket) -> {pong, <<>>} | {pang, Error}
ping(Websocket, Message) -> {pong, Message} | {pang, Error}

send_text(Websocket, Text) -> ok

send_binary(Websocket, Binary) -> ok

send_first_text_fragment(Websocket, TextFragment) -> ok
send_text_fragment(Websocket, TextFragment) -> ok
send_last_text_fragment(Websocket, TextFragment) -> ok

send_first_binary_fragment(Websocket, TextFragment) -> ok
send_binary_fragment(Websocket, TextFragment) -> ok
send_last_binary_fragment(Websocket, TextFragment) -> ok

recv(Websocket) -> 
		{frame, text, TextAsBinary} | 
		{fragment, text, TextAsBinary} |
		{frame, binary, Binary} | 
		{fragment, binary, Binary} |
		{frame, pong, Text} | 
		{frame, close, Binary}
...

Warning
-------

The test suite in the test directory is to be run with the Common Test framework. 

In this moment, the suite is hardcoded with a URI on the local machine. PLEASE CHANGE IT TO SUIT YOUR NEEDS. 

TODO: Put the URI in a config file or string.

This has been tested with yaws, using the callback module in the priv directory.


		
=======
erlws
=====

Websocket functions in Erlang, mostly for a client
>>>>>>> e93a137e9bdbbd501de4d20a1af8a3bc1fd586ea
