# erlws

Websocket functions in Erlang, mostly for a client

The functions here can help you write a websocket client in Erlang.

## Limitations

The only available protocol is http, so the URI must be 

	http://host:[port]/[path]

## API

The module is `ws_client`. Perhaps it should have been `erlws`?

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


## Usage

The test suite in the test directory uses the complete API. Briefly:

- Conenct to the websocket server at URI with `ws_client:connect(URI).` This will return `{ok, Websocket}`
- Send text messages over the websocket with `ws_client:send_text(Webscoket, String)`
- Send binaries over the websocket with `ws_client:send_binary(Websocket, Bin)`
- Ping the websocket, optionally specifying a timeout which defaults to 5 seconds, with `ws_client:ping(Websocket, String, Timeout)`. It should reply with a `{pong, String}` and the same string you sent
- If you expect to receive something from the websocket server, wait, optionally specifying the timeout which defaults to 5 seconds,  with `ws_client:recv(Websocket, Timeout)`. You should either get a frame: `{frame, Type, Data}` or a fragment: `{fragment, Type, Data}`. `Type` will be `text`, `binary`, `pong` or `close`
- Close the webscoket, optionally specifying a code and a reason, with `ws_client:close(Websocket, Code, Reason)`. The code defaults to 1000: normal closure.

## Warning

The test suite in the test directory is to be run with the Common Test framework. 

In this moment, the suite is hardcoded with a URI on the local machine. **PLEASE CHANGE IT TO SUIT YOUR NEEDS**. 

**TODO**: Put the URI in a config file or string.

This has been tested with **yaws**, using the callback module, `wscb`, in the `priv` directory.

