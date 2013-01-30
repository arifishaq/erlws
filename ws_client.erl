-module(ws_client).
-compile(export_all).

%% -record(state, {socket}).

%% make sure crypto app is started!

connect(URI) ->
    {ok, {http, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(URI),
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
	{ok, Socket} ->        
	    %% io:format("socket request accepted~n"),
	    %% SecKey = base64:encode(lists:flatten(io_lib:format("~16..0B", [random:uniform(16#FFFFFFFF)]))),
	    SecKey = base64:encode(integer_to_list(random:uniform(16#FFFFFFFF), 16)),
	    Request = io_lib:format("GET ~s HTTP/1.1\r\nHost: ~s:~p\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: ~s\r\nSec-WebSocket-Version: 13\r\n\r\n", [Path, Host, Port, SecKey]),
	    case gen_tcp:send(Socket, Request) of
		ok ->
		    %% io:format("websocket request sent with key ~p~n", [SecKey]),
		    %% wait for an answer
		    receive
			{tcp, Socket, Data} ->
			    %% io:format("got the answer ~p~n", [Data]),
			    case erlang:decode_packet(http, Data, []) of
				{ok,{http_response,{1,1},101,"Switching Protocols"}, Rest} ->
				    %% io:format("the answer was an HTTP response~n"),
				    Headers = http_headers(Rest, []),
				    %% io:format("the headers were ~p~n", [Headers]),
				    %% make sure the Sec-Accept header is there
				    %% -------------------------------------
				    %% THIS IS A BUG WORKAROUND!! The key should be Sec-WebSocket-Accept
				    %% -------------------------------------
				    case proplists:get_value("Sec-Websocket-Accept", Headers) of
					undefined -> 
					    %% io:format("there was no Accept key~n"),
					    {error, econnrefused};
					AcceptKey ->
					    %% io:format("there was an Accept key~n"),
					    Expected = binary_to_list(base64:encode(crypto:sha([SecKey, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"]))),
					    %% io:format("it was expected to be ~p~n", [Expected]),
					    case Expected of
						AcceptKey ->
						    {ok, Socket};
						_ ->
						    {error, econnrefused}
					    end
				    end;
				{error, Reason} -> {error, Reason};
				More -> More
			    end;
			{tcp_closed, Socket} -> 
			    {error, socket_closed};
			{tcp_error, Socket, Reason} ->
			    {error, Reason}
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.



http_headers(Bin, HeaderList) ->
    case erlang:decode_packet(httph, Bin, []) of
	{ok,{http_header, _N, HeaderName, _ , HeaderValue}, Rest} ->
	    http_headers(Rest, [{HeaderName, HeaderValue} | HeaderList]);
	{ok,http_eoh, <<>>} ->
	    HeaderList
    end.


parse_frame(<<0:1, 0:1, 0:1, 0:1, Opcode:4, Rest/binary>>) ->
    parse_frame(fragmented, frame_type(Opcode), Rest);
parse_frame(<<1:1, 0:1, 0:1, 0:1, Opcode:4, Rest/binary>>) ->
    parse_frame(final, frame_type(Opcode), Rest).

parse_frame (Fragmentation, Opcode, <<0:1, 126:7, PayloadLen:16/integer, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, Payload};
parse_frame (Fragmentation, Opcode, <<1:1, 126:7, PayloadLen:16/integer, MaskingKey:4/binary, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, mask(Payload, MaskingKey)};

parse_frame (Fragmentation, Opcode, <<0:1, 127:7, PayloadLen:64/integer, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, Payload};
parse_frame (Fragmentation, Opcode, <<1:1, 127:7, PayloadLen:64/integer, MaskingKey:4/binary, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, mask(Payload, MaskingKey)};

parse_frame (Fragmentation, Opcode, <<0:1, PayloadLen:7, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, Payload};
parse_frame (Fragmentation, Opcode, <<1:1, PayloadLen:7, MaskingKey:4/binary, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, mask(Payload, MaskingKey)}.


frame_type(0) ->
    continuation;
frame_type(1) ->
    text;
frame_type(2) ->
    binary;
frame_type(8) ->
    close;
frame_type(9) ->
    ping;
frame_type(10) ->
    pong;
frame_type(_R) ->
    %% io:format("unknown frame type ~p~n", [_R]),
    reserved.


mask(Payload, MaskingKey) ->
    %% io:format("masking with key ~p~n", [MaskingKey]),
    MK = binary_to_list(MaskingKey),
    {MaskedPayload, _} = 
	lists:mapfoldl(
	  fun(Unmasked, Index) ->
		  {Unmasked bxor lists:nth((Index rem 4) + 1, MK), Index+1}
	  end, 0, binary_to_list(Payload)),
    MaskedPayload.


make_unmasked_fragment(Type, Payload) ->
    PayloadLen = byte_size(Payload),
    <<0:1, 0:1, 0:1, 0:1, Type:4, 0:1, PayloadLen:7/integer, Payload/binary>>.
    
make_unmasked_frame(Type, Payload) ->
    PayloadLen = byte_size(Payload),
    <<1:1, 0:1, 0:1, 0:1, Type:4, 0:1, PayloadLen:7/integer, Payload/binary>>.

make_unmasked_binary_frame(Payload) ->
    make_unmasked_frame(2, Payload).
make_unmasked_text_frame(Payload) ->
    make_unmasked_frame(1, list_to_binary(Payload)).
   
make_close_frame(Payload) ->
    make_unmasked_frame(8, Payload).


make_masked_fragment(Type, Payload) ->
    PayloadLen = byte_size(Payload),
    MaskingKey = list_to_binary(integer_to_list(random:uniform(16#FFFFFFFF),16)),
    Bin = list_to_binary(mask(Payload, MaskingKey)),
    <<0:1, 0:1, 0:1, 0:1, Type:4, 1:1, PayloadLen:7/integer, MaskingKey:4/binary, Bin/binary>>.

make_masked_frame(Type, Payload) ->
    PayloadLen = byte_size(Payload),
    MaskingKey = list_to_binary(integer_to_list(random:uniform(16#FFFFFFFF),16)),
    Bin = list_to_binary(mask(Payload, MaskingKey)),
    <<1:1, 0:1, 0:1, 0:1, Type:4, 1:1, PayloadLen:7/integer, MaskingKey:4/binary, Bin/binary>>.

make_masked_binary_frame(Payload) ->
    make_masked_frame(2, Payload).
make_masked_text_frame(Payload) ->
    make_masked_frame(1, list_to_binary(Payload)).
    

payload_in_frame(Frame) ->
    <<1:1, 0:1, 0:1, 0:1, _Type:4, 0:1, _PayloadLen:7/integer, Payload/binary>> = Frame,
    Payload.

payload_in_masked_frame(Frame) ->
    <<1:1, 0:1, 0:1, 0:1, _Type:4, 1:1, _PayloadLen:7/integer, MaskingKey:4/binary, Payload/binary>> = Frame,
    mask(Payload, MaskingKey).

