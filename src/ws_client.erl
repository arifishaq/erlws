-module(ws_client).
%% -compile(export_all).

-export([connect/1, 
	 send_text/2, send_binary/2,
	 send_first_text_fragment/2, send_first_binary_fragment/2,
	 send_text_fragment/2, send_binary_fragment/2,
	 send_last_text_fragment/2, send_last_binary_fragment/2,
	 recv/1, recv/2,
	 close/1, close/2, close/3, ping/1, ping/2]).

-include("websocket.hrl").

%% make sure crypto app is started!

connect(URI) ->
    {ok, {http, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(URI),

    {ok, Socket} =  gen_tcp:connect(Host, Port, [binary, {packet, 0}]),

    SecKey = base64:encode(masking_key()),

    Request = io_lib:format("GET ~s HTTP/1.1\r\nHost: ~s:~p\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: ~s\r\nSec-WebSocket-Version: 13\r\n\r\n", [Path, Host, Port, SecKey]),

    ok = gen_tcp:send(Socket, Request),

    %% io:format("websocket request sent with key ~p~n", [SecKey]),
    %% wait for an answer

    receive
	{tcp, Socket, Data} ->
	    %% io:format("got the answer ~p~n", [Data]),

	    {ok,{http_response,{1,1},101,"Switching Protocols"}, Rest} = 
		erlang:decode_packet(http, Data, []),
	    %% io:format("the answer was an HTTP response~n"),
	
	    ExpectedKey = binary_to_list(base64:encode(crypto:sha([SecKey, ?MAGIC_KEY]))),
	    %% io:format("Expecting security key ~p~n", [ExpectedKey]),

	    Headers = http_headers(Rest, []),
	    %% io:format("the headers were ~p~n", [Headers]),
	
	    ExpectedKey = proplists:get_value("sec-websocket-accept", Headers),
	    %% io:format("there was an Accept key~n"),
	    
	    {ok, Socket};

	{tcp_closed, Socket} -> 
	    {error, socket_closed};

	{tcp_error, Socket, Reason} ->
	    {error, Reason}
    end.



http_headers(Bin, HeaderList) ->
    case erlang:decode_packet(httph, Bin, []) of
	{ok,{http_header, _N, HeaderName, _ , HeaderValue}, Rest} when is_atom(HeaderName) ->
	    http_headers(Rest, [{string:to_lower(atom_to_list(HeaderName)), HeaderValue} | HeaderList]);
	{ok,{http_header, _N, HeaderName, _ , HeaderValue}, Rest} when is_list(HeaderName) ->
	    http_headers(Rest, [{string:to_lower(HeaderName), HeaderValue} | HeaderList]);
	{ok,http_eoh, <<>>} ->
	    HeaderList
    end.


parse_frame(<<?FRAGMENT:1, 0:1, 0:1, 0:1, Opcode:4, Rest/binary>>) ->
    parse_frame(fragment, frame_type(Opcode), Rest);
parse_frame(<<?FRAME:1, 0:1, 0:1, 0:1, Opcode:4, Rest/binary>>) ->
    parse_frame(frame, frame_type(Opcode), Rest).

parse_frame (Fragmentation, Opcode, <<?PLAIN:1, 126:7, PayloadLen:16/integer, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, Payload};
parse_frame (Fragmentation, Opcode, <<?MASKED:1, 126:7, PayloadLen:16/integer, MaskingKey:4/binary, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, mask(Payload, binary:bin_to_list(MaskingKey))};

parse_frame (Fragmentation, Opcode, <<?PLAIN:1, 127:7, PayloadLen:64/integer, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, Payload};
parse_frame (Fragmentation, Opcode, <<?MASKED:1, 127:7, PayloadLen:64/integer, MaskingKey:4/binary, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, mask(Payload, binary:bin_to_list(MaskingKey))};

parse_frame (Fragmentation, Opcode, <<?PLAIN:1, PayloadLen:7, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, Payload};
parse_frame (Fragmentation, Opcode, <<?MASKED:1, PayloadLen:7, MaskingKey:4/binary, Payload:PayloadLen/binary>>) ->
    {Fragmentation, Opcode, mask(Payload, binary:bin_to_list(MaskingKey))}.


frame_type(?CONTINUATION_FRAME) ->
    continuation;
frame_type(?TEXT_FRAME) ->
    text;
frame_type(?BINARY_FRAME) ->
    binary;
frame_type(?CLOSE_FRAME) ->
    close;
frame_type(?PING_FRAME) ->
    ping;
frame_type(?PONG_FRAME) ->
    pong;
frame_type(_R) ->
    %% io:format("unknown frame type ~p~n", [_R]),
    reserved.


masking_key() ->
    integer_to_list(random:uniform(16#FFFFFFFF),16).


mask(Payload, MaskingKey) ->
    %% io:format("masking with key ~p~n", [MaskingKey]),
    {MaskedPayload, _} = 
	lists:mapfoldl(
	  fun(Unmasked, Index) ->
		  {Unmasked bxor lists:nth((Index rem 4) + 1, MaskingKey), Index + 1}
	  end, 0, binary:bin_to_list(Payload)),
    binary:list_to_bin(MaskedPayload).


plain_application_data(FrameType, Bin) ->
    plain_application_data(?FRAME, FrameType, Bin).

plain_application_data(Fragmentation, FrameType, Bin) ->
    PayloadLen = byte_size(Bin),
    if
	PayloadLen =< 125 ->
	    <<Fragmentation:1, 0:1, 0:1, 0:1, FrameType:4, 0:1, PayloadLen:7/integer, Bin/binary>>;
	PayloadLen =< 16#FFFF ->
	    <<Fragmentation:1, 0:1, 0:1, 0:1, FrameType:4, 0:1, 126:7, PayloadLen:16/integer, Bin/binary>>;
	true ->
	    <<Fragmentation:1, 0:1, 0:1, 0:1, FrameType:4, 0:1, 127:7, PayloadLen:64/integer, Bin/binary>>
    end.

       
masked_application_data(FrameType, Bin) ->
    masked_application_data(?FRAME, FrameType, Bin).

masked_application_data(Fragmentation, FrameType, Bin) ->
    PayloadLen = byte_size(Bin),
    MK = masking_key(),
    Payload = mask(Bin, MK),
    MaskingKey = list_to_binary(MK),
    if
	PayloadLen =< 125 ->
	    <<Fragmentation:1, 0:1, 0:1, 0:1, FrameType:4, 1:1, PayloadLen:7/integer, MaskingKey:4/binary, Payload/binary>>;
	PayloadLen =< 16#FFFF ->
	    <<Fragmentation:1, 0:1, 0:1, 0:1, FrameType:4, 1:1, 126:7, PayloadLen:16/integer, MaskingKey:4/binary, Payload/binary>>;
	true ->
	    <<Fragmentation:1, 0:1, 0:1, 0:1, FrameType:4, 1:1, 127:7, PayloadLen:64/integer, MaskingKey:4/binary, Payload/binary>>
    end.
	

recv(Websocket, MaxWait) ->
    receive
	{tcp, Websocket, Data} ->
	    parse_frame(Data);
	M ->
	    {unexpected, M}
    after MaxWait ->
	    timeout
end.

recv(Websocket) ->
    recv(Websocket, 5000).


send_text(Websocket, Text) ->
    gen_tcp:send(Websocket, masked_application_data(?TEXT_FRAME, binary:list_to_bin(Text))).
send_binary(Websocket, Bin) ->
    gen_tcp:send(Websocket, masked_application_data(?BINARY_FRAME, Bin)).

send_first_text_fragment(Websocket, Text) ->
    gen_tcp:send(Websocket, masked_application_data(?FRAGMENT, ?TEXT_FRAME, binary:list_to_bin(Text))).
send_first_binary_fragment(Websocket, Bin) ->
    gen_tcp:send(Websocket, masked_application_data(?FRAGMENT, ?BINARY_FRAME, Bin)).

send_text_fragment(Websocket, Text) ->
    gen_tcp:send(Websocket, masked_application_data(?FRAGMENT, ?CONTINUATION_FRAME, binary:list_to_bin(Text))).
send_binary_fragment(Websocket, Bin) ->
    gen_tcp:send(Websocket, masked_application_data(?FRAGMENT, ?CONTINUATION_FRAME, Bin)).

send_last_text_fragment(Websocket, Text) ->
    gen_tcp:send(Websocket, masked_application_data(?CONTINUATION_FRAME, binary:list_to_bin(Text))).
send_last_binary_fragment(Websocket, Bin) ->
    gen_tcp:send(Websocket, masked_application_data(?CONTINUATION_FRAME, Bin)).


close(Websocket, Code, Reason) ->
    ClosingReason = list_to_binary(Reason),
    Payload = <<Code:16/integer, ClosingReason/binary>>,
    gen_tcp:send(Websocket, masked_application_data(?CLOSE_FRAME, Payload)),
    FrameType = frame_type(?CLOSE_FRAME),
    case recv(Websocket) of
	{frame, FrameType, <<ServerCode:16, OptionalReason/binary>>} ->
	    gen_tcp:close(Websocket),
	    {normal, ServerCode, OptionalReason};
	M -> 
	    gen_tcp:close(Websocket),
	    {abnormal, M}
    end.

close(Websocket, Code) ->
    close(Websocket, Code, "").
close(Websocket) ->
    close(Websocket, ?CLOSE_NORMAL).


ping(Websocket, Message, MaxWait) ->    
    Payload = list_to_binary(Message),
    gen_tcp:send(Websocket, masked_application_data(?PING_FRAME, Payload)),    
    %% wait for pong
    receive
	{tcp, Websocket, Data} ->
	    FrameType = frame_type(?PONG_FRAME),
	    case parse_frame(Data) of
		{frame, FrameType, Payload} ->
		    {pong, Message};
		M -> 
		    {pang, M}
	    end;
	E ->
	    {pang, E}
    after MaxWait ->
	pang		
    end.
    
ping(Websocket, Message) ->
    ping(Websocket, Message, 5000).

ping(Websocket) ->
    ping(Websocket, "").


