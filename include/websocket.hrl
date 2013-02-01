-define(MAGIC_KEY, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

-define(CONTINUATION_FRAME, 0).
-define(TEXT_FRAME, 1).
-define(BINARY_FRAME, 2).
-define(CLOSE_FRAME, 8).
-define(PING_FRAME, 9).
-define(PONG_FRAME, 10).

-define(FRAME, 1).
-define(FRAGMENT, 0).

-define(PLAIN, 0).
-define(MASKED, 1).

-define(CLOSE_NORMAL, 1000).
-define(CLOSE_GOING_AWAY, 1001).
