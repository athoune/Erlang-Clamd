-module(clamd_protocol).

-export([start_stream/1, chunk_stream/2, end_stream/1, message/1, ask/2, scan/2]).


start_stream(Socket) ->
    gen_tcp:send(Socket, message("INSTREAM")).

chunk_stream(Socket, Chunk) ->
    Size = length(Chunk),
    gen_tcp:send(Socket, <<Size:32/big>>),
    gen_tcp:send(Socket, Chunk),
    ok.

end_stream(Socket) ->
    gen_tcp:send(Socket,[0,0,0,0]),
    R = case response(Socket) of
        {ok, "OK"} -> {ok, no_virus};
        {ok,"stream: " ++ Name} -> {ok, virus, Name};
        {error, Reason} -> {error, Reason}
    end,
    R.

scan(Socket, Path) ->
    case ask(Socket, "SCAN " ++ Path) of
        {ok, Blob} ->
            T = string:tokens(Blob, " "),
            case lists:nth(length(T), T) of
                "OK" -> {ok, no_virus};
                "FOUND" -> {ok, virus, lists:nth(length(T) -1, T), lists:nth(1, T)};
                _ -> {error, Blob}
        end;
        {error, Reason} -> {error, Reason}
end.

% http://www.clamav.net/doc/latest/html/node28.html

message(Action) ->
    "z" ++ Action ++ [0].

% Ask something to clamd and retuen response
ask(Socket, Action) ->
    gen_tcp:send(Socket, message(Action)),
    response(Socket).

response(Socket) ->
    response(Socket, []).
response(Socket, Acc) ->
    case gen_tcp:recv(Socket, 1) of
        {ok, Packet} ->
            case Packet of
                [0] -> {ok, Acc};
                _ -> response(Socket, Acc ++ Packet)
            end;
        {error, Reason} -> {error, Reason}
    end.

