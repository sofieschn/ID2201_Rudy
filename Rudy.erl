-module(rudy).
-export([init/1, handler/1, request/1, reply/1]).

init(Port) ->
    Opt = [{active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error]),
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            gen_tcp:close(Client);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end.

