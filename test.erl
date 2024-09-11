-module(test).
-export([bench/2]).

bench(Host, Port) ->
    Start = erlang:system_time(micro_seconds), 
    run(100, Host, Port), % runs 100 requests
    Finish = erlang:system_time(micro_seconds),
    Finish - Start. % checks how many ms it takes to run the requests 

run(N, Host, Port) ->
    if
        N == 0 -> ok; 
        true ->
            request(Host, Port),
            run(N-1, Host, Port) % calls itself to run all requests until N-1=0 
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt), % est TCP connection to the server at spec host and port 
   % gen_tcp:send(Server, http:get("foo")), % sends a http GET request for foo    
    gen_tcp:send(Server, http:get("sofie")), % sends a http GET request for sofie TESTING
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        {ok, _} -> ok; % nothing happens if the responce is recieved 
        {error, Error} ->
            io:format("test: error: ~w~n", [Error]) % if error occurs when response is/is not recieved, error is printed
    end,
    gen_tcp:close(Server). %close connection

