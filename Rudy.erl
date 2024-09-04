-module(rudy).
-export([init/1, handler/1, request/1, reply/1]).

% Initialize the server and start listening on a port
init(Port) ->
Opt = [{active, false}, {reuseaddr, true}], % allows the port to be reused immediately after prev connection
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            io:format("Server started on port ~p~n", [Port]),
            handler(Listen),  % Pass the socket to the handler function
            gen_tcp:close(Listen),  % Close the socket after the handler completes
            ok;
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error]),
            error
    end.

% Handle incoming client connections
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            io:format("Client connected~n"),
            request(Client),  % Handle the request
            gen_tcp:close(Client),  % Close the client socket after handling the request
            handler(Listen);  % Wait for the next connection
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end.

% Process the client request
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->  % If data is successfully received
            io:format("Received request: ~s~n", [Str]),  % Print the request
            case http:parse_request(Str) of  % Parse the HTTP request
                {{get, URI, Version}, Headers, Body} ->  % Capture the full values
                Response = reply({{get, URI, Version}, Headers, Body}),
                gen_tcp:send(Client, Response);  % Send the response back to the client
                {error, _} ->  % Handle parsing errors
                    io:format("rudy: error parsing request~n")
            end;
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).


% Generate a response based on the request
reply({{get, URI, _Version}, _Headers, _Body}) ->
    http:ok("You requested " ++ URI);  % Simple response with the requested URI
reply(_) ->
    http:ok("Unknown request").  % Fallback for other types of requests

