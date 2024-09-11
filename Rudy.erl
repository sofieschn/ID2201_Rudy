-module(rudy).
-export([init/1, handler/1, request/1, reply/1, start/1, stop/0]).


% Start the server on a specified port
start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

% Stop the server by killing the registered process
stop() ->
    exit(whereis(rudy), "time to die"). 

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
            handler(Listen); % Recursively calls itself to listen for new connections
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

% Old reply function below 
% Generate a response based on the request
% reply({{get, URI, _Version}, _Headers, _Body}) ->
    % http:ok("You requested " ++ URI);  % Simple response with the requested URI
% reply(_) ->
    % http:ok("Unknown request").  % Fallback for other types of requests

% New reply function below 
% reply({{get, URI, _Version}, _Headers, _Body}) ->
    % timer:sleep(40),  % Simulate a 40ms delay 
    % http:ok("You requested " ++ URI);
reply({{get, "/sofie", _Version}, _Headers, _Body}) -> % testing testing
    timer:sleep(40),  % Simulate a 40ms delay 
    http:ok("Hello, Sofie! This is your custom response.");
reply(_) ->
    timer:sleep(40),  % Add delay even for unknown requests
    http:ok("Unknown request").

