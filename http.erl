-module(http).
-export([parse_request/1]).

%Main function for parsing the request
parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

% Function for parsing the line in which the request is made
request_line([$G, $E, $T, 32 |R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,
    {{get, URI, Ver}, R3}. % returns the tuple containing 1st the parsed request line and 2nd the remainders of the string

% function parses the URI 
request_uri([32|R0])-> % checks if the URI is a space, ie the URI has ended
    {[], R0}; 
request_uri([C|R0]) -> % when URI is not space/32
{Rest, R1} = request_uri(R0), % function calls itself recursively until it has parsed the entire string, adding o each new int to the string until it reaches the space/32 at the end of the URI input
    {[C|Rest], R1}. % returns the parsed URI as a string

% function matches the HTTP version, to either 1.0 or 1.1 and presents it as atoms v11 or v10. It only handles these 2 verisons atm.
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0}; % returns v11 if the final $1 in the pattern matches 
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}. % returns atom v10 if the final $0 in the pattern matches 

% function processes an entire sequence of headers
headers([13,10|R0]) -> % checks if the input starts with r\n\/[13,10] which means there is no new header  
    {[],R0}; % returns an empty list and the rest of the input (R0)
headers(R0) -> % if it does not match 13,10, the parsing will continue 
    {Header, R1} = header(R0), % calls the header function to parse each individual header in the sequence of headers
    {Rest, R2} = headers(R1), % recursively calls itself to parse each sequence of headers
    {[Header|Rest], R2}. % returns the list of headers with the remaining input
    
header([13,10|R0]) -> % checks if the header start with 13,10
    {[], R0}; % if so, returns empty list and remainders of the input
header([C|R0]) -> % if it does not start with 13,10 
    {Rest, R1} = header(R0), % function calls itself until all of header has been processed
    {[C|Rest], R1}. % returns completed header

  

