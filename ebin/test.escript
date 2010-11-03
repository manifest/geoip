#!/usr/bin/env escript

% Returns the location of an IP address. Based on the ipinfodb.com web service.
% Copyright (c) 2010 Andrey Nesterov
% See MIT-LICENSE for licensing information.

-include_lib("$PWD/../src/geoip.hrl").

main([Key | Params]) ->
	geoip:start(Key),
	io:format("~p~n", [run(Params)]),
	geoip:stop().

-spec run(list()) -> #response {}.
run([]) ->
	geoip:geolocation();
run([IP]) ->
	geoip:geolocation(ip(IP));
run([P, T]) ->
	geoip:geolocation(options(P, T));
run([IP, P, T]) ->
	geoip:geolocation(ip(IP), options(P, T)).

-spec ip(string()) -> inet:ip_address().
ip(String) ->
	{ok, IP} = inet_parse:address(String),
	IP.

-spec options(string(), string()) -> #options {}.
options(P, T) ->
	#options {precision=list_to_atom(P), timezone=list_to_atom(T)}.

