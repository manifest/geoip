#!/usr/bin/env escript

% Provides information about the geolocation. Based on the ipinfodb.com web service.
% Copyright (c) 2010 Andrey Nesterov
% See MIT-LICENSE for licensing information.

-include_lib("$PWD/../src/geoip.hrl").

main(Params) ->
	geoip:start(),
	io:format("~p~n", [run(Params)]).

-spec run(list()) -> #response {}.
run([]) ->
	geoip:geolocation();
run([IP]) ->
	geoip:geolocation(ip(IP));
run([P, T]) ->
	geoip:geolocation(#options {precision=P, timezone=T});
run([IP, P, T]) ->
	geoip:geolocation(IP, #options {precision=P, timezone=T}).

-spec ip(string()) -> inet:ip_address().
ip(String) ->
	{ok, IP} = inet_parse:address(String),
	IP.

