% Provides information about the geolocation. Based on the ipinfodb.com web service.
% Copyright (c) 2010 Andrey Nesterov
% See MIT-LICENSE for licensing information.

-module(geoip).
-author('ae.nesterov@gmail.com').

-include("geoip.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([
	start/0,
	ip/0,
	geolocation/0,
	geolocation/1,
	geolocation/2
]).

-spec start() -> ok.
start() ->
	ok = inets:start().

-spec ip() -> inet:address() | error.
ip() ->
	case geolocation() of
		#response {ip = IP} ->
			IP;
		error ->
			error
	end.

-spec geolocation() -> #response {} | error.
geolocation() ->
	geolocation(#options {}).

-spec geolocation(#options {} | inet:ip_address()) -> #response {} | error.
geolocation(Options) when is_record(Options, options) ->
	request(request_url(Options));
geolocation(IP) ->
	geolocation(IP, #options {}).

-spec geolocation(inet:ip_address(), #options {}) -> #response {} | error.
geolocation(IP, Options) ->
	request(lists:concat([
		request_url(Options),
		?IP_API_PARAM, inet_parse:ntoa(IP)
	])).

-spec request_url(#options {}) -> string().
request_url(#options {precision = P, timezone = T}) ->
	lists:concat([
		?SERVICE_URL, case P of
			city -> ?CITY_API;
			country -> ?COUNTRY_API
		end,
		?TIMEZONE_REQUIRED_API_PARAM, T
	]).

-spec request(string()) -> #response {} | error.	
request(Url) ->
	case httpc:request(get, {Url, []}, [], []) of
		{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body}} ->
			{E, _} = xmerl_scan:string(Body),
			extract(E, #response{});
		{ok, Response} ->
			error_logger:error_msg("~p: strange server response ~p~n", [?MODULE, Response]),
			error;
		{error, Reason} ->
			error_logger:error_msg("~p: request error ~p~n", [?MODULE, Reason]),
			error
	end.

-spec extract(xmerl:content(), #response {}) -> #response {}.
extract(#xmlElement{name='Response'} = E, R) ->
	lists:foldl(fun extract/2, R, E#xmlElement.content);
extract(#xmlElement{name=Name, content=[C]}, R) ->
	String = fun(X) -> X#xmlText.value end,
	IP = fun(X) -> {ok, IP} = inet_parse:address(String(X)), IP end,
	Integer = fun(X) -> list_to_integer(String(X)) end,
	Number = fun(X) ->
		Value = String(X),
		try list_to_float(Value)
		catch
			error:badarg -> 
				try list_to_integer(Value)
				catch
					error:badarg ->
						Value
				end
		end
	end,
	case Name of
		'Ip' ->
			R#response{ip = IP(C)};
		'Status' ->
			R#response{status = String(C)};
		'CountryCode' ->
			R#response{country_code = String(C)};
		'CountryName' ->
			R#response{country_name = String(C)};
		'RegionCode' ->
			R#response{region_code = Integer(C)};
		'RegionName' ->
			R#response{region_name = String(C)};
		'City' ->
			R#response{city = String(C)};
		'ZipPostalCode' ->
			R#response{zip_postal_code = Integer(C)};
		'Latitude' ->
			R#response{latitude = Number(C)};
		'Longitude' ->
			R#response{longitude = Number(C)};
		'TimezoneName' ->
			R#response{timezone = String(C)};
		'Gmtoffset' ->
			R#response{utc_offset = Integer(C)};
		'Isdst' ->
			R#response{dst = String(C) =:= "1"};
		true ->
			error_logger:error_msg("~p: unknown element ~n", [?MODULE]),
			R
	end;
extract(#xmlElement{content=[]}, R) ->
	R;
extract(#xmlText{}, R) ->
	R.

