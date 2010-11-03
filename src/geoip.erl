% Returns the location of an IP address. Based on the ipinfodb.com web service.
% Copyright (c) 2010 Andrey Nesterov
% See MIT-LICENSE for licensing information.

-module(geoip).
-behaviour(gen_server).
-author('ae.nesterov@gmail.com').

-include("geoip.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
	start/1,
	stop/0,
	ip/0,
	geolocation/0,
	geolocation/1,
	geolocation/2
]).

-spec start(string()) -> {ok, pid()}.
start(Key) ->
	ok = inets:start(),
	{ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Key], []).

-spec stop() -> ok.
stop() ->
	ok = gen_server:call(?MODULE, stop),
	ok = inets:stop().

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
	gen_server:call(?MODULE, {lookup, Options});
geolocation(IP) ->
	geolocation(IP, #options {}).

-spec geolocation(inet:ip_address(), #options {}) -> #response {} | error.
geolocation(IP, Options) ->
	gen_server:call(?MODULE, {lookup, IP, Options}).

-spec request_url(string(), #options {}) -> string().
request_url(Key, #options {precision = P, timezone = T}) ->
	lists:concat([
		?SERVICE_URL, case P of
			city -> ?CITY_API;
			country -> ?COUNTRY_API
		end,
		?KEY_REQUIRED_API_PARAM, Key,
		?TIMEZONE_API_PARAM, T
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
		'Timezone' ->
			R;
		'TimezoneName' ->
			R#response{timezone = String(C)};
		'Gmtoffset' ->
			R#response{utc_offset = Integer(C)};
		'Dstoffset' ->
			R;
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

-spec init(list()) -> {ok, any()}.
init([Key]) ->
	{ok, #state {key = Key}}.

-spec handle_call(any(), any(), #state {}) -> {reply, any(), #state {}} | {noreply, #state {}}.
handle_call({lookup, Options}, _From, State) ->
	Reply = request(request_url(State#state.key, Options)),
	{reply, Reply, State};
handle_call({lookup, IP, Options}, _From, State) ->
	Reply = request(lists:concat([
		request_url(State#state.key, Options),
		?IP_API_PARAM, inet_parse:ntoa(IP)
	])),
	{reply, Reply, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

-spec handle_cast(any(), #state {}) -> {noreply, #state {}}.
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(any(), #state {}) -> {noreply, #state {}}.
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(any(), #state {}) -> any().
terminate(_Reason, _State) ->
	ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

