% Provides information about the geolocation. Based on the ipinfodb.com web service.
% Copyright (c) 2010 Andrey Nesterov
% See MIT-LICENSE for licensing information.

-define(SERVICE_URL, "http://ipinfodb.com/").
-define(CITY_API, "ip_query.php").
-define(COUNTRY_API, "ip_query_country.php").
-define(TIMEZONE_REQUIRED_API_PARAM, "?timezone=").
-define(IP_API_PARAM, "&ip=").

-record(response, {
	ip :: inet:ip_address(),
	status :: string(),
	country_code :: string(),
	country_name :: string(),
	region_code :: integer,
	region_name :: string(),
	city :: string(),
	zip_postal_code :: integer,
	latitude :: float,
	longitude :: float,
	timezone :: string(),
	utc_offset :: integer,
	dst :: boolean()
}).

-record(options, {
	timezone = false :: boolean(),
	precision = city :: city | country
}).

