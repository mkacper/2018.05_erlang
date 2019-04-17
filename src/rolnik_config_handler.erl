-module(rolnik_config_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    {Upper, Lower} = maybe_get_limits(Method, HasBody, Req0),
    ok = rolnik_alarmer:set_limits(Upper, Lower),
    {ok, Req0, Opts}.

maybe_get_limits(<<"POST">>, true, Req0) ->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    {Upper, _} = string:to_integer(proplists:get_value(<<"upper_limit">>, PostVals)),
    {Lower, _} = string:to_integer(proplists:get_value(<<"lower_limit">>, PostVals)),
    {Upper, Lower}.
