% @doc rolnik public API.
% @end
-module(rolnik).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    start_http_server(),
    rolnik_sup:start_link().

stop(_State) -> ok.

start_http_server() ->
    Dispatch = cowboy_router:compile([{'_', [{"/config",
                                              rolnik_config_handler, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8000}],
                                 #{env => #{dispatch => Dispatch}}).
