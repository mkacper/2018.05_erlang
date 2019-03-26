% @doc rolnik top level supervisor.
% @end
-module(rolnik_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
%   {ok, TempCheckFreq} = application:get_env(rolnik, temp_check_freq),
    SensorId = get_sensor_id(),
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
                  %% start rolnik alarmer
                  #{id => rolnik_alarmer_id,
                    start => {rolnik_alarmer, start_link, [[15, 28, 10, 0.5]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [rolnik_alarmer]},
                  %% start rolnik_temp_checker
                  #{id => rolnik_temp_checker_id,
                    start => {rolnik_temp_checker, start_link, [[1000, SensorId]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [rolnik_temp_checker]}
                 ],
    {ok, {SupFlags, ChildSpecs}}.
%% Internal functions

get_sensor_id() ->
    [ID] = grisp_onewire:transaction(fun() -> grisp_onewire:search() end),
    ID.
