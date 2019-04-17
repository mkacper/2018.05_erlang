-module(rolnik_alarmer).
-behaviour(gen_server).

%% API
-export([start_link/1, report_temp/1, set_limits/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {current_sample_temps = [], current_temp_status = normal,
                upper_limit, lower_limit, checks_per_sample,
                status_return_limit}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

report_temp(Temp) ->
    gen_server:call(?MODULE, Temp).

set_limits(Upper, Lower) ->
    gen_server:call(?MODULE, {set_limits, Upper, Lower}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([LowerLimit, UpperLimit, ChecksPerSample, StatusReturnLimit]) ->
    {ok, #state{lower_limit = LowerLimit, upper_limit = UpperLimit,
                checks_per_sample = ChecksPerSample,
                status_return_limit = StatusReturnLimit}}.

handle_call({set_limits, Upper, Lower}, _From, State) ->
    {reply, ok, State#state{upper_limit = Upper, lower_limit = Lower}};
handle_call(Temp, _From, State = #state{current_temp_status = CurrStatus,
                                           current_sample_temps = Temps,
                                           checks_per_sample = ChecksPerSample})
  when length(Temps) == (ChecksPerSample - 1) ->
    UpdatedTemps = [Temp | Temps],
    NewTempStatus = new_temp_status(State, UpdatedTemps), 
    case make_decision(NewTempStatus, CurrStatus) of
        do_nothing -> ok;
        alarm -> send(NewTempStatus)
    end,
    case NewTempStatus of
        normal -> grisp_led:color(2, green);
        below -> grisp_led:color(2, blue);
        above -> grisp_led:color(2, red)
    end,
    NewState = State#state{current_sample_temps = [],
                           current_temp_status = NewTempStatus},
    {reply, ok, NewState};
handle_call(Temp, _From,  State = #state{current_sample_temps = Temps}) ->
    UpdatedTemps = [Temp | Temps],
    NewState = State#state{current_sample_temps = UpdatedTemps},
    {reply, ok, NewState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions

new_temp_status(#state{current_temp_status = CurrStatus,
                       lower_limit = LowerLimit, upper_limit = UpperLimit,
                       status_return_limit = StatusReturnLimit}, Temps) ->
    AvrgTemp = average(Temps),
    verify_average_temp(CurrStatus, AvrgTemp, LowerLimit, UpperLimit,
                        StatusReturnLimit).

average(List) ->
    lists:sum(List) / length(List).

verify_average_temp(normal, Temp, LowerLimit, _UpperLimit, _StatusReturnLimit)
  when Temp < LowerLimit ->
    below;
verify_average_temp(normal, Temp, _LowerLimit, UpperLimit, _StatusReturnLimit)
  when Temp > UpperLimit ->
    above;
verify_average_temp(below, Temp, LowerLimit, _UpperLimit, StatusReturnLimit)
  when Temp > (LowerLimit + StatusReturnLimit) ->
    normal;
verify_average_temp(above, Temp, _LowerLimit, UpperLimit, StatusReturnLimit) 
  when Temp < (UpperLimit - StatusReturnLimit) ->
    normal;
verify_average_temp(below, _Temp, _LowerLimit, _UpperLimit, _StatusReturnLimit) ->
    below;
verify_average_temp(above, _Temp, _LowerLimit, _UpperLimit, _StatusReturnLimit) ->
    above;
verify_average_temp(normal, _Temp, _LowerLimit, _UpperLimit, _StatusReturnLimit) ->
    normal.


make_decision(below, normal) ->
    alarm;
make_decision(above, normal) ->
    alarm;
make_decision(_NewStatus, _CurrStatus) ->
    do_nothing.

send(Status) ->
    ok.
