%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(hooks_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start_phase/3, stop/1, prep_stop/1,
         config_change/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
          {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    hooks_sup:start_link().

-spec start_phase(atom(), normal | {takeover, node()} | {failover, node()}, term()) -> ok.
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

-spec stop(term()) -> any().
stop(_State) ->
    ok.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    State.

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
