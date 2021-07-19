%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2021, Big Data Technology. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @doc
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
