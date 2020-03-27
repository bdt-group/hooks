%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(hooks_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, pid()} |
          {error, {already_started, pid()}} |
          {error, {shutdown, term()}} |
          {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_one,
            intensity => 10,
            period => 1},
          [#{id => hooks,
             start => {hooks, start_link, []},
             restart => permanent,
             shutdown => timer:seconds(5),
             type => worker,
             modules => [hooks]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
