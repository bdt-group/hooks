%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(hooks).
-behaviour(gen_server).

%% External exports
-export([start_link/0]).
-export([add/4]).
-export([del/4]).
-export([run/2, run/3]).
-export([run_fold/3, run_fold/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {}).
-type state() :: #state{}.
-type hook() :: term().
-type callback() :: {Seq :: integer(), Module :: module(), Function :: atom()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(hook(), module(), atom(), integer()) -> ok.
add(Hook, Module, Function, Seq) ->
    gen_server:call(?MODULE, {add, Hook, Module, Function, Seq}).

-spec del(hook(), module(), atom(), integer()) -> ok.
del(Hook, Module, Function, Seq) ->
    gen_server:call(?MODULE, {del, Hook, Module, Function, Seq}).

-spec run(hook(), list()) -> ok.
run(Hook, Args) ->
    try ets:lookup_element(hooks, Hook, 2) of
        Ls ->
            run(Ls, Hook, Args)
    catch _:badarg ->
            ok
    end.

-spec run_fold(hook(), T, list()) -> T.
run_fold(Hook, Acc, Args) ->
    try ets:lookup_element(hooks, Hook, 2) of
        Ls ->
            run_fold(Ls, Hook, Acc, Args)
    catch _:badarg ->
            Acc
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    _ = ets:new(hooks, [named_table, public, {read_concurrency, true}]),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()} | {noreply, state()}.
handle_call({add, Hook, Module, Function, Seq}, _From, State) ->
    Reply = handle_add(Hook, {Seq, Module, Function}),
    {reply, Reply, State};
handle_call({del, Hook, Module, Function, Seq}, _From, State) ->
    Reply = handle_del(Hook, {Seq, Module, Function}),
    {reply, Reply, State};
handle_call(Request, {From, _}, State) ->
    ?LOG_WARNING("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_add(hook(), callback()) -> ok.
handle_add(Hook, El) ->
    case ets:lookup(hooks, Hook) of
        [{_, Ls}] ->
            case lists:member(El, Ls) of
                true ->
                    ok;
                false ->
                    NewLs = lists:merge(Ls, [El]),
                    ets:insert(hooks, {Hook, NewLs}),
                    ok
            end;
        [] ->
            NewLs = [El],
            ets:insert(hooks, {Hook, NewLs}),
            ok
    end.

-spec handle_del(hook(), callback()) -> ok.
handle_del(Hook, El) ->
    case ets:lookup(hooks, Hook) of
        [{_, Ls}] ->
            NewLs = lists:delete(El, Ls),
            ets:insert(hooks, {Hook, NewLs}),
            ok;
        [] ->
            ok
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?LOG_WARNING("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?LOG_WARNING("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
-spec run([callback()], hook(), list()) -> ok.
run([], _Hook, _Args) ->
    ok;
run([{_Seq, Module, Function} | Ls], Hook, Args) ->
    Res = safe_apply(Hook, Module, Function, Args),
    case Res of
        'EXIT' ->
            run(Ls, Hook, Args);
        stop ->
            ok;
        _ ->
            run(Ls, Hook, Args)
    end.

-spec run_fold([callback()], hook(), T, list()) -> T.
run_fold([], _Hook, Acc, _Args) ->
    Acc;
run_fold([{_Seq, Module, Function} | Ls], Hook, Acc, Args) ->
    Res = safe_apply(Hook, Module, Function, [Acc | Args]),
    case Res of
        'EXIT' ->
            run_fold(Ls, Hook, Acc, Args);
        stop ->
            Acc;
        {stop, NewAcc} ->
            NewAcc;
        NewAcc ->
            run_fold(Ls, Hook, NewAcc, Args)
    end.

-spec safe_apply(hook(), module(), atom(), list()) -> any().
safe_apply(Hook, Module, Function, Args) ->
    ?LOG_DEBUG("Running hook ~p: ~p:~p/~B",
               [Hook, Module, Function, length(Args)]),
    try apply(Module, Function, Args)
    catch E:R:St when E /= exit; R /= normal ->
            error_logger:error_msg(
              "Hook ~p crashed when running ~p:~p/~p:~n" ++
                  string:join(
                    ["** ~ts"|
                     ["** Arg " ++ integer_to_list(I) ++ " = ~p"
                      || I <- lists:seq(1, length(Args))]],
                    "~n"),
              [Hook, Module, Function, length(Args),
               format_exception(2, E, R, St)|Args]),
            'EXIT'
    end.

format_exception(Level, Class, Reason, Stacktrace) ->
    erl_error:format_exception(
      Level, Class, Reason, Stacktrace,
      fun(_M, _F, _A) -> false end,
      fun(Term, I) ->
              io_lib:print(Term, I, 80, -1)
      end).
