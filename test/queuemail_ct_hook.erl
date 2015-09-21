-module(queuemail_ct_hook).

-export([init/2, terminate/1]).
-export([on_tc_fail/3, on_tc_skip/3]).

-record(state, {}).

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    #state{}.

%% @doc Called when the scope of the CTH is done
terminate(#state{}) ->
    ok.

format_test_name({TestName, TestGroup}) ->
    io_lib:format("[~p] ~p", [TestGroup, TestName]);
format_test_name(TestName) ->
    io_lib:format("~p", [TestName]).

on_tc_fail(TestName, {_, {Reason, StackTrace}}, CtState) ->
    ct:pal("Failed test ~s!~n~p~n~n~p~n----------------------------------------------------",
            [format_test_name(TestName), Reason, StackTrace]),
    CtState.

on_tc_skip(TestName, {_, {Reason, StackTrace}}, CtState) ->
    ct:pal("Skipped test ~s!~n~p~n~n~p~n----------------------------------------------------",
    [format_test_name(TestName), Reason, StackTrace]),
    CtState.

