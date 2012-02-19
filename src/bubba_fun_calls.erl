%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Test the speed of function calls and data accesses.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_fun_calls).

-license("New BSD").
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% External API
-export([run_exec_test/3, run_access_test/4]).

%% Choice of tests to run.
-export([
         operator_plus/1, operator_minus/1, operator_times/1, operator_divide/1,
         operator_rem/1, operator_gt/1, operator_lt/1, operator_eq/1, operator_eeq/1,
         function_call/1, mfa_call/3,
         list_comp/1, list_loop/1, list_foreach/1,
         binary_comp/1, binary_loop/1,
         list_nth/2, list_head/2,
         binary_raw/2, binary_at/2,
         tuple_inx/2
        ]).

%% Spawned process API
-export([loop/3, loop/4]).
-export([auto_expire_proc/0, my_spawn/1, my_spawn_link/1]).

%% Type definitions
-include("bubba.hrl").

-define(OPER_MAX, 100).


%%%===================================================================
%%% External API
%%%===================================================================

-type timing_result()   :: {loop_time, pos_integer()}.
-type run_result()      :: {proc_lib, pos_integer(), [timing_result(), ...]}.

-spec run_exec_test(Fun::valid_exec_types(), NumExecsPerTest::pos_integer(), TimesToTest::pos_integer()) -> [run_result() | none].
-spec run_access_test(Fun::valid_access_types(), DataSize::pos_integer(), NumAccessesPerTest::pos_integer(), TimesToTest::pos_integer()) -> [run_result() | none].

%% @doc Execute a function repeatedly for each test.
run_exec_test(Fun, NumExecs, TestTimes)
  when is_integer(NumExecs), is_integer(TestTimes) ->
    run_exec_test(Fun, NumExecs, TestTimes, []).

run_exec_test(_Fun, _NumExecs, 0, Results) -> Results;
run_exec_test(Fun, NumExecs, TestTimes, Results)
  when is_integer(NumExecs), is_integer(TestTimes) ->
    garbage_collect(),
    {ok, _P1} = start_loop(Fun, self(), NumExecs),
    NewResults = wait_and_get_results(NumExecs),
    run_exec_test(Fun, NumExecs, TestTimes-1, [NewResults | Results]).

%% @doc Access a randomly generated data structure repeatedly for each test.
run_access_test(Fun, DataSize, NumAccesses, TestTimes)
  when is_integer(DataSize), is_integer(NumAccesses), is_integer(TestTimes) ->
    run_access_test(Fun, DataSize, NumAccesses, TestTimes, []).

run_access_test(_Fun, _DataSize, _NumAccesses, 0, Results) -> Results;
run_access_test(Fun, DataSize, NumAccesses, TestTimes, Results)
  when is_integer(DataSize), is_integer(NumAccesses), is_integer(TestTimes) ->
    garbage_collect(),
    {ok, _P1} = start_loop(Fun, self(), DataSize, NumAccesses),
    NewResults = wait_and_get_results(NumAccesses),
    run_access_test(Fun, DataSize, NumAccesses, TestTimes-1, [NewResults | Results]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec wait_and_get_results(pos_integer()) ->  run_result() | none.

-define(TEST_SETUP_TIME, 4000).

wait_and_get_results(Count) ->
    receive
        {proc_lib, Count, LoopMicros} ->
            {proc_lib, Count, [{loop_time, LoopMicros}]}
    after
        ?TEST_SETUP_TIME -> none
    end.


%% ============= proc_lib =====================================================

%% Function execution tests
-spec start_loop(valid_exec_types(), pid(), pos_integer()) -> {ok, pid()}.
-spec loop(valid_exec_types(), pid(), pos_integer()) -> ok.

start_loop(Fun, Caller, LoopCount) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Fun, Caller, LoopCount]),
    {ok, Pid}.

loop(Fun, Caller, LoopCount) ->
    Args = exec_arglist(Fun, LoopCount),
    {Time, _Val} = timer:tc(?MODULE, Fun, Args),
    Caller ! {proc_lib, LoopCount, Time},
    ok.


%% Data structure access tests
-spec start_loop(valid_access_types(), pid(), pos_integer(), pos_integer()) -> {ok, pid()}.
-spec loop(valid_access_types(), pid(), pos_integer(), pos_integer()) -> ok.

start_loop(Fun, Caller, DataSize, LoopCount) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Fun, Caller, DataSize, LoopCount]),
    {ok, Pid}.

loop(Fun, Caller, DataSize, LoopCount) ->
    Args = access_arglist(Fun, DataSize, LoopCount),
    {Time, _Val} = timer:tc(?MODULE, Fun, Args),
    Caller ! {proc_lib, LoopCount, Time},
    ok.


%% ==== Utilities for function execution tests ===================

%% Operators
exec_arglist(operator_plus,   LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_minus,  LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_times,  LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_divide, LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_rem,    LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_gt,     LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_lt,     LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_eq,     LC) -> [make_arg_pairs(LC)];
exec_arglist(operator_eeq,    LC) -> [make_arg_pairs(LC)];

%% Function calls and looping constructs
exec_arglist(function_call,   LC) -> [LC];
exec_arglist(mfa_call,        LC) -> [?MODULE, mfa_call, LC];
exec_arglist(list_comp,       LC) -> [make_random_inxs(LC, ?OPER_MAX)];
exec_arglist(list_loop,       LC) -> [make_random_inxs(LC, ?OPER_MAX)];
exec_arglist(list_foreach,    LC) -> [make_random_inxs(LC, ?OPER_MAX)];
exec_arglist(binary_comp,     LC) -> [list_to_binary(make_random_inxs(LC, ?OPER_MAX))];
exec_arglist(binary_loop,     LC) -> [list_to_binary(make_random_inxs(LC, ?OPER_MAX))];
               
%% Process spawns, links, etc
exec_arglist(my_spawn,        LC) -> [LC];
exec_arglist(my_spawn_link,   LC) -> [LC].

make_arg_pairs(LoopCount) ->
    Numbers = make_random_inxs(LoopCount*2, ?OPER_MAX),
    return_pairs(Numbers, []).

return_pairs([], Pairs) -> Pairs;
return_pairs([A,B | Rest], Pairs) -> 
    O1 = A - ?OPER_MAX div 2,
    O2 = case B - ?OPER_MAX div 2 of 0 -> 1; Num -> Num end,
    return_pairs(Rest, [{O1, O2} | Pairs]).

%% Random indexes are in the range 1 - N-1 so that bin and tuple work.
make_random_inxs(Num, MaxInx) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    make_random_inxs(Num, MaxInx-1, []).

make_random_inxs(0, _MaxInx, Inxs) -> Inxs;
make_random_inxs(N, MaxInx, Inxs) when N > 0 -> 
    make_random_inxs(N-1, MaxInx, [random:uniform(MaxInx) | Inxs]).


%% ==== Utilities for data access tests ==========================

%% Accessing data structures
access_arglist(tuple_inx,  DataSize, LC) -> make_tuple_args(DataSize, LC);
access_arglist(list_head,  DataSize, LC) -> make_list_args(DataSize, LC);
access_arglist(list_h_t,   DataSize, LC) -> make_list_args(DataSize, LC);
access_arglist(list_nth,   DataSize, LC) -> make_list_args(DataSize, LC);
access_arglist(binary_at,  DataSize, LC) -> make_bin_args(DataSize, LC);
access_arglist(binary_raw, DataSize, LC) ->
    [Inxs, Bin] = make_bin_args(DataSize, LC),
    [[X*8 || X <- Inxs], Bin].


make_list_args(ListSize, LoopCount) ->
    List = lists:seq(1, ListSize),
    RepeatTimes = case ListSize > LoopCount of
                      true ->  LoopCount;
                      false -> ListSize - 1
                  end,
    [RepeatTimes, List].

make_bin_args(BinSize, LoopCount) ->    
    Bin = make_bin(BinSize),
    Inxs = make_random_inxs(LoopCount, BinSize),
    [Inxs, Bin].

make_tuple_args(TupleSize, LoopCount) ->
    Tuple = make_tuple(TupleSize),
    Inxs = make_random_inxs(LoopCount, TupleSize),
    [Inxs, Tuple].

make_bin(Size) ->   list_to_binary(lists:seq(1,Size)).
make_tuple(Size) -> list_to_tuple(lists:seq(1,Size)).


%% ============= exec funs =====================================================
-spec operator_plus  ([{integer(), integer()}]) -> ok.
-spec operator_minus ([{integer(), integer()}]) -> ok.
-spec operator_times ([{integer(), integer()}]) -> ok.
-spec operator_divide([{integer(), integer()}]) -> ok.
-spec operator_rem   ([{integer(), integer()}]) -> ok.
-spec operator_gt    ([{integer(), integer()}]) -> [integer()].
-spec function_call  (non_neg_integer()) -> ok.
-spec mfa_call       (module(), atom(), non_neg_integer()) -> ok.
-spec list_comp      ([non_neg_integer()]) -> ok.
-spec list_loop      ([non_neg_integer()]) -> ok.
-spec list_foreach   ([non_neg_integer()]) -> ok.
-spec binary_comp    (binary()) -> ok.
-spec binary_loop    (binary()) -> ok.

operator_plus  (Pairs) -> _ = [A+B || {A,B} <- Pairs], ok.
operator_minus (Pairs) -> _ = [A-B || {A,B} <- Pairs], ok.
operator_times (Pairs) -> _ = [A*B || {A,B} <- Pairs], ok.
operator_divide(Pairs) -> _ = [A div B || {A,B} <- Pairs], ok.
operator_rem   (Pairs) -> _ = [A rem B || {A,B} <- Pairs], ok.
operator_gt    (Pairs) -> _ = [if A < B -> 1; true -> 0 end || {A,B} <- Pairs].
operator_lt    (Pairs) -> _ = [if A > B -> 1; true -> 0 end || {A,B} <- Pairs].
operator_eq    (Pairs) -> _ = [if A == B -> 1; true -> 0 end || {A,B} <- Pairs].
operator_eeq   (Pairs) -> _ = [if A =:= B -> 1; true -> 0 end || {A,B} <- Pairs].
    
function_call(0) -> ok;
function_call(Count) when Count > 0 ->
    function_call(Count-1).

mfa_call(_M, _F, 0) -> ok;
mfa_call(M, F, Count) when Count > 0 ->
    M:F(M, F, Count-1).

list_comp(Nums) -> _ = [N || N <- Nums], ok.
binary_comp(Nums) -> _ = << <<N>> || <<N>> <= Nums >>, ok.

list_loop([]) -> ok;
list_loop([_H|T]) ->
    list_loop(T).

list_foreach(L) ->
    lists:foreach(fun(T) -> T end, L).

binary_loop(<<>>) -> ok;
binary_loop(<< _C, Rest/binary >>) ->
    binary_loop(Rest).

my_spawn(0) -> ok;
my_spawn(N) ->
    spawn(?MODULE, auto_expire_proc, []),
    my_spawn(N-1).

my_spawn_link(0) -> ok;
my_spawn_link(N) ->
    spawn_link(?MODULE, auto_expire_proc, []),
    my_spawn_link(N-1).

auto_expire_proc() -> receive after 5000 -> expired end.
     

%% ============= access funs =====================================================
-spec list_nth(non_neg_integer(), list(pos_integer())) -> ok.
-spec list_head(non_neg_integer(), list(pos_integer())) -> ok.
-spec binary_at([non_neg_integer(),...],  binary()) -> [byte()].
-spec binary_raw([non_neg_integer(),...], binary()) -> [byte()].
-spec tuple_inx([pos_integer(),...],      tuple(integer())) -> [pos_integer()].

list_nth(N, L) -> lists:nth(N,L).
    
list_head(0, _List) -> ok;
list_head(1, [_H]) -> ok;
list_head(N, [_H|T]) when N > 0 -> 
    list_head(N-1, T).
    
binary_raw(Positions, Bin) ->
    [ begin << _B:P, C, _Rest/binary >> = Bin, C end || P <- Positions ].
                
binary_at(Positions, Bin) ->
    [ binary:at(Bin, P) || P <- Positions ].
                
tuple_inx(Positions, Tuple) ->
    [ element(P, Tuple) || P <- Positions ].
