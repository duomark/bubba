%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012, 2017, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Module to create a new process which receives messages and then times
%%%   queueing and dequeueing of messages.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_mq_raw).

-copyright("(c) 2012, 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').
-license('New BSD').

%% External API
-export([run_test/3]).

%% Exported to allow timer:tc to call.
-export([send_msgs/2]).

%% Spawned message queue process API
-export([loop/1, unload_all_proc_lib_msgs/1, unload_any_proc_lib_msgs/1]).

-define(QUEUE_FILL_TIME, 60000).


%%%===================================================================
%%% External API
%%%===================================================================

-type timing_result() :: {send_time | recv_time, pos_integer()}.
-type run_result() :: {proc_lib, pos_integer(), [timing_result(), ...]}.

-spec run_test(MsgCount::pos_integer(), NumTimes::non_neg_integer(), Msgs::bubba_mq_data:msg_set()) -> [run_result() | none].

%% @doc Fill a new process' mailbox queue, then dequeue and report timings.
run_test(MsgCount, NumTimes, Msgs)
  when is_integer(MsgCount), is_integer(NumTimes), is_tuple(Msgs) ->
    run_test(MsgCount, NumTimes, Msgs, []).

run_test(_MsgCount, 0, _Msgs, Results) -> Results;
run_test(MsgCount, NumTimes, Msgs, Results)
  when is_integer(MsgCount), is_integer(NumTimes), is_tuple(Msgs) ->
    {ok, P1} = start_loop(self()),
    MsgList = gen_msgs(MsgCount, Msgs, []),
    garbage_collect(),
    {SendTime, {last}} = timer:tc(?MODULE, send_msgs, [P1, MsgList]),
    NewResults = wait_and_get_results(MsgCount, SendTime),
    run_test(MsgCount, NumTimes-1, Msgs, [NewResults | Results]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec wait_and_get_results(pos_integer(), pos_integer()) ->  run_result() | none.
-spec gen_msgs(non_neg_integer(), bubba_mq_data:msg_set(), list()) -> list().
-spec send_msgs(pid(), list()) -> {last}.

wait_and_get_results(Count, SendMicros) ->
    receive
        {proc_lib, Count, RecvMicros} ->
            {proc_lib, Count, [{send_time, SendMicros}, {recv_time, RecvMicros}]}
    after
        ?QUEUE_FILL_TIME -> none
    end.

%% Generate N msgs randomly selected from a pre-built tuple.
gen_msgs(0, _SrcMsgs, GenMsgs) -> GenMsgs;
gen_msgs(N, SrcMsgs, GenMsgs) ->
    M = rand:uniform(tuple_size(SrcMsgs)),
    gen_msgs(N-1, SrcMsgs, [element(M, SrcMsgs) | GenMsgs]).

    
%% Send msgs as quickly as possible to another process.
%% The msg {last} is sent to signal that message sends have completed.
send_msgs(Pid, []) ->
    Pid ! {last};
send_msgs(Pid, [Msg|Msgs]) ->
    Pid ! {msg, self(), Msg},
    send_msgs(Pid, Msgs).


%% ============= proc_lib =====================================================

-spec start_loop(pid()) -> {ok, pid()}.
-spec loop(pid()) -> ok.
-spec unload_all_proc_lib_msgs(pos_integer()) -> pos_integer().
-spec unload_any_proc_lib_msgs(pos_integer()) -> pos_integer().

start_loop(Caller) ->
    Pid = proc_lib:spawn(?MODULE, loop, [Caller]),
    %% register(mq_spawn, Pid),
    {ok, Pid}.

loop(Caller) ->
    receive {last} -> ok
    after   ?QUEUE_FILL_TIME -> ok
    end,
    %% garbage_collect(),
    %% {Time, Count} = timer:tc(?MODULE, unload_all_proc_lib_msgs, [0]),
    {Time, Count} = timer:tc(?MODULE, unload_any_proc_lib_msgs, [0]),
    Caller ! {proc_lib, Count, Time},
    ok.

unload_all_proc_lib_msgs(Count) ->
    receive
        {msg, _Pid, Msg} when is_binary(Msg) ->
            unload_all_proc_lib_msgs(Count + 1)
    after
        0 -> Count
    end.

unload_any_proc_lib_msgs(Count) ->
    receive
        _Any -> unload_any_proc_lib_msgs(Count + 1)
    after
        0 -> Count
    end.

