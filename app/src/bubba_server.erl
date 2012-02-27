%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Benchmark server for invoking experimental performance tests.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_server).

-license("New BSD").
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         mq_raw/1, mq_raw/2,
         exec/2, exec/3,
         access/3, access/4
        ]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("bubba.hrl").

-define(SERVER, ?MODULE).
-define(RESULT(__Type, __Call), {__Type, __Value} = __Call, __Value).

-record(dkb_state, {}).


%%%===================================================================
%%% API
%%%===================================================================

%% Start a bubba server.
-spec start_link() -> {ok, pid()}.

%% Message queueing speed.
-spec mq_raw(NumMsgs::pos_integer()) -> list().
-spec mq_raw(NumMsgs::pos_integer(), TimesToRun::pos_integer()) -> list().

%% Loop executing a single function.
-spec exec(Fun::valid_exec_types(), LoopCount::pos_integer()) -> list().
-spec exec(Fun::valid_exec_types(), LoopCount::pos_integer(), TimesToRun::pos_integer()) -> list().

%% Accessing a data structure.
-spec access(Fun::valid_access_types(), DataSize::pos_integer(), LoopCount::pos_integer()) -> list().
-spec access(Fun::valid_access_types(), DataSize::pos_integer(), LoopCount::pos_integer(), TimesToRun::pos_integer()) -> list().


%% @doc Start the benchmark server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @doc Test message queuing speed with NumMsgs.
mq_raw(NumMsgs) ->
    ?RESULT(mq_raw, gen_server:call(?SERVER, {mq_raw, NumMsgs})).

%% @doc Test message queueing speed repeating TimesToRun times.
mq_raw(NumMsgs, TimesToRun) ->
    ?RESULT(mq_raw, gen_server:call(?SERVER, {mq_raw, NumMsgs, TimesToRun})).

%% @doc Call a simple function LoopCount times.
exec(Fun, LoopCount) ->
    ?RESULT(exec, gen_server:call(?SERVER, {exec, Fun, LoopCount})).

%% @doc Call a simple function repeating it TimesToRun times.
exec(Fun, LoopCount, TimesToRun) ->
    ?RESULT(exec, gen_server:call(?SERVER, {exec, Fun, LoopCount, TimesToRun})).

%% @doc Iterate over data LoopCount times.
access(Fun, DataSize, LoopCount) ->
    ?RESULT(access, gen_server:call(?SERVER, {access, Fun, DataSize, LoopCount})).

%% @doc Iterate over data repeating it TimesToRun times.
access(Fun, DataSize, LoopCount, TimesToRun) ->
    ?RESULT(access, gen_server:call(?SERVER, {access, Fun, DataSize, LoopCount, TimesToRun})).
    

%%%===================================================================
%%% init, terminate, code_change callbacks
%%%===================================================================

-spec init({}) -> {ok, #dkb_state{}}.
-spec terminate(atom(), #dkb_state{}) -> ok.
-spec code_change(string(), #dkb_state{}, any()) -> {ok, #dkb_state{}}.

%% @private
%% @doc Initialize bubba_server instance by creating an empty #dkb_state{}.
init({}) -> {ok, #dkb_state{}}.

%% @private
%% @doc Terminate the bubba_server, no cleanup is necessary.
terminate(_Reason, _State) -> ok.
%% @private
%% @doc Code change requires no state conversion right now.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% unused callbacks
%%%===================================================================

-spec handle_cast(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.
-spec handle_info(any(), #dkb_state{}) -> {noreply, #dkb_state{}}.

%% @private
%% @doc No cast messages are expected.
handle_cast(_Msg, State) ->  {noreply, State}.
%% @private
%% @doc No info messages are expected.
handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% handle_call callbacks
%%%===================================================================

-type from() :: {pid(), reference()}.

-type mq_call_rqst() ::
        {mq_raw, NumMsgs::pos_integer()}
      | {mq_raw, NumMsgs::pos_integer(), TimesToRun::pos_integer()}.
-type mq_call_reply() :: {mq_raw, list()}.

-type exec_call_rqst() ::
        {exec, Fun::valid_exec_types(), LoopCount::pos_integer()}
      | {exec, Fun::valid_exec_types(), LoopCount::pos_integer(), TimesToRun::pos_integer()}.
-type exec_call_reply() :: {exec, list()}.

-type access_call_rqst() ::
        {access, Fun::valid_access_types(), DataSize::pos_integer(), LoopCount::pos_integer()}
      | {access, Fun::valid_access_types(), DataSize::pos_integer(), LoopCount::pos_integer()}.
-type access_call_reply() :: {access, list()}.

-type call_rqst()  :: mq_call_rqst() | exec_call_rqst() | access_call_rqst().
-type call_reply() :: mq_call_reply() | exec_call_reply() | access_call_reply().

-spec handle_call(call_rqst(), from(), #dkb_state{}) -> {reply, call_reply(), #dkb_state{}}.


%% @private
%% @doc Perform benchmark timing of message queueing, function execution or data structure access.
handle_call({mq_raw, NumMsgs}, _From, #dkb_state{} = State) ->
    {reply, {mq_raw, get_mq_results(NumMsgs)}, State};
handle_call({mq_raw, NumMsgs, TimesToRun}, _From, #dkb_state{} = State) ->
    {reply, {mq_raw, get_mq_results(NumMsgs, TimesToRun)}, State};

handle_call({exec, Fun, LoopCount}, _From, #dkb_state{} = State) ->
    {reply, {exec, get_exec_results(Fun, LoopCount)}, State};
handle_call({exec, Fun, LoopCount, TimesToRun}, _From, #dkb_state{} = State) ->
    {reply, {exec, get_exec_results(Fun, LoopCount, TimesToRun)}, State};

handle_call({access, Fun, DataSize, LoopCount}, _From, #dkb_state{} = State) ->
    {reply, {access, get_access_results(Fun, DataSize, LoopCount)}, State};
handle_call({access, Fun, DataSize, LoopCount, TimesToRun}, _From, #dkb_state{} = State) ->
    {reply, {access, get_access_results(Fun, DataSize, LoopCount, TimesToRun)}, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_exec_results(Fun, LoopCount) ->
    rpt_results(bubba_fun_calls:run_exec_test(Fun, LoopCount, 1)).
get_exec_results(Fun, LoopCount, TimesToRun) ->
    rpt_results(bubba_fun_calls:run_exec_test(Fun, LoopCount, TimesToRun)).

get_access_results(Fun, DataSize, LoopCount) ->
    rpt_results(bubba_fun_calls:run_access_test(Fun, DataSize, LoopCount, 1)).
get_access_results(Fun, DataSize, LoopCount, TimesToRun) ->
    rpt_results(bubba_fun_calls:run_access_test(Fun, DataSize, LoopCount, TimesToRun)).

get_mq_results(NumMsgs) ->
    rpt_results(bubba_mq_raw:run_test(NumMsgs, 1, bubba_mq_data:msgs())).
get_mq_results(NumMsgs, TimesToRun) ->
    rpt_results(bubba_mq_raw:run_test(NumMsgs, TimesToRun, bubba_mq_data:msgs())).

rpt_results([{proc_lib, NumMsgs, Props}]) ->
    [ {Key, {Micros / 1000, milliseconds}, {Micros / NumMsgs, microseconds_per_msg}}
      || {Key, Micros} <- Props ];
rpt_results(PropList) ->
    [
     [
      {Key, {Micros / 1000, ms}, {int_ceil(Micros / NumMsgs * 1000), nanos_per}}
      || {Key, Micros} <- Props
     ]  || {proc_lib, NumMsgs, Props} <- PropList
    ].

int_ceil(X) ->
    T = trunc(X),
    case X > T of
        true -> T + 1;
        false -> T
    end.

