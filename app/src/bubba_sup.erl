%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Benchmark server for invoking experimental performance tests.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_sup).

-license("New BSD").
-copyright("(c) 2012, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(supervisor).

%% External API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.

%% @doc Start the root BUBBA supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(Args::{}) -> {ok, any()}.

-define(CHILD(I, ARGS), {I, {I, start_link, ARGS}, permanent, 5000, worker, [I]}).
-define(SUPER(I, ARGS), {I, {I, start_link, ARGS}, permanent, infinity, supervisor, [I]}).

%% @doc Initialize the root supervisor to manage a root dk_yaws supervisor and a BUBBA server.
init({}) ->
    YawsSup = ?SUPER(dk_yaws_sup, []),
    BubbaServer = ?CHILD(bubba_server, []),
    {ok, { {one_for_one, 5, 10}, [YawsSup, BubbaServer]} }.

