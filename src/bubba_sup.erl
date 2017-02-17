%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012,2017, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Benchmark server for invoking experimental performance tests.
%%%
%%%   Replaced YAWS with elli and upgraded to use R19 and supervisor
%%%   option maps in v0.0.9.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_sup).

-license('New BSD').
-copyright("(c) 2012, 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').

-behaviour(supervisor).

%% External API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> supervisor:startchild_ret().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init({}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init({}) ->
    Elli_Opts = [{callback, benchmark}, {port, bubba_env:get_port()}],
    Elli      = worker_child(elli,         start_link, [Elli_Opts]),
    Bubba     = worker_child(bubba_server, start_link, []),
    {ok, {one_for_one_sup_options(5,10), [Elli, Bubba]} }.

one_for_one_sup_options(Intensity, Period) ->
   #{
      strategy  => one_for_one,
      intensity => Intensity,     % Num failures allowed,
      period    => Period         % Within this many seconds
    }.

worker_child(Mod, Fun, Args) ->
    #{
       id      =>  Mod,
       start   => {Mod, Fun, Args},
       restart =>  permanent,
       type    =>  worker,
       modules => [Mod]
     }.
