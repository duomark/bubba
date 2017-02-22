%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012, 2017, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Benchmarking Using Browser-Based Animation (BUBBA) is an app used
%%%   to compare the relative performance of erlang code while viewing the
%%%   results graphically in a web browser.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_app).

-copyright("(c) 2012, 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').
-license('New BSD').

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-type restart_type() :: 'permanent' | 'transient' | 'temporary'.

-spec start()                                         ->  ok | {error, any()}.
-spec start(application:start_type(), restart_type()) -> {ok, pid()}.
-spec stop([])                                        ->  ok.

start ()                       -> application:start(bubba).
start (_StartType, _StartArgs) -> bubba_sup:start_link().
stop  (_State)                 -> ok.
