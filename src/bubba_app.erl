%%%------------------------------------------------------------------------------
%%% @copyright (c) 2012, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Benchmarking Using Browser-Based Animation (BUBBA) is an app used
%%%   to compare the relative performance of erlang code while viewing the
%%%   results graphically in a web browser.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_app).
-copyright("(c) 2012, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start() -> {ok, pid()}.
-spec start(any(), any()) -> {ok, pid()}.
-spec stop([]) -> ok.

%% @doc Start the application's root supervisor in erl listener.
start() ->
    bubba_sup:start_link().

%% @doc Start the application's root supervisor from boot.
start(_StartType, _StartArgs) ->
    bubba_sup:start_link().

%% @doc Stop the application.
stop(_State) -> ok.
