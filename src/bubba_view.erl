%%%------------------------------------------------------------------------------
%%% @copyright (c) 2017, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Elli callback module for d3 visualization of results.
%%% @since v0.0.9
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_view).

-copyright("(c) 2012, 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').
-license('New BSD').

-behaviour(elli_handler).

%% External API
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%%% Handle HTTP requests
handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [], _Req) ->
    File = "priv/index.html",
    {ok, [], {file, File}};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%%% Event signals from Elli
handle_event(_Event, _Data, _Args) ->
    ok.
