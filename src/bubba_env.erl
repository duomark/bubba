%%%------------------------------------------------------------------------------
%%% @copyright (c) 2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Environment configuration variables, plus access functions with
%%%   default values supplied. The file rel/sys.config contains the
%%%   actual runtime configured values for production.
%%%
%%% @since v0.0.9
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_env).

-license('New BSD').
-copyright("(c) 2012, 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').

%% External API
-export([get_port/0]).


%%%===================================================================
%%% API functions
%%%===================================================================
-spec get_port() -> pos_integer().

get_port() -> get_app_env(sse_port, 9998).


%%%===================================================================
%%% Support functions
%%%===================================================================
-spec get_app_env(atom(), any()) -> any().

get_app_env(Param, Default) ->
    case application:get_env(bubba, Param) of
        {ok, Val} -> Val;
        undefined ->
            case init:get_argument(Param) of
                {ok, [[FirstVal | _OtherVals] | _MoreVals]} -> FirstVal;
                error -> Default
            end
    end.
