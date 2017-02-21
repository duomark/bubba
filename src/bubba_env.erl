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

-copyright("(c) 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').
-license('New BSD').

%% External API
-export([get_ip/0, get_port/0]).


%%%===================================================================
%%% API functions
%%%===================================================================
-type ip_elem() :: non_neg_integer().

-spec get_ip   () -> {ip_elem(), ip_elem(), ip_elem(), ip_elem()}.
-spec get_port () -> pos_integer().

get_ip   () -> get_app_env(ip,  {127,0,0,1}).
get_port () -> get_app_env(port, 9998).


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
