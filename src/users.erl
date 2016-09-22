%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(users).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({users,   
       {number,"id"},
       {string,"email"},
       {string,"name"},
       {boolean, "active"},
       {string, "role"},
       {number, "organization_id"}
}).


%% API
-export([encode/1,
	 decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
encode(Term) ->
    to_json(Term).
decode(Term) ->
    from_json(Term,users).
%%%===================================================================
%%% Internal functions
%%%===================================================================


