%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(organization).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({organization,   
       {number,"id"},
       {string,"url"},
       {string,"external_id"},
       {string,"name"},
       {string, "created_at"},
       {string, "updated_at"},
       {list, "domain_names", [string]},
       {string,"details"},
       {string,"notes"},
       {number,"group_id"}
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
    from_json(Term,organization).
%%%===================================================================
%%% Internal functions
%%%===================================================================


