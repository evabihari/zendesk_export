%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(groups).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({groups,   
       {string, "url"},
       {number,"id"},
       {string,"name"},
       {boolean, "deleted"},
       {string, "created_at"},
       {string, "updated_at"}
}).

%% API
-export([encode/1,
	 decode/1,
	 start1/0]).

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
    from_json(Term,groups).
%%%===================================================================
%%% Internal functions
%%%===================================================================

start1() ->
    R=#groups{
	 url="https://esl1.zendesk.com/api/v2/groups/22004826.json",
	 id=22004826,
	 name="Wombat",
	 deleted=false,
	 created_at="2014-07-07T12:24:17Z",
	 updated_at="2014-07-07T12:24:17Z"},
    {ok,Json}=to_json(R),
    io:format("~s~n", [Json]),
    {ok, R1} = from_json(Json,groups),
    io:format("~p~n",[R1]).
	 
