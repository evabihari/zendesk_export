%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(tickets).
-include("../include/record.hrl").
%% API
-export([]).
-export([encode/1,
	 decode/1]).
-export([string_or_bool_to_jsx/2,
         string_or_bool_from_jsx/1,
	 binlist_to_jsx/2,
	 binlist_from_jsx/1]).


-json({tickets,
	  {string,"url"},
	  {number,"id"},
	  {string,"external_id"},
%          {string,"via"},
	  skip,
	  {string,"created_at"},
	  {string,"updated_at"},
	  {string,"type"},
	  {string,"subject"},
	  {string,"raw_subject"},
	  {string,"description"},
	  {string,"priority"},
	  {string,"status"},
	  {string,"recipient"},
	  {number,"requester_id"},
          {string,"requester",[{default, "not_known"}]},
	  {number,"submitter_id"},
          {string,"submitter",[{default, "not_known"}]},
	  {number,"assignee_id"},
	  {number,"organization_id"},
	  {number,"group_id"},
	  {list,"collaborator_ids",[{type,number}]},
	  {number,"forum_topic_id"},
	  {number,"problem_id"},
	  {boolean,"has_incidents"},
	  {boolean,"is_public"},
	  {string,"due_at"},
	  {list,"tags",[string,
		       {pre_encode,  {?MODULE, binlist_to_jsx}},
                       {post_decode, {?MODULE, binlist_from_jsx}}]},
	  {list,"custom_fields",[{type,field}]},
	  {string,"satisfaction_rating"},
	  {list,"sharing_agreement_ids",[{type,number}]},
	  {list,"fields",[{type,field}]},
	  {number,"brand_id"},
	  {boolean,"allow_channelback"},
	  {number,"generated_timestamp"}}).

-json({field,
       {number, "id"},
       {generic, "value", [{pre_encode,  {?MODULE, string_or_bool_to_jsx}},
                           {post_decode, {?MODULE, string_or_bool_from_jsx}}
			  ]}}).


% what is happening if the field has other type, ex. boolean?
% how can we decode that part?


-define(R2P(Record), record_to_proplist(#Record{} = Rec) ->
	       lists:zip(record_info(fields, Record), tl(tuple_to_list(Rec)))).

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
    from_json(Term,tickets).
%%%===================================================================
%%% Internal functions
%%%===================================================================
    
string_or_bool_to_jsx(_Rec,String) when is_list(String) ->
    unicode:characters_to_binary(String);
string_or_bool_to_jsx(_Rec,Bool) when is_boolean(Bool) ->
    Bool;
string_or_bool_to_jsx(_Rec,BinString) when is_binary(BinString) ->
    binary_to_list(BinString);
string_or_bool_to_jsx(_Rec,null) -> 
    null.


string_or_bool_from_jsx(String) when is_list(String) ->
%    io:format("string_or_bool_from_jsx ~p~n",[String]),
    unicode:characters_to_list(String, utf8);
string_or_bool_from_jsx(Bool) when is_boolean(Bool) ->
%    io:format("string_or_bool_from_jsx ~p~n",[Bool]),
    Bool;
string_or_bool_from_jsx(BinString) when is_binary(BinString) ->
    binary_to_list(BinString);
string_or_bool_from_jsx(null) ->
    null.

binlist_to_jsx(_Rec,InList) ->
    % io:format("binlist_to_jsx _Rec=~p InList=~p~n",[_Rec,InList]),
    [B|_]=InList,
    Result=case is_binary(B) of
	true ->
	    List=[binary_to_list(X) || X <- InList],
	    lists:flatten(string:join([[X] || X <- List],","));
	_ ->
	    InList
    end,
    Result.

binlist_from_jsx(BinList) ->
    % io:format("binlist_from_jsx  BinList=~p~n",[BinList]),
    [binary_to_list(X) || X <- BinList].
