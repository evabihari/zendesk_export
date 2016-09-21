%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(comment).
%% -compile({parse_transform, 'ejson_trans'}).
-include("../include/record.hrl").

-json({comment,   
       {number,"id"},
       {string,"type"},
       {string,"body"},
       {string,"html_body"},
       {boolean, "public"},
       {string, "created_at"},
       {number,"author_id"},
       {list, "attachments", [{type,attachment}]},
%       {string, "metadata"},
       skip,
%       {string, via}
       skip
}).

-json({attachment,
       {number,"id"},
       {string,"file_name"},
       {string,"content_url"},
       {string,"content_type"},
       {number, "size"},
       skip
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
    from_json(Term,comment).
%%%===================================================================
%%% Internal functions
%%%===================================================================


