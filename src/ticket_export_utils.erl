%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(ticket_export_utils).

-include("../include/record.hrl").

%% API
-export([read_tickets/0,
	 request_download/2,
	 get_groups/0,
	 get_group_id/1,
	 get_group_name/1,
	 get_organizations/0,
	 get_org_id/1,
	 get_org_name/1,
	 get_tickets_by_org/1,
	 get_tickets_by_group/1,
	 get_all_tickets/0,
	 get_ticket_comments/1,
	 collect/2,
	 get_attachment/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
read_tickets()->
    ok.

get_groups() ->
    Url=?ZENDESK_URL++"groups.json",
    case request(Url) of
	{error,{error, timeout}} ->
	    io:format("request failed due to timeout, try again ~n",[]),
	    timer:sleep(30000),
	    get_organizations();    
    {ok,Body} ->
	{Groups,_Count}=decode(list_to_binary("groups"), Body),
	    [G|_]=Groups,
	    % io:format("Response groups: ~p~n",[G]),
	OrgRecords=collect(groups,Groups),
	{ok, OrgRecords};
	Other  ->
	    io:format("request failed due to ~p ~n",[Other])
     end.


get_organizations() ->
    Url=?ZENDESK_URL++"organizations.json",
    case request(Url) of
	{error,{error, timeout}} ->
	    io:format("request failed due to timeout, try again ~n",[]),
	    timer:sleep(30000),
	    get_organizations();    
    {ok,Body} ->
	{Orgs,_Count}=decode(list_to_binary("organizations"), Body),
	    [O|_]=Orgs,
	   % io:format("Response organizations: ~p~n",[O]),
	OrgRecords=collect(organization,Orgs),
	{ok, OrgRecords};
	Other  ->
	    io:format("request failed due to ~p ~n",[Other])
     end.

get_all_tickets() ->
    Url=?ZENDESK_URL++"incremental/tickets.json?start_time=1383734680",
    case request(Url) of
	{error,{error, timeout}} ->
	    io:format("request failed due to timeout, try again ~n",[]),
	    timer:sleep(30000),
	    get_all_tickets();
	{ok,Body} ->
	    {Tickets,_Count}=decode(list_to_binary("tickets"), Body),
	    [T|_]=Tickets,
	    % io:format("Response tickets: ~p~n",[T]),
	    TicketRecords=collect(tickets,Tickets),
	    {ok,TicketRecords};
	Other  ->
	    io:format("request failed due to ~p ~n",[Other])
     end.


get_tickets_by_org(OrgId) when is_integer(OrgId) ->
    gen_server:call(ticket_export_worker,{tickets_by_org,OrgId});
get_tickets_by_org(OrgName) ->
    {ok,Id}=get_org_id(OrgName),
    get_tickets_by_org(Id).

get_org_id(Org_Name) ->
    gen_server:call(ticket_export_worker,{org_id_by_name,Org_Name}).
get_org_name(Org_Id) ->
    gen_server:call(ticket_export_worker,{org_name_by_id,Org_Id}).
get_group_id(GroupName) ->
    gen_server:call(ticket_export_worker,{group_id_by_name,GroupName}).
get_group_name(Group_Id) ->
    gen_server:call(ticket_export_worker,{group_name_by_id,Group_Id}).

get_tickets_by_group(GroupId) when is_integer(GroupId) ->
    gen_server:call(ticket_export_worker,{tickets_by_group,GroupId});
get_tickets_by_group(GroupName) ->
     {ok,Id} = get_group_id(GroupName),
     get_tickets_by_group(Id).   

    
%%%===================================================================

%%% Internal functions
%%%===================================================================
request(Url) ->
    ContentType = "application/json",
    Headers=[get_auth_header(), {"Content-Type",ContentType}],
    case hackney:request("GET",Url,Headers) of
	{ok,200,_Head,Ref} ->
	    case hackney:body(Ref) of
		{ok, Body} ->
		    {ok, Body};
		{error, timeout} ->
		    timer:sleep(30000),
		    request(Url)
	    end;
	Result -> io:format("Http request failed, Result=~p~n",[Result]),
	    {error, Result}
	end.

request_download(Url,TargetFile) ->
    gen_server:cast(ticket_export_worker,{download_attachment,{Url,TargetFile}}).

get_auth_header() ->
    {"Authorization","Basic " ++ base64:encode_to_string(lists:append([?USER ,":",?PWD]))}.

decode(Type, Body) ->
    Struct=mochijson2:decode(Body),
    {struct,TypeStruct}=Struct,
    [{Type,Items},T1,T2,T3]=TypeStruct,    
    CountField= <<"count">>,
    Count = case lists:keyfind(CountField,1,[T1,T2,T3]) of
	{CountField,Value} ->
               Value;
        _ ->
               0
    end,
    {Items,Count}.

%% filter_by_value([],_Field,_Name) ->
%%     {not_found,[]};
%% filter_by_value([Struct|List],Field,Name) ->
%%     {struct,FieldList}=Struct,
%%     case lists:keyfind(Field, 1, FieldList) of
%% 	{Field, Name} ->
%% 	    {ok, FieldList};
%% 	_ -> 
%% 	    filter_by_value(List,Field,Name)
%%     end.

%% collect_values([],_Fields) ->
%%     [];
%% collect_values([OrgStruct|List],Field) ->
%%     {struct,FieldsList}=OrgStruct,
%%     Value= case lists:keyfind(Field, 1, FieldsList) of
%% 	 [] ->
%% 	    not_found,[];
%% 	{Field, Name} ->
%% 	     Name
%% 	   end,
%%     [Value|collect_values(List,Field)].


collect(_Type,[]) ->
    [];
collect(Type,[Item|TList]) ->
    Json=mochijson2:encode(Item),
    {ok,ItemRec}=erlang:apply(Type,decode,[list_to_binary(Json)]),
    [ItemRec|collect(Type,TList)].

get_ticket_comments(Id) ->    
    Url=?ZENDESK_URL++"tickets/"++integer_to_list(Id)++"/comments.json",
    case request(Url) of
	{error, timeout} ->
	    io:format("request failed due to timeout, try again ~n",[]),
	    timer:sleep(30000),
	    get_ticket_comments(Id);
	{ok,Body} ->
	    {Comments,_Count}=decode(list_to_binary("comments"), Body),
	    % io:format("Response: ~s~n",[Body]),
	    [C|_]=Comments,
	    % io:format("1st comment: ~p~n",[C]),
	    CommentRecords=collect(comment,Comments),
	    {ok,CommentRecords};
	 Other  ->
	    io:format("request failed due to ~p ~n",[Other])
     end.

get_attachment(Url,TargetFile) ->
    Headers=[get_auth_header()],
    ReqBody = <<>>,
    Options = [{follow_redirect, true}, {max_redirect, 5}],
    case hackney:request(get, Url, Headers, ReqBody, Options) of
        {ok, _S, _H, Ref} ->
	    case hackney:body(Ref) of
		{ok, Body1} ->
			  {ok,IO}=file:open(TargetFile, [write, binary]),
			  file:write(IO,Body1),
			  gen_server:cast(ticket_export_worker,download_ready);
		{error,timeout} ->
		    io:format("Timeout while downloading file , wait a bit and try again Url=~p~n",[ Url]),
		    timer:sleep(30000),
		    get_attachment(Url,TargetFile)
	    end;
	{error, {closed,_O}} ->
	    io:format("Connection closed with reason ~p, wait a bit and try again Url=~p~n",[_O, Url]),
	    timer:sleep(30000),
	    get_attachment(Url,TargetFile)
    end.
