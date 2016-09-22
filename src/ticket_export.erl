%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(ticket_export).

%% API
-export([start/0, start/1]).
-export([export_tickets_by_org/1,
	 export_tickets_by_group/1]).
-export([export_tickets_by_org_and_group/2]).
-export([get_state/0]).

-include("../include/record.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start() ->
    ok = application:ensure_started(inets),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    hackney:start(),
    ok = application:start(ticket_export),
    start1().

start1() ->
    start("Riak").

start(Group) ->
    {ok,Groups}=ticket_export_utils:get_groups(),
    gen_server:cast(ticket_export_worker,{groups,Groups}),
    {ok,Users}=ticket_export_utils:get_users(),
    gen_server:cast(ticket_export_worker,{users,Users}),
    {ok,Orgs}  =ticket_export_utils:get_organizations(),
    gen_server:cast(ticket_export_worker,{orgs,Orgs}),
    {ok,Tickets} = ticket_export_utils:get_all_tickets(),
    gen_server:cast(ticket_export_worker,{tickets,Tickets}),
    % io:format("Actual state: ~p~n",[get_state()]),
    {ok,Group_id} =gen_server:call(ticket_export_worker,{group_id_by_name,Group}),
    gen_server:cast(ticket_export_worker,{group_id,{Group,Group_id}}).

get_state() ->
    gen_server:call(ticket_export_worker,get_state).

export_tickets_by_org(OrgId) when is_integer(OrgId) ->
    OrgTickets=ticket_export_utils:get_tickets_by_org(OrgId),
    pretty_print(OrgTickets);
export_tickets_by_org([]) ->
    ok;
export_tickets_by_org(OrgName) ->
    {ok,OrgId} = ticket_export_utils:get_org_id(OrgName),
    export_tickets_by_org(OrgId).


export_tickets_by_org_and_group(OrgId,GroupId) when (is_integer(OrgId) and is_integer(GroupId)) -> 
    Tickets=gen_server:call(ticket_export_worker,{tickets_by_org_and_group,OrgId,GroupId}),
    pretty_print(Tickets);
export_tickets_by_org_and_group(OrgId,GroupName) when is_integer(OrgId)-> 
    {ok,GroupId} = ticket_export_utils:get_group_id(GroupName),
    export_tickets_by_org_and_group(OrgId,GroupId);
export_tickets_by_org_and_group(OrgName,GroupId) ->
    {ok,OrgId} = ticket_export_utils:get_org_id(OrgName),
    export_tickets_by_org_and_group(OrgId,GroupId).
    
export_tickets_by_group(GroupId) when is_integer(GroupId) ->
   Tickets=ticket_export_utils:get_tickets_by_group(GroupId),
    pretty_print(Tickets);
export_tickets_by_group([]) ->
    ok;
export_tickets_by_group(GroupName) ->
    {ok,GroupId} = ticket_export_utils:get_group_id(GroupName),
    export_tickets_by_group(GroupId).


%%%===================================================================
%%% Internal functions
%%%===================================================================
pretty_print([]) ->
    [];
pretty_print([Ticket|List]) ->
    % io:format("~p~n",[Ticket]),
    Id=Ticket#tickets.id,
    GroupId=Ticket#tickets.group_id,
    OrgId=Ticket#tickets.organization_id,
    % io:format("OrgId=~p, TicketId=~p, Ticket=~p ~n",[OrgId, Id, Ticket]),
    case {OrgId,Ticket#tickets.status} of
	{undefined,_} ->
	    % test ticket, don't export
	    do_nothing;
	{_,"deleted"} ->
	    % ticket deleted don't export
	    do_nothing;
	{Other,_S} when is_integer(Other) ->
	    {ok,OrgName}=ticket_export_utils:get_org_name(OrgId),
	    {ok,GroupName}=ticket_export_utils:get_group_name(GroupId),
	    DirName=?DIR ++ GroupName ++ "/" ++ OrgName++ "/"++ 
		     string:right(integer_to_list(Id), 4, $0),
	    filelib:ensure_dir(DirName++"/a"),  
	    %% make sure all subdirectory exist, if not it will be created
	    RequesterId=Ticket#tickets.requester_id,
	    {ok,Requester}=ticket_export_utils:find_name_by_id(RequesterId),
	    SubmitterId=Ticket#tickets.submitter_id,
	    {ok,Submitter}=ticket_export_utils:find_name_by_id(SubmitterId),
	    Ticket1=Ticket#tickets{submitter=Submitter,
				   requester=Requester},
	    {ok,Json}=tickets:encode(Ticket1),
	    Pretty=jsx:prettify(Json),
	    {ok,Comments}=ticket_export_utils:get_ticket_comments(Ticket1#tickets.id),
	    _PrettyComments=pretty_print_comments(Comments),
	    write_to_file(Ticket1,Pretty,Comments,DirName)
    end,
    pretty_print(List).

pretty_print_comments([]) ->		   
    [];
pretty_print_comments([Comment|List]) ->
    {ok,Json}=comment:encode(Comment),
    Pretty=jsx:prettify(Json),
    % io:format("~s~n",[Pretty]),
%    io:format("~p~n",[Json]),
    [Pretty|pretty_print_comments(List)].
	    
write_to_file(Ticket, PTicket,Comments,DirName) ->
    Id=Ticket#tickets.id,
    IdName=string:right(integer_to_list(Id), 4, $0),
    TName=DirName++"/"++IdName,
    Name=TName++".txt",
    {ok,File}=file:open(Name, [write]),
    io:format(File,"~s~n",[PTicket]),
    file:close(File),
    add_comments(Comments,1,DirName++"/").

add_comments([],_N,_D) ->
    ok;
add_comments([Comment|List],N,DirName) ->
    Name=DirName ++ string:right(integer_to_list(N), 4, $0)++".comment",
    {ok,File}=file:open(Name, [write]),    
    Author_id=Comment#comment.author_id,
    {ok,Author}=ticket_export_utils:find_name_by_id(Author_id),
    io:format(File,"-----BEGINNING OF COMMENT-----~n",[]),
    io:format(File,"author : ~p~n",[Author]),
    io:format(File,"created_at : ~s~n",[Comment#comment.created_at]),
    io:format(File,"Public? : ~p~n",[Comment#comment.public]),
    io:format(File,"Attachments: ~p~n",[Comment#comment.attachments]),
    io:format(File,"Body: ~p~n",[Comment#comment.body]),
    io:format(File,"-----END OF COMMENT-----",[]),
    file:close(File),
    case Comment#comment.attachments of
	[] -> ok;
	Attachment ->
	    download_attachments(Attachment,N,1,DirName)
	end,
    add_comments(List,N+1,DirName).
    
download_attachments([],_CN,_AN,_DirName) ->
    ok;
download_attachments([Attachment|List],CommentNbr,AttachNbr,DirName) ->
    FileName=Attachment#attachment.file_name,
    Url=Attachment#attachment.content_url,
    Type=Attachment#attachment.content_type,
    Targetname=DirName++"C"++string:right(integer_to_list(CommentNbr), 4, $0)++"A"++string:right(integer_to_list(AttachNbr), 2, $0)++"_"++FileName,
    ticket_export_utils:request_download(Url,Targetname),
    download_attachments(List,CommentNbr,AttachNbr+1,DirName).



