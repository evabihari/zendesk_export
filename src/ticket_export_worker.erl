%%%-------------------------------------------------------------------
%%% @author Eva <evabihari@Evas-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Eva
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2016 by Eva <evabihari@Evas-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(ticket_export_worker).

-include("../include/record.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tickets=[],
	        orgs=[],
		groups=[],
	        group_id,
	        ongoing_download=0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------    
handle_call({tickets_by_org,OrgId}, _From, State) ->
    FilterFun=fun(Ticket) ->
		      Ticket#tickets.organization_id == OrgId end,
    OrgTickets=lists:filter(FilterFun,State#state.tickets),
    {reply, OrgTickets, State};
handle_call({tickets_by_group,GroupId}, _From, State) ->
    FilterFun=fun(Ticket) ->
		      Ticket#tickets.group_id == GroupId end,
    OrgTickets=lists:filter(FilterFun,State#state.tickets),
    {reply, OrgTickets, State};
handle_call({tickets_by_org_and_group,OrgId,GroupId},_From, State) ->
    FilterFun=fun(Ticket) ->
		      ((Ticket#tickets.organization_id == OrgId) 
			  and
		      (Ticket#tickets.group_id == GroupId))
	      end,
    OrgTickets=lists:filter(FilterFun,State#state.tickets),
    {reply, OrgTickets, State};

handle_call(get_state,_From,State) ->
    {reply,State,State};
handle_call({org_id_by_name,OrgName},_From,State) ->
    Orgs=State#state.orgs,
    Result =case lists:keyfind(OrgName,#organization.name,Orgs) of
		false ->
		    {error,not_found};
		OrgRec ->
		    {ok,OrgRec#organization.id}
	    end,
    {reply,Result,State};
handle_call({org_name_by_id,OrgId},_From,State) ->
    Orgs=State#state.orgs,
    Result =case lists:keyfind(OrgId,#organization.id,Orgs) of
		false ->
		    {error,not_found};
		OrgRec ->
		    {ok,OrgRec#organization.name}
	    end,
    {reply,Result,State};
handle_call({group_id_by_name,GroupName},_From,State) ->
    Groups=State#state.groups,
    Result =case lists:keyfind(GroupName,#groups.name,Groups) of
		false ->
		    {error,not_found};
		GroupRec ->
		    {ok,GroupRec#groups.id}
	    end,
    {reply,Result,State}; 
handle_call({group_name_by_id,GroupId},_From,State) ->
    Groups=State#state.groups,
    Result =case lists:keyfind(GroupId,#groups.id,Groups) of
		false ->
		    {error,not_found};
		OrgRec ->
		    {ok,OrgRec#groups.name}
	    end,
    {reply,Result,State};   

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({tickets,Tickets}, State) ->
    {noreply, State#state{tickets=Tickets}};
handle_cast({orgs,Orgs}, State) ->
    {noreply, State#state{orgs=Orgs}};
handle_cast({groups,Groups}, State) ->
    {noreply, State#state{groups=Groups}};
handle_cast({group_id,{Group,Group_id}}, State) ->
    {noreply, State#state{group_id={Group,Group_id}}};
handle_cast({download_attachment,{Url,TargetFile}}, State) ->
    D=State#state.ongoing_download,
    spawn(ticket_export_utils, get_attachment, [Url,TargetFile]),
    {noreply, State#state{ongoing_download=D+1}};
handle_cast(download_ready,State) ->
    D=State#state.ongoing_download,
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    case D of
	1 -> io:format("Attachment download finished at ~p~p ~n",[{Year,Month,Day},{Hour,Min,Sec}]);
	N ->
	    io:format("Ongoing download ~p at ~p~p~n",[N-1,{Year,Month,Day},{Hour,Min,Sec}])
    end,
    {noreply, State#state{ongoing_download=D-1}};
handle_cast({tickets_by_org,OrgId}, State) ->
    ticket_export_utils:get_tickets_by_org(OrgId,State#state.tickets),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

