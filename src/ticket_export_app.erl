-module(ticket_export_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ticket_export_sup:start_link().
stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    ok = application:start(ticket_export),
    ?assertNot(undefined == whereis(ticket_export_sup)).

-endif.
