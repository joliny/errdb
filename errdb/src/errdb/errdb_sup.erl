%% @author Ery Lee<ery.lee@opengoss.com>
%% @copyright www.opengoss.com

%% @doc Supervisor for the errdb application.

-module(errdb_sup).
-author('Ery Lee<ery.lee@opengoss.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	%% Httpd config
	{ok, HttpdConf} = case application:get_env(httpd) of
		{ok, Conf} ->
			NewConf = lists:keymerge(1, Conf, [{ip, mochiweb(ip)}, {port, mochiweb(port)}, {docroot, errdb_deps:local_path(["priv", "www"])}]),
			{ok, NewConf};
		false ->
			{ok, [{ip, mochiweb(ip)}, {port, mochiweb(port)}, {docroot, errdb_deps:local_path(["priv", "www"])}]}
	end,
	%% Httpd 
    Httpd = {errdb_httpd, {errdb_httpd, start, [HttpdConf]},
           permanent, 5000, worker, dynamic},

	%% Errdb
	{ok, Options} = application:get_env(rrdb),
	Errdb = {errdb, {errdb, start_link, [Options]},
		permanent, 5000, worker, dynamic},
    {ok, {{one_for_one, 10, 10}, [Errdb, Httpd]}}.

mochiweb(ip) ->
	case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end;   

mochiweb(port) ->
	case os:getenv("MOCHIWEB_PORT") of false -> 8000; Any -> Any end;

mochiweb(docroot) ->
	["priv", "www"].

