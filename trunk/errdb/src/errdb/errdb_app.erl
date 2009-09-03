%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Errdb application.

-module(errdb_app).

-export([start/0, stop/0]).
-behavior(application).
-export([start/2, stop/1]).

%%@spec start() -> ok
%%@doc Start the errdb server
start() -> 
	errdb_deps:ensure(),
    ensure_started(crypto),
    application:start(erlrrd),
	application:start(errdb).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%%@spec stop() -> ok
%%@doc Stop the errdb server
stop() -> 
    Res = application:stop(errdb),
    application:stop(erlrrd),
    application:stop(crypto),
    Res.

start(_Type, _Args) ->
    {ok, LogLevel} = application:get_env(log_level),
    {ok, LogPath} = application:get_env(log_path),
	elog:init(LogLevel, LogPath),
	errdb_sup:start_link().

stop(_State) ->
	ok.

