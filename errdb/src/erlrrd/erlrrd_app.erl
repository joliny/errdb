-module(erlrrd_app).

-export([start/0, stop/0]).

-behavior(application).
-export([start/2, stop/1]).

start() -> 
  application:start(erlrrd).

stop() -> 
  application:stop(erlrrd).

start(_Type, _Args) -> 
   Env = application:get_all_env(),
   register(?MODULE, self()),
   erlrrd_sup:start_link(Env).

stop(_State) -> 
  ok. 

