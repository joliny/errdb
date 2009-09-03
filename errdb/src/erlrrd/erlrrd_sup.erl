-module(erlrrd_sup).

-export([start_link/1]).

-behavior(supervisor).

-export([init/1]).

%% @spec start_link(Env) ->  Result
%%   Env = list()
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link(Env) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Env).

init(Args) ->  
	RRDToolCmd = proplists:get_value(rrdtool_cmd, Args, "rrdtool -"),
	RRDToolPool = proplists:get_value(rrdtool_pool, Args, 8),
	{ok, {
		{one_for_one, 5, 10},
		[begin
			 Id = list_to_atom("erlrrd_" ++ integer_to_list(Which)),
			 {Id, {erlrrd, start_link, [Id, RRDToolCmd]},
			  permanent, 100, worker, [erlrrd]}
		 end
		 || Which <- lists:seq(1, RRDToolPool)]
	    }
	}.
