%% @author Ery Lee<ery.lee@opengoss.com>

%% @copyright www.opengoss.com

%% @doc Errdb server
-module(errdb).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-include("errdb.hrl").

-export([start_link/1, exist/2, list/1, list/2, last/2, info/2, create/2, update/2, fetch/2, tune/2, move/2, delete/2, graph/1, graph/2]).

-export([async_create/2]).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {data_folder, graphs_folder}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Options], []).

%%--------------------------------------------------------------------
%% Function: create(rrdb, {Name, Args}) -> ok | {error, {Status, Reason}}
%% Description: create rrdb
%%--------------------------------------------------------------------
exist(rrdb, DbName) ->
    call({exist, rrdb, DbName});

exist(rrdfile, {DbName, FileName}) ->
    call({exist, rrdfile, {DbName, FileName}}).

create(rrdb, Name) ->
	call({create, rrdb, {Name, []}});

create(rrdfile, {DbName, FileName, Args}) ->
	call({create, rrdfile, {DbName, FileName, Args}});

create(datalog, {DnName, FileName, Datalog}) ->
	call({create, datalog, {DnName, FileName, Datalog}});

create(rrdfile_template, {TemplateName, Params}) ->
	call({create, rrdfile_template, {TemplateName, Params}});

create(rrdgraph_template, {TemplateName, Params}) ->
	call({create, rrdgraph_template, {TemplateName, Params}}).

async_create(datalog, {DnName, FileName, Datalog}) ->
	gen_server:cast({global, ?MODULE}, {async_create, datalog, {DnName, FileName, Datalog}}).

update(rrdfile_template, {TemplateName, Params}) ->
	call({update, rrdfile_template, {TemplateName, Params}});

update(rrdgraph_template, {TemplateName, Params}) ->
	call({update, rrdgraph_template, {TemplateName, Params}}).

move(rrdb, {SrcName, DstName}) ->
	call({move, rrdb, {SrcName, DstName}});

move(rrdfile, {DbName, SrcName, DstName}) ->
	call({move, rrdfile, {DbName, SrcName, DstName}}).

delete(rrdb, Name) ->
	call({delete, rrdb, Name});	

delete(rrdfile, {DbName, FileName}) ->
	call({delete, rrdfile, {DbName, FileName}});

delete(rrdfile_template, TemplateName) ->
	call({delete, rrdfile_template, TemplateName});

delete(rrdgraph_template, TemplateName) ->
	call({delete, rrdgraph_template, TemplateName}).

fetch(datalogs, {DbName, FieName, Params})->
	call({fetch, datalogs, {DbName, FieName, Params}}).

list(rrdbs) ->
	call({list, rrdbs});

list(rrdfile_templates) ->
	call({list, rrdfile_templates});

list(rrdgraph_templates) ->
	call({list, rrdgraph_templates}).

list(rrdfiles, DbName) ->
	call({list, rrdfiles, DbName}).

info(rrdfile, {DbName, FileName}) ->
	call({info, rrdfile, {DbName, FileName}}). 

last(rrdfile, {DbName, FileName}) ->
	call({last, rrdfile, {DbName, FileName}});

last(datalog, {DbName, FileName}) ->
	call({last, datalog, {DbName, FileName}}).

tune(rrdfile, {DbName, FileName, Params}) ->
	call({tune, rrdfile, {DbName, FileName, Params}}).

graph({DbName, FileName, Args}) ->
	call({graph, {DbName, FileName, Args}}).

graph(ImgName, {DbName, FileName, Args}) ->
	call({graph, {ImgName, DbName, FileName, Args}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Options]) ->
	ROOT = proplists:get_value(root, Options, "var/rrdb"),
	TemplatesDir = proplists:get_value(templates, Options),
	{ok, TemplateFiles} = file:list_dir(TemplatesDir),
	ets:new(template, [set, private, named_table]),
	lists:foreach(fun(TemplateFile) -> 
        case lists:suffix(".templates", TemplateFile) of
        true -> 
		  case file:consult(filename:join(TemplatesDir, TemplateFile)) of
			{ok, Terms} ->
				lists:foreach(fun(Term) -> ets:insert(template, Term) end, Terms);
			{error, Reason} ->
				?ERROR("Can't load template file ~p: ~p", [TemplateFile, Reason]),
				{stop, "Cannot load errdb template file: " ++ TemplateFile ++ ", reason: " ++ file:format_error(Reason)}
		  end;
        false ->
            pass
        end
	end, TemplateFiles),
	DataFolder = filename:join(ROOT, "data"),
	GraphsFolder = filename:join(ROOT, "graphs"),
    ?INFO("Errdb is started...[ok]", []),
    {ok, #state{data_folder = DataFolder, graphs_folder = GraphsFolder}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({exist, rrdb, DbName}, _From, State) ->
	Dir = filename:join([State#state.data_folder, DbName]),
    Reply = filelib:is_dir(Dir),
    {reply, Reply, State};

handle_call({exist, rrdfile, {DbName, FileName}}, _From, State) ->
    RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
    Reply = filelib:is_file(RRDFile),
    {reply, Reply, State};

handle_call({create, rrdb, {Name, _Args}}, _From, State) ->
	Dir = filename:join([State#state.data_folder, Name]),
	?DEBUG("create rrdb: ~p", [Dir]),
	Reply = case filelib:is_dir(Dir) of
		false ->
            filelib:ensure_dir(Dir),
            case file:make_dir(Dir) of
				ok -> 
                    GraphDir = filename:join(State#state.graphs_folder, Name),
					filelib:ensure_dir(GraphDir),
                    filelib:makd_dir(GraphDir),
					{ok, Name};
				{error, Reason} -> 
                    {error, {500, Reason}}
			end;
		true ->
			{error, {409, "Conflict: the rrdb is existed."}}
	end,
    {reply, Reply, State};

handle_call({create, rrdfile, {DbName, FileName, Args}}, _From, State) ->
    Dir = filename:join([State#state.data_folder, DbName]),
    case filelib:is_dir(Dir) of
    false -> 
        filelib:ensure_dir(Dir),
        file:make_dir(Dir),
        GraphDir = filename:join(State#state.graphs_folder, DbName),
        filelib:ensure_dir(GraphDir),
        file:make_dir(GraphDir);
    true -> 
        ok 
	end,
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	case filelib:is_file(RRDFile) of
	false ->
		Reply = handle_create_rrdfile(RRDFile, Args),
		{reply, Reply, State};
	true ->
		{reply, {error, {409, <<"Conflict: rrdfile is existed.">>}}, State}
	end;

handle_call({create, datalog, {DbName, FileName, Datalog}}, _From, State) ->
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	case filelib:is_file(RRDFile) of
	true ->
		Reply = handle_update_rrdfile(RRDFile, Datalog),
		{reply, Reply, State};
	false ->
		{reply, {error, {404, <<"rrdfile does not exist.">>}}, State}
	end;

handle_call({create, rrdfile_template, {TemplateName, Params}}, _From, State) ->
	?DEBUG("create rrdfile_template: ~p ~p", [TemplateName, Params]),
	%TODO: unsupported now
	{reply, {error, {500, <<"still not support to create rrdfile template.">>}}, State};

handle_call({create, rrdgraph_template, {TemplateName, Params}}, _From, State) ->
	?DEBUG("create rrdgraph_template: ~p ~p", [TemplateName, Params]),
	%TODO: unsupported now
	{reply, {error, {500, <<"still not support to create graph template.">>}}, State};

handle_call({update, rrdfile_template, {TemplateName, Params}}, _From, State) ->
	?DEBUG("update rrdfile_template: ~p ~p", [TemplateName, Params]),
	%TODO: unsupported now
	{reply, {error, {500, <<"still not support to update rrdfile template.">>}}, State};
	
handle_call({update, rrdgraph_template, {TemplateName, Params}}, _From, State) ->
	?DEBUG("update rrdgraph_template: ~p ~p", [TemplateName, Params]),
	%TODO: unsupported now
	{reply, {error, {500, <<"still not support to update rrdgraph template.">>}}, State};

handle_call({move, rrdb, {SrcName, DstName}}, _From, State) ->
	SrcDir = filename:join(State#state.data_folder, SrcName),
	DstDir = filename:join(State#state.data_folder, DstName),
	Reply = case filelib:is_dir(SrcDir) of
		true ->
			case filelib:is_dir(DstDir) of
				false ->
					case os:cmd("mv "++ SrcDir ++ " " ++ DstDir) of
						[] -> ok;
						Error -> {error, {500, Error}}
					end;
				true ->
					{error, {409, "The destination rrdb is existed."}}
			end;
		false ->
			{error, {404, "The source rrdb is not existed."}}
	end,
	{reply, Reply, State};

handle_call({move, rrdfile, {DbName, SrcName, DstName}}, _From, State) ->
	SrcFile = filename:join(State#state.data_folder, DbName, SrcName),
	DstFile = filename:join(State#state.data_folder, DbName, DstName),
	Reply = case filelib:is_file(SrcFile) of
		true ->
			case filelib:is_dir(DstFile) of
				false ->
					case file:rename(SrcFile, DstFile) of
						ok -> ok;
						{error, Reason} -> {error, {500, atom_to_list(Reason)}}
					end;
				true ->
					{error, {409, "The destination rrdfile is existed."}}
			end;
		false ->
			{error, {404, "The source rrdfile is not existed."}}
	end,
	{reply, Reply, State};

handle_call({delete, rrdb, DbName}, _From, State) ->
	DbDir = filename:join(State#state.data_folder, DbName),
	Reply = case file:del_dir(DbDir) of
		ok -> ok;
		{error, Reason} -> {error, {500, atom_to_list(Reason)}}
	end,
	{reply, Reply, State};

handle_call({delete, rrdfile, {DbName, FileName}}, _From, State) ->
	RRDFile = filename:join(State#state.data_folder, DbName, FileName),
	Reply = case filelib:is_file(RRDFile) of
		true -> 
			case file:delete(RRDFile) of
				ok -> ok;
				{error, Reason} -> {error, {500, atom_to_list(Reason)}} 
			end;
		false ->
			{error, {404, "The rrdfile is not existed"}}
	end,
	{reply, Reply, State};

handle_call({delete, rrdfile_template, TemplateName}, _From, State) ->
	?DEBUG("delete rrdfile template: ~p", [TemplateName]),
	{reply, "still not support to delete rrdfile template", State};

handle_call({delete, rrdgraph_template, TemplateName}, _From, State) ->
	?DEBUG("delete rrdgraph template: ~p", [TemplateName]),
	{reply, "still not support to delete rrdgraph template", State};

handle_call({info, rrdfile, {DbName, FileName}}, _From, State) ->
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	Reply = case erlrrd:info(RRDFile) of
		{ok, Resp} -> {ok, Resp};
		{error, Reason} -> {error, {500, Reason}}
	end,
	{reply, Reply, State};

handle_call({last, rrdfile, {DbName, FileName}}, _From, State) ->
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	Reply = case erlrrd:last(RRDFile) of
		{ok, Resp} -> {ok, Resp};
		{error, Reason} -> {error, {500, Reason}}
	end,
	{reply, Reply, State};

handle_call({last, datalog, {DbName, FileName}}, _From, State) ->
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	Reply = case erlrrd:lastupdate(RRDFile) of
		{ok, Resp} -> {ok, Resp};
		{error, Reason} -> {error, {500, Reason}}
	end,
	{reply, Reply, State};

handle_call({tune, rrdfile, {DbName, FileName, Params}}, _From, State) ->
	?DEBUG("tune ~p ~p ~p", [DbName, FileName, Params]),
	{reply, "still not support to tune rrdfile", State};

handle_call({list, rrdbs}, _From, State) ->
	Reply = case file:list_dir(State#state.data_folder) of
        {error, Reason} -> {error, {500, Reason}};
        {ok, RRDbs} -> {ok, RRDbs}
    end,
	{reply, Reply, State};

handle_call({list, rrdfiles, DbName}, _From, State) ->
	Reply = case file:list_dir(filename:join(State#state.data_folder, DbName)) of
        {error, Reason} -> {error, {500, Reason}};
        {ok, RRDbs} -> {ok, RRDbs}
    end,
	{reply, Reply, State};

handle_call({list, rrdfile_templates}, _From, State) ->
	Result = ets:foldl(fun({TemplateId, Templates}, Acc) -> 
		case lists:keysearch(create, 1, Templates) of
        {value, {create, Template}} ->
            [{TemplateId, Template} | Acc];
        false ->
            [{TemplateId, "no create template"} | Acc]
        end
	end, [], template),
	{reply, {ok, Result}, State};

handle_call({list, rrdgraph_templates}, _From, State) ->
	Result = ets:foldl(fun({TemplateId, Templates}, Acc) -> 
		case lists:keysearch(graph, 1, Templates) of
        {value, {graph, Template}} ->
            [{TemplateId, Template} | Acc];
        false ->
            [{TemplateId, "no graph template"} | Acc]
        end
	end, [], template),
	{reply, {ok, Result}, State};

handle_call({graph, {DbName, FileName, Args}}, _From, State) ->
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	ImgFile = filename:join([State#state.graphs_folder, DbName, uuid:new() ++ ".png"]),
	Reply = case filelib:is_file(RRDFile) of
	true ->
		case handle_graph_rrdfile(ImgFile, RRDFile, Args) of
			{ok, _Resp} -> 
				case file:read_file(ImgFile) of
					{ok, Binary} -> file:delete(ImgFile), {ok, Binary};
					{error, Reason} -> {error, {500, atom_to_list(Reason)}}
				end;
			{error, Reason} -> 
				{error, Reason}
		end;
	false ->
		{error, {404, "rrdfile does not exist."}}
	end,
	{reply, Reply, State};

handle_call({graph, {ImgName, DbName, FileName, Args}}, _From, State) ->
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	ImgFile = filename:join([State#state.graphs_folder, DbName, ImgName]),
	case filelib:is_file(RRDFile) of
	true ->
		Reply = handle_graph_rrdfile(ImgFile, RRDFile, Args),
		{reply, Reply, State};
	false ->
		{reply, {error, {404, "rrdfile does not exist."}}, State}
	end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({async_create, datalog, {DbName, FileName, Datalog}}, State) ->
	RRDFile = filename:join([State#state.data_folder, DbName, FileName]),
	case filelib:is_file(RRDFile) of
	true ->
		handle_update_rrdfile(RRDFile, Datalog),
		{noreply, State};
	false ->
        ?ERROR("rrdfile does not exist for db: ~p", [DbName]),
		{noreply, State}
	end;

handle_cast(_Msg, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(Req) ->
	gen_server:call({global, ?MODULE}, Req).

%cast(Msg) ->
%	gen_server:cast(?MODULE, Msg).
handle_create_rrdfile(RRDFile, Args) ->
	handle_template_action(create, RRDFile, Args).

handle_update_rrdfile(RRDFile, Datalog) ->
	handle_template_action(update, RRDFile, Datalog).

handle_graph_rrdfile(ImgFile, RRDFile, Args) ->
	handle_template_action(graph, RRDFile, [{imagefile, ImgFile} | Args]).

handle_template_action(Action, RRDFile, Args) ->
	case lists:keysearch(template, 1, Args) of
		{value, {template, TemplateId}} ->
			case ets:lookup(template, TemplateId) of
				[{_, Templates}] ->
					case lists:keysearch(Action, 1, Templates) of
						{value, {_, Template}} ->
							Cmd = parse_cmd(Template, [{rrdfile, RRDFile}|Args], []),
							%?INFO("~p", [Cmd]),
							case apply(erlrrd, Action, [Cmd]) of
								{ok, Resp} -> {ok, Resp};
								{error, Error} -> {error, {500, Error}}
							end;
						false ->
							{error, {500, "Cannot find create template: " ++ TemplateId}}
					end;
				[] ->
					{error, {500, "Cannot find template: " ++ TemplateId}}
			end;	
		false ->
			{error, {400, "No 'template' in params"}}
	end.

parse_cmd([H|T],Args,Acc) ->
	if 
		is_atom(H) -> 
			case lists:keysearch(H,1,Args) of
				{value,Value} ->
					parse_cmd(T,Args,[to_string(erlang:element(2,Value))|Acc]);
				false ->
					parse_cmd(T,Args,[atom_to_list(H)|Acc])
			end;
		true -> 
			parse_cmd(T, Args, [H|Acc])
	end;

parse_cmd([],_Args,Acc) -> lists:reverse(Acc).

to_string(Value) ->
    if
    	is_integer(Value) -> integer_to_list(Value);
		is_float(Value) -> float_to_list(Value);
        true -> Value
    end.         
