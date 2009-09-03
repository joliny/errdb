%%%----------------------------------------------------------------------
%%% File    : errdb_client.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : errdb client
%%% Created : 28 Aug 2009
%%% Vsn:	: 1.0
%%%----------------------------------------------------------------------
-module(errdb_client).

-author('ery.lee@gmail.com').

-compile(export_all).

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
	cast({async_create, datalog, {DnName, FileName, Datalog}}).

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

cast(Msg) ->
	gen_server:cast({global, errdb}, Msg).

call(Req) ->
	gen_server:call({global, errdb}, Req).
