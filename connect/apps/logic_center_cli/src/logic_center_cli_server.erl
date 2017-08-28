%% gen_server代码模板
% 遍历股票列表，
% 将所有的历史链接都检查是否己下载过，
% 如没下载则下载相应的页面提取数据，
% 如已下载则忽略，
% 如果是当季度链接，则也要下载提取数据


-module(logic_center_cli_server).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
% --------------------------------------------------------------------
-export([]).

% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% -record(state, {}).
-record(state, {socket, transport, data}).

% --------------------------------------------------------------------
% External API
% --------------------------------------------------------------------
% -export([doit/1]).

% doit(FromPid) ->
%     gen_server:cast(?MODULE, {doit, FromPid}).



% -export([start_goroutine/0, info/0, stop_goroutine/1, send_cast/2]).

% info() ->
%     gen_server:call(?MODULE, info).

% start_goroutine() ->
%     gen_server:call(?MODULE, start_goroutine).

% stop_goroutine(GoMBox) ->
%     gen_server:call(?MODULE, {stop_goroutine, GoMBox}).

% send_cast(GoMBox, Msg) ->
%     gen_server:cast(?MODULE, {send_cast, GoMBox, Msg}).

% get_gombox() ->
%     gen_server:call(?MODULE, get_gombox).

% start_link() ->
%     gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) ->
	case ranch_tcp:connect("127.0.0.1", 9999,[],3000) of
		{ok,Socket} ->
	        	ok = ranch_tcp:setopts(Socket, [{active, once}]),
			% {ok, Socket};
			Bin = <<"hello world!!">>,
			P1 = data_center_cli_package:package(Bin),
			ranch_tcp:send(Socket, P1),

			{ok, #state{socket=Socket, transport=ranch_tcp, data = <<>>} };
		{error,Reason} ->
			{stop,Reason}
	end.



% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------

% handle_call({doit, FromPid}, _From, State) ->
%     io:format("doit  !! ============== ~n~n"),

%     lists:foreach(fun(_I) ->
%         FromPid ! {from_doit, <<"haha">>}
%     end, lists:seq(1, 100)),

%     {reply, [], State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
% handle_cast({send_cast, GoMBox, Msg}, State) ->
%     io:format("send cast !! ============== ~n~n"),
%     % {ok, GoMBox} = application:get_env(go, go_mailbox),
%     % io:format("message ~p!! ============== ~n~n", [GoMBox]),
%     gen_server:cast(GoMBox, {Msg, self()}),
%     {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
% handle_info(_Info, State) ->
%     {noreply, State}.
handle_info({tcp, Socket, CurrentPackage}, State=#state{
		socket=Socket, transport=Transport, data=LastPackage}) -> 
		% when byte_size(Data) > 1 ->
	Transport:setopts(Socket, [{active, once}]),
	PackageBin = <<LastPackage/binary, CurrentPackage/binary>>,

	io:format("package ========== ~p~n ", [PackageBin]),

	case data_center_cli_package:unpackage(PackageBin) of
		{ok, waitmore} -> 
			io:format("wait more ===========~n~n"),
			{noreply, State#state{data = PackageBin}};
		{ok, RightPackage, NextPageckage} -> 
			% parse logic and reply here ========================
			% Transport:send(Socket, reverse_binary(RightPackage)),
			io:format("package here ========== ~p~n ", [RightPackage]),

			{noreply, State#state{data = NextPageckage}};			
		_ -> 
			{stop, stop_noreason,State}
	end;

	% Transport:send(Socket, reverse_binary(Data)),
	% {noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
	io:format("~p:~p  tcp closed  !!!!!! ~n~n", [?MODULE, ?LINE]),
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

% handle_info(Info, State) ->
%     % 接收来自go 发过来的异步消息
%     io:format("~nhandle info BBB!!============== ~n~p~n", [Info]),
%     {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% private functions

% insert_page(InfoKey, Link) ->
%     Html = go:http_get(Link),
%     Html1 = go:iconv(Html, 'gb2312', 'utf-8'),

%     % go_lib:file_put_contents("/web/1.html", Html1),

%     Data = [
%         {<<"info_key">>, InfoKey},
%         {<<"url">>, Link},
%         {<<"html_page">>, Html1}
%     ],

%     mysql:insert("sina_web_page", Data),

%     io:format("~p~n", [Link]).


