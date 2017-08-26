-module(connection_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 5000).

-record(state, {socket, transport, data}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
%init([]) -> {ok, undefined}.

init({Ref, Socket, Transport, _Opts = []}) ->
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
	gen_server:enter_loop(?MODULE, [],
		#state{socket=Socket, transport=Transport, data=<<>>},
		?TIMEOUT).

handle_info({tcp, Socket, CurrentPackage}, State=#state{
		socket=Socket, transport=Transport, data=LastPackage})
		% when byte_size(Data) > 1 ->
	Transport:setopts(Socket, [{active, once}]),
	PackageBin = <<LastPackage/binary, CurrentPackage/binary>>,

	case parse_package(PackageBin) of
		{ok, waitmore} -> 
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
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

% reverse_binary(B) when is_binary(B) ->
% 	[list_to_binary(lists:reverse(binary_to_list(
% 		binary:part(B, {0, byte_size(B)-2})
% 	))), "\r\n"].


parse_package(PackageBin) when erlang:byte_size(PackageBin) >= 2 ->
	case parse_head(PackageBin) of
		{ok, PackageLen} ->	
			parse_body(PackageLen, PackageBin);
		_ ->	
			{ok, waitmore}
	end.
parse_package(_) ->
	{ok, waitmore}. 

parse_head(<<PackageLen:16 ,_/binary>> )
	{ok, PackageLen};
parse_head(_) ->
	error.

parse_body(PackageLen, PackageBin) ->
	case PackageBin of 
		<<Package:PackageLen/binary,LefBin/binary>> ->
			{ok, RightPackage , NextPageckage};
		_ -> {ok, waitmore}
	end.