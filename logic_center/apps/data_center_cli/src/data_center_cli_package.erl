-module(data_center_cli_package).


-export([package/1, unpackage/1]).

unpackage(PackageBin) when erlang:byte_size(PackageBin) >= 2 ->
	io:format("parse package =========~n~n"),
	case parse_head(PackageBin) of
		{ok, PackageLen} ->	
			parse_body(PackageLen, PackageBin);
		Any -> 
			Any
	end;
unpackage(_) ->
	{ok, waitmore}. 

parse_head(<<PackageLen:16 ,_/binary>> ) ->
	io:format("parse head ======: ~p ~n~n", [PackageLen]), 
	{ok, PackageLen};
parse_head(_) ->
	error.

parse_body(PackageLen, _ ) when PackageLen > 9000 ->
	error; 
parse_body(PackageLen, PackageBin) ->
	io:format("parse body -----------~n~n"),
	case PackageBin of 
		<<RightPackage:PackageLen/binary,NextPageckage/binary>> ->
			{ok, RightPackage , NextPageckage};
		_ -> {ok, waitmore}
	end.

package(DataBin) ->
	Len = byte_size(DataBin)+2,
	<<Len:16, DataBin/binary>>.