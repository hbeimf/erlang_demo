-module(rr).

-compile(export_all).


demo() -> 
	Bin = <<"hello world!!">>,
	P1 = connection_package:package(Bin),
	{ok, U1, _ } = connection_package:unpackage(P1),
	<<_Len:16, Body/binary>> = U1,
	{P1, U1, Body}.


