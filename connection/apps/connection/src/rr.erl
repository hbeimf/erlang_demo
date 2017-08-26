-module(rr).

-compile(export_all).


demo() -> 
	Bin = <<"hello world!!">>,
	P1 = connection_package:package(Bin),
	U1 = connection_package:unpackage(P1),
	{P1, U1}.


