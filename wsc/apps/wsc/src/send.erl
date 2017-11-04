-module(send).
-compile(export_all).

test() -> 
	Bin = create_package(),
	% binary(PackageBinary).
	{ok, Pid} = wsc_cc:start_link(),
	% Pid ! {binary, <<Bin/binary,Bin/binary,Bin/binary>>},
	Pid ! {binary, Bin},

	ok.
			

create_package() -> 
	<<"hello world">>.

