-module(send).
-compile(export_all).

test() -> 
	Txt = <<"hello world">>,
	{ok, Pid} = wsc_cc:start_link(),
	% Pid ! {binary, <<Bin/binary,Bin/binary,Bin/binary>>},
	Pid ! {text, Txt},

	ok.




test_bin() -> 
	Bin = create_package(),
	% binary(PackageBinary).
	{ok, Pid} = wsc_cc:start_link(),
	% Pid ! {binary, <<Bin/binary,Bin/binary,Bin/binary>>},
	Pid ! {binary, Bin},

	ok.
			

create_package() -> 
	<<"hello world">>.

