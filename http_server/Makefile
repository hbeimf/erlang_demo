all:
	rebar3 compile
	rebar3 release
	rebar3 tar

run:
	rebar3 shell --name http_server@127.0.0.1 --setcookie http_server_cookie 


# 请先修改app的版本号再执行make up 
up:
	rebar3 release
	rebar3 appup generate
	rebar3 relup
	rebar3 tar

