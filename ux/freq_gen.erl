-module(freq_gen).
-import(ux.string).
-export([gen/0, do_gen/1, do_freq/1, do_regex/1]).
-record(do_gen_in, {count = 200, to, urls}).
-record(do_freq_in, {data, fd}).
-record(do_regex_in, {re, to, mod}).
-define(UC_FREQ_OUT, "string/freq_dict.erl").

gen() ->
	application:start(inets),
	{ok, FreqOutFd} = file:open(?UC_FREQ_OUT, [write]),
	Pid = spawn(?MODULE, do_freq, [#do_freq_in{ data=dict:new(), fd=FreqOutFd }]),
	Mod = [unicode,global,{capture,[2],list},dotall,ungreedy],
	RE = "<a\s[^>]*href=(\"??)(http://[^\" >]*?)\\1[^>]*>(.*)<\/a>",
	Pid1 = spawn(?MODULE, do_regex, [#do_regex_in{ re=RE, mod=Mod, to=false }]),   
	Pid2 = spawn(?MODULE, do_gen, [#do_gen_in{ to=[Pid, Pid1], urls=dict:new() }]),
	Pid1 ! {to, [Pid2]},
	Pid2.  

%do_gen(Par) when Par#do_gen_in.count==0 -> send_all(Par#do_gen_in.to, [save]), do_gen(Par);
do_gen(Par) ->
	receive
	{test, Url} -> 
		io:format("~s", [Url]),
		do_gen(Par);
	{get, Url} when Par#do_gen_in.count>0 -> 
		io:format("~s~n", [Url]),
		case dict:is_key(Url, Par#do_gen_in.urls) of 
			false -> http:request(get, {Url, []}, [], [{sync, false}]),
				do_gen(Par#do_gen_in{ urls=dict:store(Url, true, Par#do_gen_in.urls) 
							, count=Par#do_gen_in.count-1});
			true -> do_gen(Par)
		end;
	{http, {ReqestId, 
		{{HttpVer, 200, Msg}, Headers, Body} = Result}} ->
		Str = unicode:characters_to_list(Body),
		%io:format("~w", [Str]),
		send_all(Par#do_gen_in.to, [{Str}]),
		io:format("OK", []),
		do_gen(Par#do_gen_in{  });
	save -> send_all(Par#do_gen_in.to, [save]), do_gen(Par);
	_    -> do_gen(Par) 
	end.

send_all([], _) -> ok;
send_all([Dest|List], Mess) -> send_mess(Dest, Mess), send_all(List, Mess).

send_mess(_, []) -> ok;
send_mess(Dest, [Mess|List]) -> Dest ! Mess, send_mess(Dest, List).

do_freq(Par) ->
	receive
		save   -> io:format("~w", [dict:to_list(Par#do_freq_in.data)]),
			do_freq(Par);
		{Str}  when is_list(Str) ->
			do_freq(Par#do_freq_in{
				data = dict:merge(
					fun(Key, Val1, Val2) -> Val1+Val2 end
				,	Par#do_freq_in.data
				,	ux.string:freq(Str) 	
				)
			});
		_ -> do_freq(Par)
	end.

do_regex(Par) ->
	receive
		{to, Pid} ->	
			do_regex(Par#do_regex_in{ to=Pid });
		{Str} when is_list(Str) -> 
			case re:urun(Str, 
				%"<a\s[^>]*href=(\"??)([^\" >]*?)\\1[^>]*>(.*)<\/a>",
				Par#do_regex_in.re, 
				Par#do_regex_in.mod) of
				nomatch -> ok;
				{match, Arr} -> send_all(Par#do_regex_in.to
			 			,	lists:map(fun([X]) -> {get, X} end, Arr))
						%io:format("~w", [lists:map(fun([X]) -> {test, X} end, Arr)])

			end,
			do_regex(Par);
		_ ->	do_regex(Par)
	end.

to_file(Dict, OutFd) -> to_file(Dict, OutFd, sum(Dict, 0)).
to_file([], OutFd, _) ->  io:format(OutFd, "freq_dict(_) -> na; ~n", []).
to_file([H|T], OutFd, Sum) -> save(OutFd, H, Sum), to_file(T, OutFd, Sum).
save(OutFd, {Char, Count}, Sum) -> io:format(OutFd, "freq_dict(~w) -> ~w; ~n", [Char, Count/Sum]).

sum([], Sum) -> Sum;
sum([{Char|Count}|T], Sum) -> sum(T, Count+Sum).
