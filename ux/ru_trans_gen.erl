-module(ru_trans_gen).
-export([gen/0]).
-import(ux.string).
-define(TRDATA, "ru_translit.txt"). 
gen() ->
	{ok, InFd} = file:open(?TRDATA, [read]),
	io:setopts(InFd,[{encoding,utf8}]),
	do_gen(InFd, {}).
do_gen(InFd, OutFds) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds);
		{ok, Data} -> Str = unicode:characters_to_list(Data, utf8), io:format("~w", [ux.string:explode("\t", Str)]), do_gen(InFd, OutFds);
		eof -> ok
	end.
	
