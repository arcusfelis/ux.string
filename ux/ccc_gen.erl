%  Generator for Canonical_Combining_Class (ccc) values
%  Also, this data in UNIDATA
%  http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#Canonical Combining Classes
-module(ccc_gen).
-export([gen/0]).
-import(ux.string).
-define(TRDATA, "data/nfc.txt"). 
gen() ->
	{ok, InFd} = file:open(?TRDATA, [read]),
	{ok, CccOutFd} = file:open("string/ccc.hrl", [write]),
	io:setopts(InFd,[{encoding,utf8}]),
	do_gen(InFd, {CccOutFd}),
    io:format(CccOutFd, "ccc(_) -> 0. ~n", []).

do_gen(InFd, {CccOutFd} = OutFds) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds);
		{ok, Data} -> Str = Data, 
            % Parse ccc
            case ux.string:explode([":", ".."], ux.string:delete_types([cc], Str)) of
                [Code, Class] = Par when length(Code) == 4 -> io:format(CccOutFd, "ccc(16#~s) -> ~s; ~n", Par);
                [FromCode, ToCode, Class] = Par when length(FromCode) == 4 -> 
                    io:format(CccOutFd, "ccc(Code) when (16#~s=<Code) and (16#~s>=Code)  -> ~s; ~n", Par);
                _ -> other
            end, 
            do_gen(InFd, OutFds);
		eof -> ok
	end.
	
