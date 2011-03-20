-module(comp_exclusions_gen).
-include("config.hrl").
-export([gen/0]).
-import(ux.string).
-define(TRDATA, ?UNIDATA_DIRECTIRY ++ "CompositionExclusions.txt"). 
gen() ->
	{ok, InFd}  = file:open(?TRDATA, [read]),
	{ok, OutFd} = file:open(?UX_STRING_INC ++ "is_comp_excl.hrl", [write]),
	do_gen(InFd, {OutFd}),
    io:format(OutFd, "is_comp_excl(_) -> false. ~n", []),
    ok.

do_gen(InFd, {OutFd} = OutFds) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds);
		{ok, Data} -> 
            case ux.string:explode(["#"], ux.string:delete_types([cc], Data)) of
                []       -> skip;
                [[]|_]   -> skip;
                [Char|_] ->
                    io:format(OutFd, "is_comp_excl(16#~s) -> true; ~n", 
                        [ux.string:delete_types([zs], Char)])
            end,
            do_gen(InFd, OutFds);
		eof -> ok
	end.
	
