%  Generator for Canonical_Combining_Class (ccc) values

-module(norm_props_gen).
-export([gen/0]).
-import(ux.string).
-define(TRDATA, "data/DerivedNormalizationProps.txt"). 
gen() ->
	{ok, InFd} = file:open(?TRDATA, [read]),
	{ok, NfcQcOutFd}  = file:open("string/nfc_qc.hrl",  [write]),
	{ok, NfdQcOutFd}  = file:open("string/nfd_qc.hrl",  [write]),
	{ok, NfkcQcOutFd} = file:open("string/nfkc_qc.hrl", [write]),
	{ok, NfkdQcOutFd} = file:open("string/nfkd_qc.hrl", [write]),
    
	do_gen(InFd, {NfcQcOutFd, NfdQcOutFd, NfkcQcOutFd, NfkdQcOutFd}),

    io:format(NfcQcOutFd,  "nfc_qc(_) -> y. ~n",  []),
    io:format(NfdQcOutFd,  "nfd_qc(_) -> y. ~n",  []),
    io:format(NfkcQcOutFd, "nfkc_qc(_) -> y. ~n", []),
    io:format(NfkdQcOutFd, "nfkd_qc(_) -> y. ~n", []),
    ok.

parse_code(Code) -> case ux.string:explode([".."], Code) of
    [From, To]  -> {From, To};
    [Code]      -> {Code}
    end.

sel_fun_name(Name) -> string:to_lower(Name).

sel_fd("NFC_QC",  {_C, _D, _CK, _DK}) ->  _C;
sel_fd("NFD_QC",  {_C, _D, _CK, _DK}) ->  _D;
sel_fd("NFKC_QC", {_C, _D, _CK, _DK}) -> _CK;
sel_fd("NFKD_QC", {_C, _D, _CK, _DK}) -> _DK.

do_gen(InFd, OutFds) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds);
		{ok, Data} -> Str = Data, 
            % Parse ccc
            case ux.string:explode([";", "#"], ux.string:delete_types([cc, zs], Str)) of
                [Code, Form, Props, Comment] when ((Form=="NFC_QC") 
                                               or  (Form=="NFD_QC")
                                               or  (Form=="NFKC_QC")
                                               or  (Form=="NFKD_QC"))
                                             and   ((Props=="Y")
                                               or   (Props=="N")
                                               or   (Props=="M")) ->
                    case parse_code(Code) of
                        {Char    } -> io:format(sel_fd(Form, OutFds), 
                                        "~s(16#~s) -> ~s; ~n", 
                                        [sel_fun_name(Form), Char, string:to_lower(Props)]);
                        {From, To} -> io:format(sel_fd(Form, OutFds), 
                                        "~s(C) when (C>=16#~s) and (C=<16#~s) -> ~s; ~n", 
                                        [sel_fun_name(Form), From, To, string:to_lower(Props)]);
                        _          -> oops
                    end;
                _ -> other
            end, 
            do_gen(InFd, OutFds);
		eof -> ok
	end.
	
