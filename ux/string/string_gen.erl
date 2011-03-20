-module(string_gen).
-export([gen/0, write/1, map/1]).
-include("config.hrl").
-define(UCDATA, ?UNIDATA_DIRECTORY ++ "UnicodeData.txt"). % http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html
-define(UC_UC_OUT, ?UX_STRING_INC ++ "char_to_upper.hrl").
-define(UC_LC_OUT, ?UX_STRING_INC ++ "char_to_lower.hrl").

-define(UC_IU_OUT, ?UX_STRING_INC ++ "is_upper.hrl").
-define(UC_IL_OUT, ?UX_STRING_INC ++ "is_lower.hrl").

-define(UC_CT_OUT, ?UX_STRING_INC ++ "char_type.hrl").
-define(UC_CO_OUT, ?UX_STRING_INC ++ "is_compat.hrl").
-define(UC_DM_OUT, ?UX_STRING_INC ++ "decomp.hrl").
-define(UC_CM_OUT, ?UX_STRING_INC ++ "comp.hrl").
-define(UC_CC_OUT, ?UX_STRING_INC ++ "char_comment.hrl").
-define(UC_CL_OUT, ?UX_STRING_INC ++ "ccc.hrl").

map(Pid) ->
    receive
    {Type, [Code, Value]} ->
        Last = get({last, Type}),       
        From = get({from, Type}),       
        OldV = get({val , Type}),       
        if
            Last == undefined ->
                put({from, Type}, Code),
                put({last, Type}, Code),
                put({val , Type}, Value);

            ((Last+1) == Code) and (Value == OldV) ->
                put({last, Type}, Code);

            true -> 
                put({from, Type}, Code),
                put({last, Type}, Code),
                put({val , Type}, Value),

                if
                   From == Last ->               
                        Pid ! {Type, "~w(~w) -> ~w;~n", 
                            [Type, From, OldV]}; 

                   true ->
                        Pid ! {Type, "~w(C) when (C>=~w) and (C=<~w) -> ~w;~n", 
                            [Type, From, Last, OldV]}
                end
        end,        
                 
        map(Pid);

        {flush, Type} ->
            Last = get({last, Type}),       
            From = get({from, Type}),       
            OldV = get({val , Type}),       

            if
               From == Last ->               
                    Pid ! {Type, "~w(~w) -> ~w;~n", 
                        [Type, From, OldV]}; 

               true ->
                    Pid ! {Type, "~w(C) when (C>=~w) and (C=<~w) -> ~w;~n", 
                        [Type, From, Last, OldV]}
            end,

            map(Pid);
       Data -> Pid ! Data, map(Pid)
    end.

write({CharToLowerFd, CharToUpperFd, IsLowerFd, IsUpperFd, 
    CharTypeFd, IsCompatFd, DecompMapFd, CompMapFd, CommentFd, CccFd} = Fds) ->
	receive
	{Id, Format, Par} ->
		case Id of	
			char_to_upper   -> Fd = CharToUpperFd;
			char_to_lower   -> Fd = CharToLowerFd;
			is_lower        -> Fd = IsLowerFd;
			is_upper        -> Fd = IsUpperFd;
			char_type       -> Fd = CharTypeFd;
            is_compat       -> Fd = IsCompatFd;
            decomp          -> Fd = DecompMapFd;
              comp          -> Fd =   CompMapFd;
            char_comment    -> Fd = CommentFd;
            ccc             -> Fd = CccFd;
			_ -> Fd = false
		end,
		case Fd of
			false -> ok;
			_ -> io:format(Fd, Format, Par)
		end,
	write(Fds)
	end.
	
gen() -> 
TopChars = lists:sort(fun(X1,X2) -> ux.string:freq_dict(X1)>ux.string:freq_dict(X2) end, 
			lists:filter(fun(X) -> ux.string:freq_dict(X)>0.0001 end, 
					lists:seq(1,65353, 1))),


	{ok, InFd} = file:open(?UCDATA, [read, raw]),
	{ok, UpperOutFd}    = file:open(?UC_UC_OUT, [write]),
	{ok, LowerOutFd}    = file:open(?UC_LC_OUT, [write]),
	{ok, IsUpperOutFd}  = file:open(?UC_IU_OUT, [write]),
	{ok, IsLowerOutFd}  = file:open(?UC_IL_OUT, [write]),
	{ok, CharTypeOutFd} = file:open(?UC_CT_OUT, [write]),
	{ok, IsCompatFd}    = file:open(?UC_CO_OUT, [write]),
	{ok, DecompMapFd}   = file:open(?UC_DM_OUT, [write]),
	{ok,   CompMapFd}   = file:open(?UC_CM_OUT, [write]),
	{ok, CommentFd}     = file:open(?UC_CC_OUT, [write]),
	{ok, CccFd}         = file:open(?UC_CL_OUT, [write]),
	Pid = spawn(?MODULE, write, [{LowerOutFd, UpperOutFd, IsLowerOutFd, IsUpperOutFd, CharTypeOutFd, 
        IsCompatFd, DecompMapFd, CompMapFd, CommentFd, CccFd}]),
    MapPid = spawn(?MODULE, map, [Pid]),
	%do_gen_head(Pid, TopChars),
	%do_gen(InFd, Pid, TopChars, [], MapPid),
	do_gen(InFd, Pid, [], [], MapPid),
	MapPid ! {flush, is_lower},
	MapPid ! {flush, is_upper},
	MapPid ! {flush, char_type},
	MapPid ! {flush, is_compat},
	MapPid ! {flush, ccc},
	Pid    ! {char_to_upper, "char_to_upper(C) -> C.", []},
	MapPid ! {is_upper, "is_upper(_) -> false.", []},
	MapPid ! {is_lower, "is_lower(_) -> false.", []},
	Pid    ! {char_to_lower, "char_to_lower(C) -> C.", []},
	MapPid ! {char_type, "char_type(_) -> other.", []},
	MapPid ! {is_compat, "is_compat(_) -> false.", []},
	Pid    ! {decomp, "decomp(_) -> [].", []},
	Pid    ! {comp, "comp(_, _) -> false.", []},
	Pid    ! {char_comment, "char_comment(_) -> [].", []},
	MapPid ! {ccc, "ccc(_) -> 0.", []},
    ok.

do_gen_head(_, []) -> ok;
do_gen_head(Pid, [H|T]) ->
        Pid ! {char_to_upper, "char_to_upper(~w) -> ~w;~n", [H, ux.string:char_to_upper(H)]},
        Pid ! {char_to_lower, "char_to_lower(~w) -> ~w;~n", [H, ux.string:char_to_lower(H)]},
        Pid ! {is_upper, "is_upper(~w) -> ~w;~n", [H, ux.string:is_upper(H)]},
        Pid ! {is_lower, "is_lower(~w) -> ~w;~n", [H, ux.string:is_lower(H)]},
        Pid ! {char_type, "char_type(~w) -> ~w;~n", [H, ux.string:char_type(H)]},

	do_gen_head(Pid, T).

from_hex([$<|Str]) -> 
        SubStr = string:sub_string(Str, string:chr(Str, $>)+1),
        from_hex(SubStr);
from_hex(Str) -> 
        lists:map(fun hex_to_int/1,
                    string:tokens(Str, " ")).

hex_to_int(Code) ->
	{ok, [Int], []} = io_lib:fread("~16u", Code),
    Int.

do_gen(InFd, Pid, TopChars, CompList, MapPid) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, Pid, TopChars, CompList, MapPid);
		{ok, Data} ->
			Tokens = ux.string:explode(";", Data)++[""],
			Code = lists:nth(1, Tokens),
			Uppercase = lists:nth(13, Tokens),
			Lowercase = lists:nth(14, Tokens),
			Comment = lists:nth(2, Tokens),
			Abbr = lists:nth(3, Tokens),
			DecompMap = lists:nth(6, Tokens),
			Ccc = lists:nth(4, Tokens),
            Int = hex_to_int(Code),
             
            Pid ! {char_comment,
			"char_comment(~w) -> \"~s\"; ~n", 
			[Int, Comment]},

            % Canonical composition classes
            case string:to_integer(ux.string:filter_types([nd], Ccc)) of
            {error, no_integer} -> ok;
            {0, []}      -> ok;
            {CccInt, []} -> MapPid ! {ccc,
			    [Int, CccInt]}
            end,

    case DecompMap of
        [] -> NewCompList = CompList;
        _  -> 
        
            % Save compability flag
            case DecompMap of 
                [$<|_]  -> MapPid ! {is_compat,
                            [Int, true]},
                            Compat = true;
                _ ->        Compat = false
            end,

            Dec = from_hex(DecompMap), 
            % Add decomposition mapping
            Pid ! {decomp,
            "decomp(~w) -> ~w; ~n",
            [Int, Dec]},

            % Add composition mapping 
            case ((not lists:member(Dec, CompList)) 
              and (false == Compat) 
              and (false == ux.string:is_comp_excl(Int))) of
                true -> case Dec of % skip one char mapping
                                    [D1,D2] -> Pid ! {comp,
                                                      "comp(~w, ~w) -> ~w; ~n",
                                                       [D1, D2, Int]};
                                    _           -> excluding
                             end,
                        NewCompList = [Dec|CompList];
                false -> NewCompList = CompList, excluding
            end,
            ok 
    end,

    % Add casing mapping
	case lists:member(Int, TopChars) of
		true -> ok; % was early added
		false -> 
			case Abbr of
				"Lu" -> % uppercase -> lowercase
					case Lowercase of
						[] -> skip;
						_  -> Pid ! {char_to_lower,
							"char_to_lower(~w) -> ~w; %~s ~n", 
							[Int, hex_to_int(Lowercase), Comment]}
					end,
					MapPid ! {is_upper,
						[Int, true]};
				"Ll" -> % lowercase -> uppercase
					case Uppercase of
						[] -> skip;
						_ -> Pid ! {char_to_upper,
							"char_to_upper(~w) -> ~w; %~s ~n", 
							[Int, hex_to_int(Uppercase), Comment]}
					end,
					MapPid ! {is_lower,
						[Int, true]};
				_ -> ok
			end,

            % Save char type
			case {Abbr, Code} of
				{[], _} -> ok;
				{_, []} -> ok;
				{_,  _} -> MapPid ! {char_type,
						[Int, erlang:list_to_atom(string:to_lower(Abbr))]}
			end
		end,
			do_gen(InFd, Pid, TopChars, NewCompList, MapPid);
		eof ->
			ok
	end.
