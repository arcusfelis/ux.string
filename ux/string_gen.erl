-module(string_gen).
-export([gen/0, write/1]).
-define(UCDATA, "UnicodeData.txt"). % http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html
-define(UC_UC_OUT, "string/char_to_upper.hrl").
-define(UC_LC_OUT, "string/char_to_lower.hrl").

-define(UC_IU_OUT, "string/is_upper.hrl").
-define(UC_IL_OUT, "string/is_lower.hrl").

-define(UC_CT_OUT, "string/char_type.hrl").
-define(UC_CO_OUT, "string/is_compat.hrl").
-define(UC_DM_OUT, "string/decomp.hrl").
-define(UC_CM_OUT, "string/comp.hrl").
write({CharToLowerFd, CharToUpperFd, IsLowerFd, IsUpperFd, 
    CharTypeFd, IsCompatFd, DecompMapFd, CompMapFd} = Fds) ->
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
	{ok, UpperOutFd} = file:open(?UC_UC_OUT, [write]),
	{ok, LowerOutFd} = file:open(?UC_LC_OUT, [write]),
	{ok, IsUpperOutFd} = file:open(?UC_IU_OUT, [write]),
	{ok, IsLowerOutFd} = file:open(?UC_IL_OUT, [write]),
	{ok, CharTypeOutFd} = file:open(?UC_CT_OUT, [write]),
	{ok, IsCompatFd} = file:open(?UC_CO_OUT, [write]),
	{ok, DecompMapFd} = file:open(?UC_DM_OUT, [write]),
	{ok,   CompMapFd} = file:open(?UC_CM_OUT, [write]),
	Pid = spawn(?MODULE, write, [{LowerOutFd, UpperOutFd, IsLowerOutFd, IsUpperOutFd, CharTypeOutFd, 
        IsCompatFd, DecompMapFd, CompMapFd}]),
	do_gen_head(Pid, TopChars),
	do_gen(InFd, Pid, TopChars, []),
	Pid ! {char_to_upper, "char_to_upper(C) -> C.", []},
	Pid ! {is_upper, "is_upper(_) -> false.", []},
	Pid ! {is_lower, "is_lower(_) -> false.", []},
	Pid ! {char_to_lower, "char_to_lower(C) -> C.", []},
	Pid ! {char_type, "char_type(_) -> other.", []},
	Pid ! {is_compat, "is_compat(_) -> false.", []},
	Pid ! {decomp, "decomp(_) -> [].", []},
	Pid ! {comp, "comp(_) -> false.", []},
    ok.

do_gen_head(_, []) -> ok;
do_gen_head(Pid, [H|T]) ->
        Pid ! {char_to_upper, "char_to_upper(16#~.16b) -> 16#~.16b;~n", [H, ux.string:char_to_upper(H)]},
        Pid ! {char_to_lower, "char_to_lower(16#~.16b) -> 16#~.16b;~n", [H, ux.string:char_to_lower(H)]},
        Pid ! {is_upper, "is_upper(16#~.16b) -> ~w;~n", [H, ux.string:is_upper(H)]},
        Pid ! {is_lower, "is_lower(16#~.16b) -> ~w;~n", [H, ux.string:is_lower(H)]},
        Pid ! {char_type, "char_type(16#~.16b) -> ~w;~n", [H, ux.string:char_type(H)]},

	do_gen_head(Pid, T).

from_hex([$<|Str]) -> 
        SubStr = string:sub_string(Str, string:chr(Str, $>)+1),
        from_hex(SubStr);
from_hex(Str) -> 
        Data = string:tokens(Str, " "),
                "[" 
                 ++ string:join(
                        lists:map(fun(X) -> "16#" ++ X end, Data),
                        ",") 
                 ++ "]".

do_gen(InFd, Pid, TopChars, CompList) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, Pid, TopChars, CompList);
		{ok, Data} ->
			Tokens = ux.string:explode(";", Data)++[""],
			Code = lists:nth(1, Tokens),
			Uppercase = lists:nth(13, Tokens),
			Lowercase = lists:nth(14, Tokens),
			Comment = lists:nth(2, Tokens),
			Abbr = lists:nth(3, Tokens),
			DecompMap = lists:nth(6, Tokens),
            
	{ok, [Int], []} = io_lib:fread("~16u", Code),
    case DecompMap of
        [] -> NewCompList = CompList;
        _  -> 
        
            % Save compability flag
            case DecompMap of 
                [$<|_]  -> Pid ! {is_compat,
                            "is_compat(16#~s) -> true; ~n",
                            [Code]},
                            Compat = true;
                _ ->        Compat = false
            end,

            Dec = from_hex(DecompMap),
            % Add decomposition mapping
            Pid ! {decomp,
            "decomp(16#~s) -> ~s; ~n",
            [Code, Dec]},

            % Add composition mapping 
            case ((not lists:member(Dec, CompList)) 
              and (false == Compat) 
              and (false == ux.string:is_comp_excl(Int))) of
                true -> Pid ! {comp,
                        "comp(~s) -> 16#~s; ~n",
                        [Dec, Code]},
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
							"char_to_lower(16#~s) -> 16#~s; %~s ~n", 
							[Code, Lowercase, Comment]}
					end,
					Pid ! {is_upper,
						"is_upper(16#~s) -> true; ~n", 
						[Code]};
				"Ll" -> % lowercase -> uppercase
					case Uppercase of
						[] -> skip;
						_ -> Pid ! {char_to_upper,
							"char_to_upper(16#~s) -> 16#~s; %~s ~n", 
							[Code, Uppercase, Comment]}
					end,
					Pid ! {is_lower,
						"is_lower(16#~s) -> true; ~n", 
						[Code]};
				_ -> ok
			end,

            % Save char type
			case {Abbr, Code} of
				{[], _} -> ok;
				{_, []} -> ok;
				{_,  _} -> Pid ! {char_type,
						"char_type(16#~s) -> ~s; ~n", 
						[Code, string:to_lower(Abbr)]}
			end
		end,
			do_gen(InFd, Pid, TopChars, NewCompList);
		eof ->
			ok
	end.
