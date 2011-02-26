-module(string_gen).
-export([gen/0, write/1]).
-define(UCDATA, "UnicodeData.txt"). % http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html
-define(UC_UC_OUT, "string/char_to_upper.hrl").
-define(UC_LC_OUT, "string/char_to_lower.hrl").

-define(UC_IU_OUT, "string/is_upper.hrl").
-define(UC_IL_OUT, "string/is_lower.hrl").

-define(UC_CT_OUT, "string/char_type.hrl").
write({CharToLowerFd, CharToUpperFd, IsLowerFd, IsUpperFd, CharTypeFd} = Fds) ->
	receive
	{Id, Format, Par} ->
		case Id of	
			char_to_upper -> Fd = CharToUpperFd;
			char_to_lower -> Fd = CharToLowerFd;
			is_lower -> Fd = IsLowerFd;
			is_upper -> Fd = IsUpperFd;
			char_type -> Fd = CharTypeFd;
			_ -> Fd = false
		end,
		case Fd of
			false -> ok;
			_ -> io:format(Fd, Format, Par)
		end,
	write(Fds)
	end.
	
gen() -> 
	{ok, InFd} = file:open(?UCDATA, [read, raw]),
	{ok, UpperOutFd} = file:open(?UC_UC_OUT, [write]),
	{ok, LowerOutFd} = file:open(?UC_LC_OUT, [write]),
	{ok, IsUpperOutFd} = file:open(?UC_IU_OUT, [write]),
	{ok, IsLowerOutFd} = file:open(?UC_IL_OUT, [write]),
	{ok, CharTypeOutFd} = file:open(?UC_CT_OUT, [write]),
	Pid = spawn(?MODULE, write, [{LowerOutFd, UpperOutFd, IsLowerOutFd, IsUpperOutFd, CharTypeOutFd}]),
	do_gen(InFd, Pid),
	Pid ! {char_to_upper, "char_to_upper(C) -> C.", []},
	Pid ! {is_upper, "is_upper(_) -> false.", []},
	Pid ! {is_lower, "is_lower(_) -> false.", []},
	Pid ! {char_to_lower, "char_to_lower(C) -> C.", []},
	Pid ! {char_type, "char_type(_) -> other.", []}.
do_gen(InFd, Pid) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, Pid);
		{ok, Data} ->
			Tokens = ux.string:explode(";", Data)++[""],
			Code = lists:nth(1, Tokens),
			Uppercase = lists:nth(13, Tokens),
			Lowercase = lists:nth(14, Tokens),
			Comment = lists:nth(2, Tokens),
			Abbr = lists:nth(3, Tokens),
			case Abbr of
				"Lu" ->
					case Lowercase of
						[] -> skip;
						_  -> Pid ! {char_to_lower,
							"char_to_lower(16#~s) -> 16#~s; %~s ~n", 
							[Code, Lowercase, Comment]}
					end,
					Pid ! {is_upper,
						"is_upper(16#~s) -> true; ~n", 
						[Code]};
				"Ll" ->
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
			case {Abbr, Code} of
				{[], _} -> ok;
				{_, []} -> ok;
				{_,  _} -> Pid ! {char_type,
						"char_type(16#~s) -> ~s; ~n", 
						[Code, string:to_lower(Abbr)]}
			end,
			do_gen(InFd, Pid);
		eof ->
			ok
	end.
