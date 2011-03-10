%% ux.string library 
%%
%% @package  ux.string
%% @author   Uvarov Michael <freeakk@gmail.com>
%% @license  http://www.fsf.org/copyleft/lgpl.html LGPL
%% @link     http://pear.horde.org/index.php?package=History
%%
%% CopyrightBegin%
%% Copyright 2010 Uvarov Michael  
%%
%% See the enclosed file COPYING for license information (LGPL). If you
%% did not receive this file, see http://www.fsf.org/copyleft/lgpl.html
%% %CopyrightEnd%
-module(ux.string).

-import(lists).
-import(dict).
-import(io).
-import(io_lib).
-import(unicode).
-import(erlang).

-export([list_to_latin1/1]).
-export([char_comment/1]).
-export([htmlspecialchars/1, hsc/1]). % hsc is short name
-export([explode/2, explode/3, to_lower/1, to_upper/1]).
-export([st/1, strip_tags/1]).
-export([st/2, strip_tags/2]).
-export([st/3, strip_tags/3]).
-export([to_string/1]).
-export([delete_types/2, delete_types/3, filter_types/2, filter_types/3, explode_types/2, split_types/2]).
-export([first_types/3, last_types/3]).

% for tests
-export([tags_to_list/1]).
-export([delete_empty/1]).

% for utf-8
-export([char_to_lower/1, char_to_upper/1]).
-export([is_lower/1, is_upper/1]).
-export([is_letter/1, is_number/1, is_decimal/1, is_separator/1, is_pm/1, is_punctuation_mark/1]).

-export([freq/1, freq_dict/1]).
-export([ccc/1]).
-export([is_nfc/1, is_nfd/1, is_nfkc/1, is_nfkd/1]).
-export([to_nfc/1, to_nfd/1, to_nfkc/1, to_nfkd/1]).
-export([to_ncr/1]).

-export([is_comp_excl/1]).
-export([is_hangul/1]).

%% @doc Returns various "character types" which can be used 
%% as a default categorization in implementations.
%% Types:
%% http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category
%% @end
-export([char_type/1, char_types/1]).

-define(ASSERT(TEST,TRUE,FALSE), case TEST of 
	true -> TRUE; 
	false -> FALSE
end).

-define(ASSERT_IN_ARRAY_LAMBDA(TEST), case TEST of 
	true -> fun lists:member/2; 
	false -> fun not_in_array/2
end).

%% Defines Hangul constants
-define(HANGUL_SBASE,  16#AC00).
-define(HANGUL_LBASE,  16#1100).
-define(HANGUL_VBASE,  16#1161).
-define(HANGUL_TBASE,  16#11A7).
-define(HANGUL_LCOUNT, 19).
-define(HANGUL_VCOUNT, 21).
-define(HANGUL_TCOUNT, 28).
-define(HANGUL_NCOUNT, 588).
-define(HANGUL_SCOUNT, 11172).



-include("string/char_to_upper.hrl").
%char_to_upper(C) -> C.
-include("string/char_to_lower.hrl").
%char_to_lower(C) -> C.

-include("string/is_upper.hrl").
%% @doc Returns true, if is C is uppercase. 
-spec is_upper(C::char()) -> boolean().
%is_upper(_) -> false.
-include("string/is_lower.hrl").
%% @doc Returns true, if is C is lowercase.
-spec is_lower(C::char()) -> boolean().
%is_lower(_) -> false.

-include("string/char_comment.hrl").
-include("string/char_type.hrl").
%% @doc Returns a char type.
-spec char_type(C::char()) -> atom().
%char_type(_) -> other.
char_types(Str)	-> lists:map({?MODULE, char_type}, Str).

-include("string/freq_dict.hrl").
-include("string/ccc.hrl").

%% From http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt
-include("string/nfc_qc.hrl").
-include("string/nfd_qc.hrl").
-include("string/nfkc_qc.hrl").
-include("string/nfkd_qc.hrl").

%% From http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt
-include("string/is_comp_excl.hrl").
-include("string/is_compat.hrl").
-include("string/decomp.hrl").
-include("string/comp.hrl").

%freq_dict(_) -> 0.

%% @doc Returns true, if C is a letter.
-spec is_letter(C::char()) -> boolean().

is_letter(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$l,_] -> true;
			_      -> false
		end.	

%% @doc Returns true, if is C is a number.
-spec is_number(C::char()) -> boolean().

is_number(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$n,_] -> true;
			_      -> false
		end.	

%% @doc Return true, if is C is a separator.
-spec is_separator(C::char()) -> boolean().

is_separator(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$z,_] -> true;
			_      -> false
		end.	

%% @see ux.string:is_punctiation_mark/1
-spec is_pm(C::char()) -> boolean().

is_pm(C) -> is_punctuation_mark(C).

%% @doc Returns true, if is C is a punctiation mark.
-spec is_punctuation_mark(C::char()) -> boolean().

is_punctuation_mark(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$p,_] -> true;
			_      -> false
		end.	

%% @doc Return true, if C is a decimal number.
-spec is_decimal(C::char()) -> boolean().

is_decimal(C) -> char_type(C) == nd.

%% @doc Returns a new string which is made from the chars of Str 
%% which are not a type from Types list.
%% @end
-spec delete_types([atom()], string()) -> string().

delete_types(Types, Str) -> 
	lists:filter(fun(El) -> 
		not lists:member(char_type(El), Types) 
	end, Str).

%% @doc Stops delete_type/2 after Limit deleted chars. If Limit < 0, then
%% stops after -Limit skipped chars.
%% @end
-spec delete_types([atom()], string(), integer()) -> string().

delete_types(Types, Str, Limit) when Limit > 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
				fun not_in_array/2, 0, -1));
delete_types(Types, Str, Limit) when Limit < 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
				fun not_in_array/2, 1,  0)).

%% @doc Returns a new string which is made from the chars of Str 
%% which are a type from Types list.
% @end
-spec filter_types([atom()], string()) -> string().

filter_types(Types, Str) -> 
	lists:filter(fun(El) -> 
		lists:member(char_type(El), Types) 
	end, Str).

%% @doc Stops filter_type/2 after Limit extracted chars. If Limit < 0, then
%% stops after -Limit skipped chars.
%% @end
-spec filter_types([atom()], string(), integer()) -> string().

filter_types(Types, Str, Limit) when Limit > 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
					fun lists:member/2, -1, 0));
filter_types(Types, Str, Limit) when Limit < 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
					fun lists:member/2,  0, 1)).

%% @doc If Len>0, then gets first Len chars of type, which is in Types
%% If Len<0, then gets first -Len chars of type, which is NOT in Types
%% @end
-spec first_types([atom()], string(), integer()) -> string().
first_types(Types, Str, Len) -> 
	lists:reverse(get_types(Types, Str, Len, [], false, 
		?ASSERT_IN_ARRAY_LAMBDA(Len>0), ?ASSERT(Len>0, -1, 1), 0)).

%% @doc If Len>0, then gets last Len chars of type, which is in Types
%% If Len<0, then gets last -Len chars of type, which is NOT in Types
%% @end
-spec last_types([atom()], string(), integer()) -> string().
last_types(Types, Str, Len) -> 
	get_types(Types, lists:reverse(Str), Len, [], false, 
		?ASSERT_IN_ARRAY_LAMBDA(Len>0), ?ASSERT(Len>0, -1, 1), 0).
	
get_types(_, [], _, Result, _, _, _, _) -> Result;
get_types(_,  _, 0, Result, false, _, _, _) -> Result;
get_types(_,  Tail, 0, Result, true, _, _, _) -> 
	lists:reverse(Tail)++Result;
get_types(Types, [Char|Tail], 
	Len, % Strop after Len chars
	Result, % Result array
	RetTail, % Concat tail with Result or not
	Fun, % Check function
	TrueStep, % Len+TrueStep, if Fun return true
	FalseStep) -> 
	case apply(Fun, [char_type(Char), Types]) of
		true  -> get_types(Types, Tail, Len+TrueStep, [Char|Result], 
					RetTail, Fun, TrueStep, FalseStep);
		false -> get_types(Types, Tail, Len+FalseStep, Result, 
					RetTail, Fun, TrueStep, FalseStep)
	end.

%% @doc Returns a new list of strings which are parts of Str splited 
%% by separator chars of a type from Types list.
%% @end
-spec explode_types([atom()], string()) -> string().

explode_types(Types, Str) -> 
	explode_reverse(explode_types_cycle(Types, Str, [], [])).

explode_types_cycle(_, [], [], Res) -> Res;
explode_types_cycle(_, [], Buf, Res) -> [Buf|Res];
explode_types_cycle(Types, [Char|Str], Buf, Res) -> 
	case lists:member(char_type(Char), Types) of
		true  -> explode_types_cycle(Types, Str, [], [Buf|Res]);
		false -> explode_types_cycle(Types, Str, [Char|Buf], Res)
	end.

%% @doc Returns a new list of strings which are parts of Str splited 
%% by separator chars of a type from Types list. Parts can not be
%% empty.
%% @end 
-spec split_types([atom()], string()) -> string().

split_types(Types, Str) -> delete_empty(explode_types(Types, Str)).

%% @doc Deletes all empty lists from List.
%% Example:
%% delete_empty([ [], "test", [1] ]) -> ["test", [1]].
%% @end
-spec delete_empty([T]) -> [T].

delete_empty([])        -> [];
delete_empty([[]|List]) -> delete_empty(List);
delete_empty([El|List]) -> [El|delete_empty(List)].

%% @doc Converts something to string (list).
-spec to_string(string() | atom() | integer()) -> string().

to_string(Str) when is_list(Str) -> Str;
to_string(Str) when is_atom(Str) -> erlang:atom_to_list(Str);
to_string(Str) when is_integer(Str) -> [Str].

%% @doc Splits the string by delimeters.
-spec explode([string()], string()) -> string().
-spec explode([string()], string(), integer()) -> string().

explode([], _) -> false;
explode(_, []) -> [];
explode(Delimeter, Str) -> 
	case explode_cycle(Delimeter, Str, [], []) of
		false -> [Str];
		Res -> explode_reverse(Res)
	end.
explode([], _, _) -> false;
explode(Delimeter, Str, Limit) when (Limit>0) -> 
	explode_reverse(explode_cycle_pos(Delimeter, Str, [], [], Limit));
explode(Delimeter, Str, Limit) when (Limit<0) -> 
	case explode_cycle(Delimeter, Str, [], []) of
		false -> [];
		Res -> explode_reverse(lists:nthtail(-Limit, Res))
	end;
explode(Delimeter, Str, _) -> explode(Delimeter, Str).

explode_reverse(Res) -> lists:map({lists, reverse}, lists:reverse(Res)). 

%% @doc This function puts a part of the string before the delimeter in Buf, 
%% if the delimeter is a substring of Str, then return Buf.
%% Buf is a reversed list of reversed parts of the string.
%% Return false, if Delimeter is not a part of Str.
%% @end
explode_cycle(_, [], _, []) -> false;
explode_cycle(_, [], Buf, Result) -> [Buf|Result];
explode_cycle(Delimeter, Str, Buf, Result) ->
	case explode_check(Delimeter, Str) of
		false -> [C|Tail] = Str, 
			explode_cycle(Delimeter, Tail, [C|Buf], Result);
		Tail -> explode_cycle(Delimeter, Tail, [], [Buf|Result])
	end.

explode_cycle_pos(_, [], Buf, Result, _) -> [Buf|Result];
explode_cycle_pos(_, Str, _, Result, 1) -> [lists:reverse(Str)|Result];
explode_cycle_pos(Delimeter, Str, Buf, Result, Limit) ->
	case explode_check(Delimeter, Str) of
		false -> [C|Tail] = Str, 
			explode_cycle_pos(Delimeter, Tail, [C|Buf], Result, 
					Limit);
		Tail -> explode_cycle_pos(Delimeter, Tail, [], [Buf|Result], 
					Limit-1)
	end.

%% @doc This function get a delimeter and a part of the string 
%% If (Str = Delimeter + Tail), return a Tail, else return 'false'.
%% @end
explode_check([], Tail) ->
	Tail;
explode_check([Delimeter], Str) when is_list(Delimeter) ->
	explode_check(Delimeter, Str);	
explode_check([Delimeter|DelArr], Str) when is_list(Delimeter) ->
	case explode_check(Delimeter, Str) of
		false -> explode_check(DelArr, Str);
		Result -> Result 
	end;
explode_check([DelHead|DelTail], [Head|Tail]) when (DelHead == Head) ->
	explode_check(DelTail, Tail);
explode_check(_, _) ->
	false.

%% @doc Converts characters of a string to a lowercase format.
-spec to_lower(string()) -> string().

to_lower(Str) ->
	lists:map({?MODULE, char_to_lower}, Str).

%% @doc Converts characters of a string to a uppercase format.
-spec to_upper(string()) -> string().

to_upper(Str) ->
	lists:map({?MODULE, char_to_upper}, Str).

%% @doc Encodes html special chars.
-spec htmlspecialchars(string()) -> string().

htmlspecialchars([]) -> [];
htmlspecialchars(Str) -> hsc(Str).

%% @see ux.string:htmlspecialchars/1
-spec hsc(string()) -> string().

hsc([]) -> [];
hsc(Str) -> hsc(lists:reverse(Str), []).

hsc([], Buf) -> Buf;
hsc([$"|T], Buf) -> hsc(T, lists:append("&quot;", Buf));
hsc([$'|T], Buf) -> hsc(T, lists:append("&#39;", Buf));
hsc([$&|T], Buf) -> hsc(T, lists:append("&amp;", Buf));
hsc([$<|T], Buf) -> hsc(T, lists:append("&lt;", Buf));
hsc([$>|T], Buf) -> hsc(T, lists:append("&gt;", Buf));
hsc([H|T], Buf) -> hsc(T, [H|Buf]).

%% @doc Deletes tags from the string.
%%
%% Example: 
%%  > ux.string:strip_tags("<b>some string</b>").
%% "some string"
%% > ux.string:strip_tags("<h1>Head</h1><p>and paragraf</p>", ["h1"]).	
%% "<h1>Head</h1>and paragraf"
%% ux.string:strip_tags("<h1>Head</h1><p><!-- and paragraf --></p>", ["!--"]).
%% "Head<!-- and paragraf -->"
%% ux.string:st("a<br />b", [], " ").
%% "a b"
%% @end
-spec strip_tags(string()) -> string().
-spec strip_tags(string, [string() | atom() | char()]) -> string().

strip_tags(Str) -> st(Str, []).

strip_tags(Str, Allowed) -> st(Str, Allowed).
strip_tags(Str, Allowed, Sub) -> st(Str, Allowed, Sub).

%% @see ux.string:strip_tags/1
st(Str) -> st_cycle(Str, [], 0, []).
%% @see ux.string:strip_tags/2
st(Str, []) -> st(Str); 
st(Str, [$<|Allowed]) -> st(Str, tags_to_list(Allowed));
st(Str, Allowed) -> st(Str, Allowed, []). 
%% @see ux.string:strip_tags/3
st(Str, [], []) -> st(Str); 
st(Str, [$<|Allowed], Sub) -> st(Str, tags_to_list(Allowed), Sub);
st(Str, [], Sub) -> st_cycle(Str, [], 0, lists:reverse(Sub)); 
st(Str, Allowed, Sub) -> 
	st_cycle_with_allowed_tags(Str, [], string:to_lower(Str), 
			lists:map({lists, reverse},
			lists:map({string, to_lower},
			lists:map({?MODULE, to_string}, Allowed))), 
			lists:reverse(Sub)).

%% @doc Drops all tags from the string.
%% Cnt is a count of not closed <
%% If we found <, then Cnt++
%% If we found >, then Cnt--
%% @end
st_cycle([], Buf, _, _) -> lists:reverse(Buf);
st_cycle([$<|Tail], Buf, Cnt, Sub) -> st_cycle(Tail, Buf, Cnt + 1, Sub);
st_cycle([$>|Tail], Buf, 1, Sub) -> st_cycle(Tail, lists:append(Sub, Buf), 0, Sub);
st_cycle([$>|Tail], Buf, 0, Sub) -> st_cycle(Tail, Buf, 0, Sub);
st_cycle([$>|Tail], Buf, Cnt, Sub) -> st_cycle(Tail, Buf, Cnt - 1, Sub);
st_cycle([Head|Tail], Buf, 0, Sub) -> st_cycle(Tail, [Head|Buf], 0, Sub);
st_cycle([_|Tail], Buf, Cnt, Sub) -> st_cycle(Tail, Buf, Cnt, Sub).

%% @doc Drops tags, but saves tags in the Allowed list.
st_cycle_with_allowed_tags([], Buf, _, _, _) -> lists:reverse(Buf);	
st_cycle_with_allowed_tags(Str, Buf, LowerStr, Allowed, Sub) ->
	case st_get_tag(Str, LowerStr) of
		{Tag, SubStr, Tail, LowerTail} -> 
			case lists:member(Tag, Allowed) of 
				true -> st_cycle_with_allowed_tags(Tail, 
					lists:append(SubStr, Buf), LowerTail, 
					Allowed, Sub);
				false -> st_cycle_with_allowed_tags(Tail,
					lists:append(Sub, Buf), LowerTail, 
					Allowed, Sub)
			end;
		{Char, Tail, LowerTail} -> st_cycle_with_allowed_tags(Tail,
					[Char|Buf], LowerTail, Allowed, Sub);
		{Tail, LowerTail} -> st_cycle_with_allowed_tags(Tail, Buf, 
					LowerTail, Allowed, Sub)
	end.

%% @doc If Str is begining from <, then returns {Tag, SubStr, Tail, LowerTail},
%% else returns {first char of the string, Tail, LowerTail}.
%% @end 
st_get_tag([$<|Tail], [_|LowerTail]) ->
	st_get_tag_end(Tail, LowerTail, [$<], [], true, 1);
st_get_tag([$>|Tail], [_|LowerTail]) ->
	% First symbol of a string is $>.
	{Tail, LowerTail};
st_get_tag([Char|Tail], [_|LowerTail]) ->
	{Char, Tail, LowerTail}.

%% @doc Convert string of tags to list
%% Example:
%% > tags_to_list("<a><b>").
%% ["a", "b"]
%% @end
tags_to_list(Str) -> tags_to_list(Str, [], []).
tags_to_list([$<|Str], Res, Buf) -> tags_to_list(Str, Res, Buf);
tags_to_list([$/|Str], Res, Buf) -> tags_to_list(Str, Res, Buf);
tags_to_list([$>|Str], Res, Buf) -> tags_to_list(Str, [lists:reverse(Buf)|Res], []);
tags_to_list([Char|Str], Res, Buf) -> tags_to_list(Str, Res, [Char|Buf]);
tags_to_list([], Res, _) -> Res. 


%% @doc Extract a tag from the beginning of the string..
%% In format:
%% {String,
%% Lower Case String, 
%% Buffer for a substring,
%% Tag - accumulates a tag name, 
%% Capture chars of a tag name or not, 
%% a number of unclosed tags}
%% @end
st_get_tag_end([$>|Tail], [_|LowerTail], Buf, Tag, _, 1) ->
	{Tag, [$>|Buf], Tail, LowerTail};
st_get_tag_end([$>|Tail], [_|LowerTail], Buf, Tag, CaptureFlag, Cnt) ->
	st_get_tag_end(Tail, LowerTail, Buf, Tag, CaptureFlag, Cnt - 1);
st_get_tag_end([$<|Tail], [_|LowerTail], Buf, Tag, CaptureFlag, Cnt) ->
	st_get_tag_end(Tail, LowerTail, Buf, Tag, CaptureFlag, Cnt + 1);
st_get_tag_end([$ |Tail], [_|LowerTail], Buf, Tag, _, Cnt) ->
	st_get_tag_end(Tail, LowerTail, [$ |Buf], Tag, false, Cnt);
st_get_tag_end([$/|Tail], [_|LowerTail], Buf, Tag, true, Cnt) ->
	st_get_tag_end(Tail, LowerTail, [$/|Buf], Tag, true, Cnt);
st_get_tag_end([Head|Tail], [LowerHead|LowerTail], Buf, Tag, true, Cnt) ->
	st_get_tag_end(Tail, LowerTail, [Head|Buf], [LowerHead|Tag], true, Cnt);
st_get_tag_end([Head|Tail], [_|LowerTail], Buf, Tag, false, Cnt) ->
	st_get_tag_end(Tail, LowerTail, [Head|Buf], Tag, false, Cnt).

not_in_array(X,Y) -> not lists:member(X,Y).

%% @doc Counts a letter frequency
-spec freq(string()) -> dict(). 

freq(Str) -> freq_1(Str, dict:new()).

freq_1([Char|Str], Dict) -> freq_1(Str, dict:update_counter(Char, 1, Dict));
freq_1([], Dict)         -> Dict.


%% NORMALIZATION
%% http://unicode.org/reports/tr15/
is_nf([Head|Tail], LastCC, Result, CheckFun) -> 
    case ccc(Head) of
        CC when (LastCC > CC) and (CC =/= 0) -> no;
        CC ->   case apply(CheckFun, [Head]) of
                    n -> no;
                    m -> is_nf(Tail, CC, maybe,  CheckFun);
                    y -> is_nf(Tail, CC, Result, CheckFun)
                end
    end;
is_nf([], _, Result, _) -> Result.



%% Detecting Normalization Forms
%% http://unicode.org/reports/tr15/#Detecting_Normalization_Forms
is_nfc(Str)  -> is_nf(Str, 0, yes, fun nfc_qc/1).
is_nfd(Str)  -> is_nf(Str, 0, yes, fun nfd_qc/1).
is_nfkc(Str) -> is_nf(Str, 0, yes, fun nfkc_qc/1).
is_nfkd(Str) -> is_nf(Str, 0, yes, fun nfkd_qc/1).

to_nfc([])   -> [];
to_nfc(Str)  -> get_composition(get_recursive_decomposition(true,  Str)).
to_nfkc([])  -> [];
to_nfkc(Str) -> get_composition(get_recursive_decomposition(false, Str)).
to_nfd([])   -> [];
to_nfd(Str)  -> get_recursive_decomposition(true,  Str).
to_nfkd([])  -> [];
to_nfkd(Str) -> get_recursive_decomposition(false, Str).


is_acsii(Char) when (Char>=0) and (Char=<16#7F) 
    -> true;
is_acsii(_) 
    -> false.

list_to_latin1(Str) ->
    lists:reverse(list_to_latin1(Str, [])).

list_to_latin1([Char|Str], Res) ->
    list_to_latin1(Str, char_to_list(Char, [], Res));
list_to_latin1([],         Res) -> Res.

% magic
% Char>255
char_to_list(Char, Buf, Res) ->
    case Char bsr 8 of
        0   ->  case Buf of
                    [] -> [Char|Res];
                    _  -> lists:reverse(Buf)++[Char|Res]
                end;
        Div ->  Rem = Char band 2#11111111,
                char_to_list(Div, [Rem|Buf], Res)
    end.

% internal_decompose(Str)
% Canonical  If true bit is on in this byte, then selects the recursive 
%            canonical decomposition, otherwise selects
%            the recursive compatibility and canonical decomposition.
get_recursive_decomposition(Canonical, Str) -> 
    normalize(
            get_recursive_decomposition(Canonical, Str, [])).

get_recursive_decomposition(Canonical, [Char|Tail], Result) ->
    IsHangul = is_hangul_precomposed(Char),
    if
      IsHangul
      ->  get_recursive_decomposition(Canonical, Tail,
               hangul_decomposition(Char, Result));

      Char < 128 
      ->  get_recursive_decomposition(Canonical, Tail,
               [Char|Result]);

      true 
      -> case decomp(Char) of
            []  -> get_recursive_decomposition(Canonical, Tail,
                                                [Char|Result]);
            Dec -> case Canonical and is_compat(Char) of % not is_compat = singleton
                    true    -> get_recursive_decomposition(Canonical,
                            Tail,  [Char|Result]);
                        false   -> get_recursive_decomposition(Canonical,
                            Tail,  get_recursive_decomposition(Canonical,
                            Dec, Result))
                   end
         end
    end;
get_recursive_decomposition(_, [], Result) -> Result.

% Normalize NFD or NFKD
normalize(Str)              -> normalize1(Str, [], []).
normalize1([], [ ], Result) -> Result;
normalize1([], Buf, Result) -> normalize2(lists:reverse(Buf), Result);
normalize1([Char|Tail], Buf, Result) ->
    Class = ccc(Char),
    if
        (Class == 0) and 
        (Buf == [])  -> normalize1(Tail, [], [Char | Result]);

        (Class == 0) -> normalize1(Tail, [], 
            [Char | normalize2(lists:reverse(Buf), Result)]);

        true -> normalize1(Tail, [{Class, Char} | Buf], Result)
    end.

% Append chars from Buf to Result in a right order.
normalize2([], Result)  -> Result;
normalize2(Buf, Result) ->
    case normalize3(Buf, false, 0) of
        false             -> Result;
        {_, Char} = Value -> normalize2(Buf -- [Value], [Char|Result])
    end.

% Return char from Buf with max ccc
normalize3([{CharClass, _} = Value | Tail], _, MaxClass) 
    when CharClass > MaxClass 
 -> normalize3(Tail, Value, CharClass);
normalize3([_|Tail], Value, MaxClass) 
 -> normalize3(Tail, Value, MaxClass);
normalize3([], Value, _) -> Value.


%% Internal Composition Function
get_composition([Char|Tail]) -> 
    CharClass = ccc(Char),
    hangul_composition(
            get_composition(Tail, Char, 
                case CharClass of
                    0 -> 0;
                    _ -> 256
                end, [], [])).

get_composition([Char|Tail], LastChar, _, Mods, Result) when Char < 128 ->
    get_composition(Tail, Char, 0, [], Mods++[LastChar|Result]);
get_composition([Char|Tail], LastChar, LastClass, Mods, Result) ->
    CharClass = ccc(Char),
    Comp = comp(LastChar, Char),
    if
        (Comp =/= false) 
        and ((LastClass < CharClass)
          or (LastClass == 0)) ->
            get_composition(Tail, Comp, LastClass, Mods, Result);
        (CharClass == 0) -> get_composition(Tail, Char, CharClass, [], 
            Mods ++ [LastChar|Result]);
        true -> get_composition(Tail, LastChar, CharClass, [Char|Mods], Result)
    end;
get_composition([], Char, _, [],   Result) ->
    [Char|Result];
get_composition([], Char, _, Mods, Result) ->
    Mods ++ [Char|Result].

% http://unicode.org/reports/tr15/#Hangul
is_hangul(Char) when ((Char>=16#1100) and (Char=<16#11FF)) % Hangul Jamo
                  or ((Char>=16#A960) and (Char=<16#A97C)) % Hangul Jamo Extended-A
                  or ((Char>=16#D7B0) and (Char=<16#D7C6)) % Hangul Jamo Extended-B
                  or ((Char>=16#D7CB) and (Char=<16#D7FB)) % Hangul Jamo Extended-B
                  or ((Char>=16#3131) and (Char=<16#318E)) % Hangul Compatibility Jamo 
                  or  (Char==17#302E) or  (Char==16#302F)  % Tone marks (used in Middle Korean) 
                  or ((Char>=16#AC00) and (Char=<16#D7A3)) % 11,172 precomposed Hangul syllables
                  or ((Char>=16#3200) and (Char=<16#321E)) % For parenthesised 
                  or ((Char>=16#3260) and (Char=<16#327E)) % and circled 
                  or ((Char>=16#FFDC) and (Char=<16#FFA0)) % For halfwidth 
                  -> true;
is_hangul(_)      -> false.

is_hangul_precomposed(Char) 
    when ((Char>=16#AC00) and (Char=<16#D7A3)) 
        % 11,172 precomposed Hangul syllables
                         -> true;
is_hangul_precomposed(_) -> false.

% Decompose one char of hangul
hangul_decomposition(Char, Result) ->
    SIndex = Char - ?HANGUL_SBASE,
    case (SIndex < 0) or (SIndex >= ?HANGUL_SCOUNT) of
        true -> [Char|Result]; % skip
        false -> 
            L = ?HANGUL_LBASE + (SIndex div ?HANGUL_NCOUNT),
            V = ?HANGUL_VBASE + (SIndex rem ?HANGUL_NCOUNT) div ?HANGUL_TCOUNT,
            T = ?HANGUL_TBASE + (SIndex rem ?HANGUL_TCOUNT),
            
            case T of
                ?HANGUL_TBASE -> [V|[L|Result]];
                _ -> [T|[V|[L|Result]]]
            end
    end.


% Compose hangul characters
% Example:
% hangul_composition(lists:reverse(Str));
hangul_composition([VChar|Tail]) ->
    hangul_composition(Tail, VChar, []);
hangul_composition([]) -> [].

% used in hangul_composition/3
hangul_composition([VChar|Tail], Result) ->
    hangul_composition(Tail, VChar, Result);
hangul_composition([], Result) -> Result.

% String is reversed
hangul_composition([LChar|Tail], VChar, Result) ->
   % 1. check to see if two current characters are L and V
   LIndex = LChar - ?HANGUL_LBASE,
    if 
      (0 =< LIndex) and (LIndex < ?HANGUL_LCOUNT) ->
        VIndex = VChar - ?HANGUL_VBASE,
        if
          (0 =< VIndex) and (VIndex < ?HANGUL_VCOUNT) ->
                 
            LVChar = ?HANGUL_SBASE + ?HANGUL_TCOUNT  
                   * (LIndex * ?HANGUL_VCOUNT + VIndex),
                 
                    % 2. check to see if two current characters are LV and T
                    case Result of % Try get last appended char
                       [] -> hangul_composition(Tail, [LVChar]);
                       [TChar|Result2] ->
                           TIndex = TChar - ?HANGUL_TBASE,
                           if
                             (0 < TIndex) and (TIndex < ?HANGUL_TCOUNT) ->
                                LVTChar = LVChar + TIndex,
                                hangul_composition(Tail, [LVTChar|Result2]);
                             true ->
                               hangul_composition(Tail, [LVChar|Result])
                           end
                    end;
          true -> hangul_composition(Tail, LChar, [VChar|Result])
        end;
      true -> hangul_composition(Tail, LChar, [VChar|Result])
     end;
hangul_composition([], Char, Result) ->
    [Char|Result].

%% Convert everything from utf-8 into an NCR (Numeric Character Reference)
to_ncr(Str) -> to_ncr(Str, []).

to_ncr([Char|Tail], Res) -> to_ncr(Tail, char_to_ncr(Char) ++ Res);
to_ncr([         ], Res) -> Res.

-spec char_to_ncr(char()) -> string().
char_to_ncr(Char) when Char =< 16#7F 
% one-byte character
    -> [Char];
char_to_ncr(Char) when Char =< 16#C2
% non-utf8 character or not a start byte
    -> [];
char_to_ncr(Char) 
    -> lists:flatten(io_lib:format("&#~p;", [Char])).   
