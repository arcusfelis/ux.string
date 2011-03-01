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

-export([htmlspecialchars/1, hsc/1]). % hsc is short name
-export([explode/2, explode/3, to_lower/1, to_upper/1]).
-export([st/1, strip_tags/1]).
-export([st/2, strip_tags/2]).
-export([st/3, strip_tags/3]).
-export([in_array/2]).
-export([to_string/1]).
-export([delete_types/2, filter_types/2, explode_types/2, split_types/2]).

% for tests
-export([tags_to_list/1]).
-export([delete_empty/1]).

% for utf-8
-export([char_to_lower/1, char_to_upper/1]).
-export([is_lower/1, is_upper/1]).
-export([is_letter/1, is_number/1, is_decimal/1, is_separator/1, is_pm/1, is_punctuation_mark/1]).

-export([freq/1, freq_dict/1]).

%% @doc Returns various "character types" which can be used 
%% as a default categorization in implementations.
%% Types:
%% http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category
%% @end
-export([char_type/1, char_types/1]).
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

-include("string/char_type.hrl").
%% @doc Returns a char type.
-spec char_type(C::char()) -> atom().
%char_type(_) -> other.
char_types(Str)	-> lists:map({ux.string, char_type}, Str).

-include("string/freq_dict.hrl").
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
		not ux.string:in_array(ux.string:char_type(El), Types) 
	end, Str).

%% @doc Returns a new string which is made from the chars of Str 
%% which are a type from Types list.
% @end
-spec filter_types([atom()], string()) -> string().

filter_types(Types, Str) -> 
	lists:filter(fun(El) -> 
		ux.string:in_array(ux.string:char_type(El), Types) 
	end, Str).

%% @doc Returns a new list of strings which are parts of Str splited 
%% by separator chars of a type from Types list.
%% @end
-spec explode_types([atom()], string()) -> string().

explode_types(Types, Str) -> 
	explode_reverse(explode_types_cycle(Types, Str, [], [])).

explode_types_cycle(_, [], [], Res) -> Res;
explode_types_cycle(_, [], Buf, Res) -> [Buf|Res];
explode_types_cycle(Types, [Char|Str], Buf, Res) -> 
	case in_array(char_type(Char), Types) of
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
			lists:map({'ux.string', to_string}, Allowed))), 
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
			case in_array(Tag, Allowed) of 
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

%% @doc Returns true, if the first argument in the second argument.
in_array(_, []) -> false;
in_array(Value, [Head|_]) when (Value == Head) -> true;
in_array(Value, [_|Tail]) -> in_array(Value, Tail).

%% @doc Counts a letter frequency
-spec freq(string()) -> dict(). 

freq(Str) -> freq_1(Str, dict:new()).

freq_1([], Dict)         -> Dict;
freq_1([Char|Str], Dict) -> freq_1(Str, dict:update_counter(Char, 1, Dict)).
