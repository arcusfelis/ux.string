-module(ux.string_tests).
-include_lib("eunit/include/eunit.hrl").

explode_test_() ->
	M = 'ux.string',
	F = 'explode',
	[?_assertEqual(M:F(":", "1:2:3"), ["1", "2", "3"])
	,?_assertEqual(M:F(":", "aa::aa"), ["aa", "", "aa"])
	,?_assertEqual(M:F(":", "aa::"), ["aa", "", ""])
	,?_assertEqual(M:F("::", "aa::aa"), ["aa", "aa"])
	,?_assertEqual(M:F("::", "aa:::aa"), ["aa", ":aa"])
	,?_assertEqual(M:F("::", "aa:::"), ["aa", ":"])

	,?_assertEqual(M:F([":", ";"], "aa:;:aa"), ["aa", "", "", "aa"])
	,?_assertEqual(M:F([";:", ";"], "aa:;:aa"), ["aa:", "aa"])

	% empty delimeter
	,?_assertEqual(M:F("", "test"), false)
	% limit >0
	,?_assertEqual(M:F("|", "one|two|three|four", 2), ["one", "two|three|four"])

	% limit <0
	,?_assertEqual(M:F("|", "one|two|three|four", -1), ["one", "two", "three"])
	,?_assertEqual(M:F("-", "one|two|three|four", -1), [])
	,?_assertEqual(M:F("-", "one|two|three|four"), ["one|two|three|four"])

	].

htmlspecialchars_test_() -> htmlspecialchars_test_X('htmlspecialchars').
hsc_test_() -> htmlspecialchars_test_X('hsc').

htmlspecialchars_test_X(F) ->
	M = 'ux.string',
	[?_assertEqual(M:F("ddf2#$\""), "ddf2#$&quot;")
	,?_assertEqual(M:F("test1 & test2"), "test1 &amp; test2")
	].
to_lower_test_() ->
	M = 'ux.string',
	F = 'to_lower',
	[?_assertEqual(M:F("small BIG"), "small big")
	,?_assertEqual(M:F(	[1069,1056,1051,1040,1053,1043]), 
				[1101,1088,1083,1072,1085,1075])
	].
to_upper_test_() ->
	M = 'ux.string',
	F = 'to_upper',
	[?_assertEqual(M:F("small BIG"), "SMALL BIG")
	,?_assertEqual(M:F(	[1101,1088,1083,1072,1085,1075]),
				[1069,1056,1051,1040,1053,1043])
	].

strip_tags_test_() ->
	strip_tags_test_X('strip_tags').
st_test_() ->
	strip_tags_test_X('st').

strip_tags_test_X(F) ->
	M = 'ux.string',
	[?_assertEqual(M:F("<b>a</b>"), "a")
	,?_assertEqual(M:F("<b>a b c</b>"), "a b c")
% Check a long tag
	,?_assertEqual(M:F("<H1>A B C</H1>"), "A B C")
	,?_assertEqual(M:F("a<img src='i.img' />b"), "ab")
% Check allowed tags
	,?_assertEqual(M:F("<b>a b c</b>", ["b"]), "<b>a b c</b>")
	,?_assertEqual(M:F("<B>a b c</B>", ["b"]), "<B>a b c</B>")
	,?_assertEqual(M:F("<code>a b c</code>", ["b"]), "a b c")
	,?_assertEqual(M:F("<code>a b c</code>", ["b", "code"]), "<code>a b c</code>")
	,?_assertEqual(M:F("<span>a b c</span>", ["b", "span"]), "<span>a b c</span>")
% Check a tag with an attribute
	,?_assertEqual(M:F("a<img src='i.gif' />b", ["b"]), "ab")
	,?_assertEqual(M:F("a<img src='i.gif' />b", ["img"]), "a<img src='i.gif' />b")
	,?_assertEqual(M:F("a<br/>b", ["br"]), "a<br/>b")
% Check an atom in the list allowed tags 
	,?_assertEqual(M:F("a<br/>b", [br]), "a<br/>b")
	,?_assertEqual(M:F("a<br/><b>b</b>", [br]), "a<br/>b")
% Check a replacement argument
	,?_assertEqual(M:F("<b>a b c</b>", [], " "), " a b c ")
	,?_assertEqual(M:F("<b>a b c</b>", [], "tag"), "taga b ctag")
	,?_assertEqual(M:F("<b>a b c</b>", [test], "tag"), "taga b ctag")
% PHP style
	,?_assertEqual(M:F("<b>a b c</b>", "<b>"), "<b>a b c</b>")
	,?_assertEqual(M:F("<span>a b c</span>", "<b><span>"), "<span>a b c</span>")
	].
tags_to_list_test_() ->
	M = 'ux.string',
	F = 'tags_to_list',
	[?_assertEqual(M:F("<a><b>"), ["b", "a"])
	,?_assertEqual(M:F("<span>"), ["span"])
	,?_assertEqual(M:F("<b><span>"), ["span", "b"])
	,?_assertEqual(M:F("<i>"), ["i"])
	].
delete_types_test_() ->
	M = 'ux.string',
	F = 'delete_types',
	[?_assertEqual(M:F([ll, lu], "Tom Cat!"), " !")
	,?_assertEqual(M:F([ll],     "Tom Cat!"), "T C!")
	,?_assertEqual(M:F([po],     "Tom Cat!"), "Tom Cat")
	,?_assertEqual(M:F([ll], "AaBbCc44ff", -2), "ABbCc44ff") %skip 2 (A,B)
	,?_assertEqual(M:F([ll], "AaBbCc44ff",  2), "ABCc44ff") %del 2 (a,b)
	,?_assertEqual(M:F([ll], "AaBbCc44ffdsBAF",  4), "ABC44fdsBAF")
	,?_assertEqual(M:F([ll], "AaBbCc44ffdsBAF", -4), "ABC44ffdsBAF")
	].
filter_types_test_() ->
	M = 'ux.string',
	F = 'filter_types',
	[?_assertEqual(M:F([ll, lu], "Tom Cat!"), "TomCat")
	,?_assertEqual(M:F([ll],     "Tom Cat!"), "omat")
	,?_assertEqual(M:F([po],     "Tom Cat!"), "!")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds",  3), "abc44ffds")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds",  4), "abcffds")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds", -2), "abCc44ffds")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds", -4), "abc4ffds")
	].
char_types_test_() ->
	M = 'ux.string',
	F = 'char_types',
	[?_assertEqual(M:F("Tom Cat!"), [lu,ll,ll,zs,lu,ll,ll,po])
	%,?_assertEqual(M:F(), )
	].
last_types_test_() ->
	M = 'ux.string',
	F = 'last_types',
	[?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -5), "99999")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -6), "D99999")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -7), "FD99999")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -8), "AFD99999")
	].
first_types_test_() ->
	M = 'ux.string',
	F = 'first_types',
	[?_assertEqual(M:F([ll], "AavbfFDsdfffds", 4), "avbf")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffds", 5), "avbfs")
	].
