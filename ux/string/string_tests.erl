-module(ux.string_tests).
-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").
-define(NFTESTDATA, UNIDATA_DIRECTORY ++ "NormalizationTest.txt"). 
-import(file).
-import(io).
-import(io_lib).
-import(unicode).
-import(lists).
-import(string).
-import(timer).
-export([to_string/1, parse_int16/1, nfc_test/2, nfc_prof/1]).

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

	,?_assertEqual(M:F($c, "dfsawcddcs"), ["dfsaw", "dd", "s"])
	,?_assertEqual(M:F($c, "dfsawcddcs",2 ), ["dfsaw", "ddcs"])

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
	,?_assertEqual(M:F("<a><b>test<a", "a"), "<a>test")
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

%    NFC
%      c2 ==  NFC(c1) ==  NFC(c2) ==  NFC(c3)
%      c4 ==  NFC(c4) ==  NFC(c5)
%
%    NFD
%      c3 ==  NFD(c1) ==  NFD(c2) ==  NFD(c3)
%      c5 ==  NFD(c4) ==  NFD(c5)
%
%    NFKC
%      c4 == NFKC(c1) == NFKC(c2) == NFKC(c3) == NFKC(c4) == NFKC(c5)
%
%    NFKD
%      c5 == NFKD(c1) == NFKD(c2) == NFKD(c3) == NFKD(c4) == NFKD(c5)

nfc_test(_, 0) -> max;
nfc_test(InFd, Max) ->
    NFC  = fun 'ux.string':to_nfc/1,
    NFD  = fun 'ux.string':to_nfd/1,
    NFKC = fun 'ux.string':to_nfkc/1,
    NFKD = fun 'ux.string':to_nfkd/1,
    case file:read_line(InFd) of
        {ok, []} -> nfc_test(InFd, Max);
        {ok, Data} -> 
            case ux.string:explode("#", Data) of
                [LineWithoutComment|_] ->  
                    case lists:map(fun ?MODULE:to_string/1, 
                        ux.string:explode(";", LineWithoutComment)) of
                         [_,_,_,_,_,_] = Row ->
                           C1 = lists:nth(1, Row),
                           C2 = lists:nth(2, Row),
                           C3 = lists:nth(3, Row),
                           C4 = lists:nth(4, Row),
                           C5 = lists:nth(5, Row),
                           % {Result from function, From, To}
                           %NFD
                           ?assertEqual({Max,C3, C1, C3}, {Max,NFD(C1), C1, C3}),
                           ?assertEqual({C3, C2, C3}, {NFD(C2), C2, C3}),
                           ?assertEqual({C3, C3, C3}, {NFD(C3), C3, C3}),
                           ?assertEqual({C5, C4, C5}, {NFD(C4), C4, C5}),
                           ?assertEqual({C5, C5, C5}, {NFD(C5), C5, C5}),
                           %NFC
                           ?assertEqual({Max, C2, C1, C2}, {Max, NFC(C1), C1, C2}),
                           ?assertEqual({C2, C2, C2}, {NFC(C2), C2, C2}),
                           ?assertEqual({C2, C3, C2}, {NFC(C3), C3, C2}),
                           ?assertEqual({C4, C4, C4}, {NFC(C4), C4, C4}),
                           ?assertEqual({C4, C5, C4}, {NFC(C5), C5, C4}),
                           %NFKC
                           ?assertEqual({C4, C1}, {NFKC(C1), C1}),
                           ?assertEqual({C4, C2}, {NFKC(C2), C2}),
                           ?assertEqual({C4, C3}, {NFKC(C3), C3}),
                           ?assertEqual({C4, C4}, {NFKC(C4), C4}),
                           ?assertEqual({C4, C5}, {NFKC(C5), C5}),
                           %NFCD
                           ?assertEqual({C5, C1}, {NFKD(C1), C1}),
                           ?assertEqual({C5, C2}, {NFKD(C2), C2}),
                           ?assertEqual({C5, C3}, {NFKD(C3), C3}),
                           ?assertEqual({C5, C4}, {NFKD(C4), C4}),
                           ?assertEqual({C5, C5}, {NFKD(C5), C5}),
                           ok;
                        _ -> skip
                   end; 
                _ -> ok
            end,
            nfc_test(InFd, Max - 1);
        eof -> ok
    end.

nfc_prof(Count) ->
        {ok, InFd} = file:open(?NFTESTDATA, [read]),
                io:setopts(InFd,[{encoding,utf8}]),
                nfc_test(InFd, Count),
                ok.
nfc_test_() ->
                        {timeout, 600, fun() -> 
                        profile(?MODULE, nfc_prof, [10000000]) end}.
parse_int16(Code) -> 
	case io_lib:fread("~16u", Code) of
        {ok, [Int], []} -> Int;
        _ -> 0 
    end.

to_string(Str) -> lists:map(fun parse_int16/1, string:tokens(Str, " ")).

to_string_test_() ->
	M = ?MODULE,
	F = 'to_string',
	[?_assertEqual(M:F("22 6e"), [34,110])
	%,?_assertEqual(M:F(), )
	].

profile(M, F, A) ->
%   apply({M, F}, A).
    io:format(user, "Time: ~w~n", [timer:tc(M, F, A)]).
