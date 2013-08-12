-module(exomler_dom_decoder).

%% API
-export([decode/1]).

-include("exomler.hrl").

%% API
decode(Bin) when is_binary(Bin) ->
    Bin1 = skip_prolog(ltrim(Bin)),
    Bin2 = skip_doctype(ltrim(Bin1)),
    {Tag, Rest} = tag(ltrim(Bin2)),
    <<>> = ltrim(Rest), 
    Tag.

%% internal
skip_prolog(<<"<?xml", Bin/binary>>) ->
    {_, Rest} = split(Bin, <<"?>">>),
    Rest;
skip_prolog(Bin) ->
    Bin.

skip_doctype(<<"<!", Bin/binary>>) ->
    {_, Rest} = split(Bin, <<">">>),
    Rest;
skip_doctype(Bin) ->
    Bin.

tag(<<"<", Bin/binary>>) ->
    {TagHeader1, Rest1} = split(Bin, <<">">>),
    Len = size(TagHeader1)-1, 
    case TagHeader1 of
        <<TagHeader:Len/binary, "/">> ->
            {Tag, Attrs} = tag_header(TagHeader),
            {{Tag, Attrs,[]}, Rest1};
        TagHeader ->
            {Tag, Attrs} = tag_header(TagHeader),
            {Content, Rest2} = tag_content(Rest1, Tag),
            {{Tag, Attrs, Content}, Rest2}
  end.

tag_header(Bin) ->
    {Tag, Rest} = split(Bin, <<" ">>),
    {Tag, tag_attrs(Rest)}.

tag_attrs(<<>>) ->
    [];
tag_attrs(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    tag_attrs(Bin);
tag_attrs(Bin) ->
    {Key, Value1} = split(Bin, <<"=">>),
    {Value2, Rest} = attr_value(Value1),
    [{rtrim(Key), unescape(Value2)}|tag_attrs(Rest)].

attr_value(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    attr_value(Bin);
attr_value(<<Quote, Value/binary>>) when ?IS_QUOTE(Quote) ->
    split(Value, <<Quote>>).

tag_content(<<"<![CDATA[", Bin/binary>>, Tag) ->
    {Text, Rest1} = split(Bin, <<"]]>">>),
    {Content, Rest2} = tag_content(Rest1, Tag),
    {[Text|Content], Rest2};
tag_content(<<"<!--", Bin/binary>>, Tag) ->
    {_Comment, Rest1} = split(Bin, <<"-->">>),
    tag_content(Rest1, Tag);
tag_content(<<"</", Bin/binary>>, Tag) ->
    Len = size(Tag),
    <<Tag:Len/binary, Rest1/binary>> = Bin,
    <<">", Rest2/binary>> = ltrim(Rest1),
    {[], Rest2};
tag_content(<<"<", _/binary>> = Bin, Tag) ->
    {TagData, Rest1} = tag(Bin),
    {Content, Rest2} = tag_content(Rest1, Tag),
    {[TagData|Content], Rest2};
tag_content(<<Blank, Bin/binary>>, Tag) when ?IS_BLANK(Blank) ->
    tag_content(Bin, Tag);
tag_content(Bin, Tag) ->
    {A, _} = binary:match(Bin, <<"<">>),
    <<Text:A/binary, Rest1/binary>> = Bin,
    {Content, Rest2} = tag_content(Rest1, Tag),
    {[rtrim(unescape(Text))|Content], Rest2}.

ltrim(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    ltrim(Bin);
ltrim(Bin) -> 
    Bin.

rtrim(<<>>) ->
    <<>>;
rtrim(Bin) ->
    case binary:last(Bin) of
        Blank when ?IS_BLANK(Blank) ->
            Size = size(Bin) - 1,
            <<Part:Size/binary, _/binary>> = Bin,
            rtrim(Part);
        _ ->
            Bin
    end.

unescape(Bin) ->
    case split(Bin, <<"&">>) of
        {Unescaped, <<>>} ->
            Unescaped;
        {Unescaped, <<"quot;", Rest/binary>>} ->
            <<Unescaped/binary, $", (unescape(Rest))/binary>>;
        {Unescaped, <<"apos;", Rest/binary>>} ->
            <<Unescaped/binary, $', (unescape(Rest))/binary>>;
        {Unescaped, <<"lt;",   Rest/binary>>} ->
            <<Unescaped/binary, $<, (unescape(Rest))/binary>>;
        {Unescaped, <<"gt;",   Rest/binary>>} ->
            <<Unescaped/binary, $>, (unescape(Rest))/binary>>;
        {Unescaped, <<"amp;",  Rest/binary>>} ->
            <<Unescaped/binary, $&, (unescape(Rest))/binary>>
    end.

split(Bin, Pattern) ->
    case binary:match(Bin, Pattern) of
		{A,B} ->
            <<Before:A/binary, _:B/binary, After/binary>> = Bin,
	        {Before, After};
	    nomatch ->
            {Bin, <<>>}
    end.

%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_tag_test_() ->
    [
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<html ></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<html ></html >\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        decode(<<"<html/>\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        decode(<<"\n<html />\n">>))
    ].

decode_content_test_() ->
    [
    ?_assertEqual({<<"html">>, [], [<<"Body">>]},
        decode(<<"<html >Body</html>\n">>)),
    ?_assertEqual({<<"html">>, [], [{<<"head">>, [], []}]},
        decode(<<"<html><head></head></html>\n">>)),
    ?_assertEqual({<<"html">>, [], [<<"TextBefore">>, {<<"head">>, [], []}, <<"TextAfter">>]},
        decode(<<"<html >TextBefore<head></head>TextAfter</html>\n">>)),
    ?_assertEqual({<<"html">>, [], [<<"TextBefore">>, {<<"head">>, [], []}, <<"TextAfter">>]},
        decode(<<"<html > \nTextBefore<head></head>TextAfter</html>\n">>)),
    ?_assertEqual({<<"html">>, [], [{<<"p">>, [], [<<"0 < 5 > 3 & \"5\"='5'">>]}]},
        decode(<<"<html><p> 0 &lt; 5 &gt; 3 &amp; &quot;5&quot;=&apos;5&apos; </p></html>\n">>)),
    ?_assertEqual({<<"html">>, [], [{<<"p">>, [], [<<" 0 < 5 > 3 & \"5\"='5'  ">>]}]},
        decode(<<"<html><p><![CDATA[ 0 < 5 > 3 & \"5\"='5'  ]]></p></html>\n">>)),
    ?_assertEqual({<<"html">>, [], [{<<"p">>, [], [<<"TextBefore">>, <<"TextAfter">>]}]},
        decode(<<"<html><p>TextBefore<!-- Comment -->TextAfter</p></html>\n">>))
    ].

decode_attributes_test_() -> 
    [
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        decode(<<"<html xmlns=\"w3c\"></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        decode(<<"<html xmlns=\"w3c\" ></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []},
        decode(<<"<html xmlns='w3c' />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        decode(<<"<html k=\"v\"/>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        decode(<<"<html k=\"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        decode(<<"<html k  =  \"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<" 0 < 1 ">>}], []},
        decode(<<"<html k  =  \" 0 &lt; 1 \" />\n">>))
  ].

-endif.
