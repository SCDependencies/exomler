-module(exomler_dom_decoder).

%% API
-export([decode_document/1]).
-export([decode/1]).

-include("exomler.hrl").

%% API
decode_document(Bin) when is_binary(Bin) ->
    {Version, Encoding, Rest1} = prolog(bstring:trim_left(Bin)),
    Rest2 = skip_doctype(bstring:trim_left(Rest1)),
    {Tag, _Rest} = tag(bstring:trim_left(Rest2)),
    {xml, Version, Encoding, Tag}.

decode(Bin) when is_binary(Bin) ->
    Rest1 = skip_prolog(bstring:trim_left(Bin)),
    Rest2 = skip_doctype(bstring:trim_left(Rest1)),
    {Tag, _Rest} = tag(bstring:trim_left(Rest2)),
    Tag.

%% internal
prolog(<<"<?xml", Bin/binary>>) ->
    {Prolog, Rest} = bstring:split(Bin, <<"?>">>),
    Attrs = tag_attrs(Prolog),
    {get_version(Attrs), get_encoding(Attrs), Rest}.

get_version(Attrs) ->
    case get_value(<<"version">>, Attrs) of
        <<"1.0">> -> '1.0';
        <<"1.1">> -> '1.1'
    end.

get_encoding(Attrs) ->
    Encoding = get_value(<<"encoding">>, Attrs), 
    case bstring:to_lower(Encoding) of
        <<"iso-8859-1">>    -> latin1;
        <<"iso_8859_1">>    -> latin1;
        <<"iso_8859-1">>    -> latin1;
        <<"iso8859-1">>     -> latin1;
        <<"utf-8">>         -> utf8;
        <<"utf_8">>         -> utf8;
        <<"utf8">>          -> utf8;
        <<>>                -> undefined;
        _                   -> unknown
    end.

get_value(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false -> <<>>
    end.

skip_prolog(<<"<?xml", Bin/binary>>) ->
    {_, Rest} = bstring:split(Bin, <<"?>">>),
    Rest;
skip_prolog(Bin) ->
    Bin.

skip_doctype(<<"<!", Bin/binary>>) ->
    {_, Rest} = bstring:split(Bin, <<">">>),
    Rest;
skip_doctype(Bin) ->
    Bin.

tag(<<"<", Bin/binary>>) ->
    {TagHeader1, Rest1} = bstring:split(Bin, <<">">>),
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
    {Tag, Rest} = bstring:split(Bin, <<" ">>),
    {Tag, tag_attrs(Rest)}.

tag_attrs(<<>>) ->
    [];
tag_attrs(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    tag_attrs(Bin);
tag_attrs(Bin) ->
    {Key, Value1} = bstring:split(Bin, <<"=">>),
    {Value2, Rest} = attr_value(Value1),
    [{bstring:trim_right(Key), unescape(Value2)}|tag_attrs(Rest)].

attr_value(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    attr_value(Bin);
attr_value(<<Quote, Value/binary>>) when ?IS_QUOTE(Quote) ->
    bstring:split(Value, <<Quote>>).

tag_content(<<"<![CDATA[", Bin/binary>>, Tag) ->
    {Text, Rest1} = bstring:split(Bin, <<"]]>">>),
    {Content, Rest2} = tag_content(Rest1, Tag),
    {[Text|Content], Rest2};
tag_content(<<"<!--", Bin/binary>>, Tag) ->
    {_Comment, Rest1} = bstring:split(Bin, <<"-->">>),
    tag_content(Rest1, Tag);
tag_content(<<"</", Bin/binary>>, Tag) ->
    Len = size(Tag),
    <<Tag:Len/binary, Rest1/binary>> = Bin,
    <<">", Rest2/binary>> = bstring:trim_left(Rest1),
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
    {[bstring:trim_right(unescape(Text))|Content], Rest2}.

unescape(Bin) ->
    case bstring:split(Bin, <<"&">>) of
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

%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_document_test_() ->
    [
    ?_assertEqual({xml, '1.0', utf8, {<<"html">>, [], []}},
        decode_document(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>))
    ].

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
