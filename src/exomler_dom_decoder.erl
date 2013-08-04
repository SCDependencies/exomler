-module(exomler_dom_decoder).

%% API
-export([start/1]).

-include("exomler.hrl").

%% API
start(Bin) when is_binary(Bin) ->
    Bin1 = skip(Bin), 
    {Tag, Rest} = tag(ltrim(Bin1)),
    <<>> = ltrim(Rest), 
    Tag.

%% internal
skip(<<"<?xml", Bin/binary>>) ->
    [_, Rest] = binary:split(Bin, <<"?>">>),
    skip(ltrim(Rest));
skip(<<"<!", Bin/binary>>) ->
    [_, Rest] = binary:split(Bin, <<">">>),
    ltrim(Rest);
skip(Bin) ->
    Bin.

tag(<<"<", Bin/binary>>) ->
    [TagHeader1, Rest1] = binary:split(Bin, <<">">>),
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

tag_header(TagHeader) ->
    case binary:split(TagHeader, <<" ">>) of
        [Tag] -> {Tag, []};
        [Tag, Attrs] -> {Tag, tag_attrs(Attrs)}
    end.

tag_attrs(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    tag_attrs(Bin);
tag_attrs(<<>>) ->
    [];
tag_attrs(Attrs) ->
    [Key, Value1] = binary:split(Attrs, <<"=">>),
    [Value2, Rest] = attr_value(ltrim(Value1)),
    [{rtrim(Key), unescape(Value2)}|tag_attrs(Rest)].

attr_value(<<Quote, Value/binary>>) when ?IS_QUOTE(Quote) ->
    binary:split(Value, <<Quote>>).

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
    [Text, _] = binary:split(Bin, <<"<">>),
    Len = size(Text),
    <<_:Len/binary, Rest1/binary>> = Bin,
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
    case binary:split(Bin, <<"&">>) of
        [Unescaped] ->
            Unescaped;
        [Unescaped, Rest1] ->
            {Char, Rest3} = case Rest1 of
                <<"quot;", Rest2/binary>> -> {$", Rest2};
                <<"apos;", Rest2/binary>> -> {$', Rest2};
                <<"lt;", Rest2/binary>> -> {$<, Rest2};
                <<"&gt;", Rest2/binary>> -> {$>, Rest2};
                <<"&amp;", Rest2/binary>> -> {$&, Rest2}
            end,
            <<Unescaped/binary, Char, (unescape(Rest3))/binary>>
    end.

%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_tag_test_() ->
    [
    ?_assertEqual({<<"html">>, [], []},
        start(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        start(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        start(<<"\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        start(<<"<html ></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        start(<<"<html ></html >\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        start(<<"<html/>\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        start(<<"\n<html />\n">>))
    ].

decode_content_test_() ->
    [
    ?_assertEqual({<<"html">>, [], [<<"Body">>]},
        start(<<"<html >Body</html>\n">>)),
    ?_assertEqual({<<"html">>, [], [{<<"head">>, [], []}]},
        start(<<"<html><head></head></html>\n">>)),
    ?_assertEqual({<<"html">>, [], [<<"TextBefore">>, {<<"head">>, [], []}, <<"TextAfter">>]},
        start(<<"<html >TextBefore<head></head>TextAfter</html>\n">>)),
    ?_assertEqual({<<"html">>, [], [<<"TextBefore">>, {<<"head">>, [], []}, <<"TextAfter">>]},
        start(<<"<html > \nTextBefore<head></head>TextAfter</html>\n">>)),
        ?_assertEqual({<<"html">>, [], [{<<"p">>, [], [<<"0 < 1">>]}]},
        start(<<"<html><p> 0 &lt; 1 </p></html>\n">>))
    ].

decode_attributes_test_() -> 
    [
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        start(<<"<html xmlns=\"w3c\"></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        start(<<"<html xmlns=\"w3c\" ></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []},
        start(<<"<html xmlns='w3c' />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        start(<<"<html k=\"v\"/>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        start(<<"<html k=\"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        start(<<"<html k  =  \"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<" 0 < 1 ">>}], []},
        start(<<"<html k  =  \" 0 &lt; 1 \" />\n">>))
  ].

-endif.
