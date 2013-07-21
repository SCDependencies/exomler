-module(exomler).

%% API
-export([encode/1]).
-export([decode/1]).

-define(IS_BLANK(Blank), Blank == $\s 
    orelse Blank == $\n
    orelse Blank == $\t
    orelse Blank == $\r).

-define(IS_QUOTE(Quote), Quote == $" orelse Quote == $').


%% API
encode({Tag, Attrs, Content}) ->
    encode_tag({Tag, Attrs, Content}).

decode(Bin) when is_binary(Bin) ->
    Bin1 = skip_declaration(Bin), 
    {Tag, Rest} = decode_tag(Bin1),
    <<>> = trim_left(Rest), 
    Tag.


%% internal
encode_tag({Tag, Attrs, Content}) ->
    BinAttrs = encode_tag_attrs(Attrs),
    BinContent = << <<(encode_content(SubTag))/binary>> || SubTag <- Content>>,
    Tag1 = trim_left(Tag),
    <<"<", Tag1/binary, BinAttrs/binary, ">", BinContent/binary, 
        "</", Tag1/binary, ">">>.

encode_tag_attrs(Attrs) ->
    encode_tag_attrs(Attrs, <<>>).

encode_tag_attrs([{Key, Value}|Tail], EncodedAttrs) ->
    EncodedAttr = <<" ", Key/binary, "=\"",Value/binary, "\"">>, 
    encode_tag_attrs(Tail, <<EncodedAttrs/binary, EncodedAttr/binary>>);
encode_tag_attrs([], EncodedAttrs) ->
    EncodedAttrs.

encode_content(Tuple) when is_tuple(Tuple) ->
    encode_tag(Tuple);
encode_content(Binary) when is_binary(Binary) ->
    escape(Binary).

skip_declaration(<<"<?xml", Bin/binary>>) ->
    [_, Rest] = binary:split(Bin, <<"?>">>),
    trim_left(Rest), 
    skip_declaration(Rest);
skip_declaration(<<"<!", Bin/binary>>) ->
    [_, Rest] = binary:split(Bin, <<">">>),
    trim_left(Rest);
skip_declaration(<<"<", _/binary>> = Bin) ->
    Bin;
skip_declaration(<<_, Bin/binary>>) ->
    skip_declaration(Bin).

decode_tag(<<"<", Bin/binary>>) ->
    [TagHeader1, Rest1] = binary:split(Bin, <<">">>),
    Len = size(TagHeader1)-1, 
    case TagHeader1 of
        <<TagHeader:Len/binary, "/">> ->
            {Tag, Attrs} = decode_tag_header(TagHeader),
            {{Tag, Attrs,[]}, Rest1};
        TagHeader ->
            {Tag, Attrs} = decode_tag_header(TagHeader),
            {Content, Rest2} = decode_tag_content(Rest1, Tag),
            {{Tag, Attrs, Content}, Rest2}
  end.

decode_tag_header(TagHeader) ->
    case binary:split(TagHeader, [<<" ">>]) of
        [Tag] -> {Tag, []};
        [Tag, Attrs] -> {Tag, decode_tag_attrs(Attrs)}
    end.

decode_tag_attrs(Attrs) ->
    case binary:split(trim_left(Attrs), <<"=">>) of
        [Key, Value1] ->
            [Value2, Rest] = decode_attr_value(trim_left(Value1)),
            [{trim(Key), unescape(Value2)}|decode_tag_attrs(Rest)];
        [<<>>] ->
            []
    end.

decode_attr_value(<<Quote, Value/binary>>) when ?IS_QUOTE(Quote) ->
    binary:split(Value, <<Quote>>).

decode_tag_content(Bin, Parent) ->
    decode_tag_content(Bin, Parent, []).

decode_tag_content(<<"</", Bin1/binary>>, Parent, Content) ->
    Len = size(Parent), 
    case binary:split(Bin1, <<">">>) of
        [<<Parent:Len/binary, Blank/binary>>, Bin] ->
            <<>> = trim_left(Blank),
            {lists:reverse(Content), Bin}
    end;
decode_tag_content(<<"<", _/binary>> = Bin, Parent, Content) ->
    {Tag, Rest1} = decode_tag(Bin),
    decode_tag_content(Rest1, Parent, [Tag|Content]);
decode_tag_content(Bin, Parent, Content) ->
    [Text, Rest] = binary:split(Bin, <<"<">>),
    decode_tag_content(<<"<", Rest/binary>>, Parent, [unescape(Text)|Content]).

trim(Bin) -> trim_right(trim_left(Bin)).

trim_left(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    trim_left(Bin);
trim_left(Bin) -> 
    Bin.

trim_right(<<>>) ->
    <<>>;
trim_right(Bin) ->
    case binary:last(Bin) of
        Blank when ?IS_BLANK(Blank) ->
            Size = byte_size(Bin) - 1,
            <<Part:Size/binary, _/binary>> = Bin,
            trim_right(Part);
        _ ->
            Bin
    end.

escape(Bin) -> escape(Bin, <<>>).

escape(<<"\"", Rest/binary>>, Escaped) ->
    escape(Rest, <<Escaped/binary, "&quot;">>);
escape(<<"'", Rest/binary>>, Escaped) ->
    escape(Rest, <<Escaped/binary, "&apos;">>);
escape(<<"<", Rest/binary>>, Escaped) ->
    escape(Rest, <<Escaped/binary, "&lt;">>);
escape(<<">", Rest/binary>>, Escaped) ->
    escape(Rest, <<Escaped/binary, "&gt;">>);
escape(<<"&", Rest/binary>>, Escaped) ->
    escape(Rest, <<Escaped/binary, "&amp;">>);
escape(<<C:1/binary, Rest/binary>>, Escaped) ->
    escape(Rest, <<Escaped/binary, C/binary>>);
escape(<<>>, Escaped) ->
    Escaped.

unescape(Bin) -> unescape(Bin, <<>>).

unescape(<<"&quot;", Rest/binary>>, Unescaped) ->
    unescape(Rest, <<Unescaped/binary, "\"">>);
unescape(<<"&apos;", Rest/binary>>, Unescaped) ->
    unescape(Rest, <<Unescaped/binary, "'">>);
unescape(<<"&lt;", Rest/binary>>, Unescaped) ->
    unescape(Rest, <<Unescaped/binary, "<">>);
unescape(<<"&gt;", Rest/binary>>, Unescaped) ->
    unescape(Rest, <<Unescaped/binary, ">">>);
unescape(<<"&amp;", Rest/binary>>, Unescaped) ->
    unescape(Rest, <<Unescaped/binary, "&">>);
unescape(<<C:1/binary, Rest/binary>>, Unescaped) ->
    unescape(Rest, <<Unescaped/binary, C/binary>>);
unescape(<<>>, Unescaped) ->
    Unescaped.



%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_tag_test_() ->
    [
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"\n\n\n<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
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
    ?_assertEqual({<<"html">>, [], [<<" \nTextBefore">>, {<<"head">>, [], []}, <<"TextAfter">>]},
        decode(<<"<html > \nTextBefore<head></head>TextAfter</html>\n">>)),
        ?_assertEqual({<<"html">>, [], [{<<"p">>, [], [<<" 0 < 1 ">>]}]},
        decode(<<"<html><p> 0 &lt; 1 </p></html>\n">>))
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

encode_tag_test_() ->
    [
    ?_assertEqual(<<"<html></html>">>, 
        encode({<<"html">>, [], []}))
    ].

encode_content_test_() ->
    [
    ?_assertEqual(<<"<html>Body</html>">>, 
        encode({<<"html">>, [], [<<"Body">>]})),
    ?_assertEqual(<<"<html>TextBefore<head>Body</head>TextAfter</html>">>, 
        encode({<<"html">>, [], [<<"TextBefore">>, {<<"head">>, [], [<<"Body">>]}, <<"TextAfter">>]}))
    ].

encode_attributes_test_() -> 
    [
    ?_assertEqual(<<"<html xmlns=\"w3c\"></html>">>, 
        encode({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}))
  ].

-endif.
