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
    Binary.

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

decode_tag_attrs(<<Blank, Attrs/binary>>) when ?IS_BLANK(Blank) -> 
    decode_tag_attrs(Attrs);
decode_tag_attrs(<<>>) ->
    [];
decode_tag_attrs(Attrs) ->
    case binary:split(Attrs, <<"=">>) of
        [Key, Value1] ->
            [Value2, Rest] = decode_attr_value(Value1),
            [{trim_right(Key), Value2}|decode_tag_attrs(Rest)]
    end.

decode_attr_value(<<Blank, Value/binary>>) when ?IS_BLANK(Blank) ->
    decode_attr_value(Value);
decode_attr_value(<<Quote, Value/binary>>) when ?IS_QUOTE(Quote) ->
    binary:split(Value, <<Quote>>);
decode_attr_value(<<>>) ->
    <<>>.

decode_tag_content(<<Blank, Bin/binary>>, Parent) when ?IS_BLANK(Blank) ->
    decode_tag_content(Bin, Parent);
decode_tag_content(<<"</", Bin1/binary>>, Parent) ->
    Len = size(Parent), 
    case binary:split(Bin1, <<">">>) of
        [<<Parent:Len/binary, Blank/binary>>, Bin] ->
            <<>> = trim_left(Blank),
            {[], Bin}
    end;
decode_tag_content(<<"<", _/binary>> = Bin, Parent) ->
    {Tag, Rest1} = decode_tag(Bin),
    {Content, Rest2} = decode_tag_content(Rest1, Parent),
    {[Tag|Content], Rest2};
decode_tag_content(Bin, Parent) ->
    [Text, Rest] = binary:split(Bin, <<"</", Parent/binary, ">">>),
    {[Text], Rest}.

trim_left(<<" ", Bin/binary>>) -> trim_left(Bin);
trim_left(<<"\n", Bin/binary>>) -> trim_left(Bin);
trim_left(<<"\t", Bin/binary>>) -> trim_left(Bin);
trim_left(<<"\r", Bin/binary>>) -> trim_left(Bin);
trim_left(Bin) -> Bin.

trim_right(<<>>) -> <<>>;
trim_right(Bin) -> trim_right(Bin, binary_part(Bin,{0, byte_size(Bin)-1})).

trim_right(Bin, Rest) when Bin =:= <<Rest/binary, " ">> -> trim_right(Rest);
trim_right(Bin, Rest) when Bin =:= <<Rest/binary, "\n">> -> trim_right(Rest);
trim_right(Bin, Rest) when Bin =:= <<Rest/binary, "\t">> -> trim_right(Rest);
trim_right(Bin, Rest) when Bin =:= <<Rest/binary, "\r">> -> trim_right(Rest);
trim_right(Bin, _) -> Bin.


%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_tag_test_() ->
    [
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html ></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html ></html >\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html/>\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html />\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html/>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html />\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"\n\n\n<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        decode(<<"\n<html></html>\n">>))
    ].

decode_content_test_() ->
    [
    ?_assertEqual({<<"html">>, [], [<<"Body">>]},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html >Body</html>\n">>)),
    ?_assertEqual({<<"html">>, [], [{<<"head">>, [], []}]},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html><head></head></html>\n">>))%,
%    ?_assertEqual({<<"html">>, [], [<<"TextBefore">>, {<<"head">>, [], []}, <<"TextAfter">>]},
%        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html >TextBefore<head></head>TextAfter</html>\n">>))
    ].

decode_attributes_test_() -> 
    [
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\"></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\" ></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []},
        decode(<<"<html xmlns='w3c' />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\"/>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k  =  \"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []},
        decode(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html k=\"v\" />\n">>))
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
