-module(exomler_dom_encoder).

%% API
-export([encode/1]).

-include("exomler.hrl").

%% API
encode({Tag, Attrs, Content}) ->
    tag({Tag, Attrs, Content}).

%% internal
tag({Tag, Attrs, Content}) ->
    BinAttrs = tag_attrs(Attrs),
    BinContent = << <<(content(SubTag))/binary>> || SubTag <- Content>>,
    Tag1 = ltrim(Tag),
    <<"<", Tag1/binary, BinAttrs/binary, ">", BinContent/binary, 
        "</", Tag1/binary, ">">>.

tag_attrs(Attrs) ->
    tag_attrs(Attrs, <<>>).

tag_attrs([{Key, Value}|Tail], EncodedAttrs) ->
    EncodedAttr = <<" ", Key/binary, "=\"",Value/binary, "\"">>, 
    tag_attrs(Tail, <<EncodedAttrs/binary, EncodedAttr/binary>>);
tag_attrs([], EncodedAttrs) ->
    EncodedAttrs.

content(Tuple) when is_tuple(Tuple) ->
    tag(Tuple);
content(Binary) when is_binary(Binary) ->
    escape(Binary).

ltrim(<<Blank, Bin/binary>>) when ?IS_BLANK(Blank) ->
    ltrim(Bin);
ltrim(Bin) -> 
    Bin.

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


%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

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
