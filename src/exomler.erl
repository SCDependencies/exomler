-module(exomler).

%% API
-export([parse/1]).

-define(IS_BLANK(Blank), Blank == $\s 
    orelse Blank == $\n
    orelse Blank == $\t
    orelse Blank == $\r).

-define(IS_QUOTE(Quote), Quote == $" orelse Quote == $').


%% API
parse(Bin) when is_binary(Bin) ->
    Bin1 = skip_declaration(Bin), 
    {Tag, Rest} = tag(Bin1),
    <<>> = trim_head(Rest), 
    Tag.

%% internal
skip_declaration(<<"<?xml", Bin/binary>>) ->
    [_, Rest] = binary:split(Bin, <<"?>">>),
    trim_head(Rest), 
    skip_declaration(Rest);
skip_declaration(<<"<!", Bin/binary>>) ->
    [_, Rest] = binary:split(Bin, <<">">>),
    trim_head(Rest);
skip_declaration(<<"<", _/binary>> = Bin) ->
    Bin;
skip_declaration(<<_, Bin/binary>>) ->
    skip_declaration(Bin).

trim_head(<<" ", Bin/binary>>) -> trim_head(Bin);
trim_head(<<"\n", Bin/binary>>) -> trim_head(Bin);
trim_head(<<"\t", Bin/binary>>) -> trim_head(Bin);
trim_head(<<"\r", Bin/binary>>) -> trim_head(Bin);
trim_head(Bin) -> Bin.

trim_tail(<<>>) -> <<>>;
trim_tail(Bin) -> trim_tail(Bin, binary_part(Bin,{0, byte_size(Bin)-1})).

trim_tail(Bin, Rest) when Bin =:= <<Rest/binary, " ">> -> trim_tail(Rest);
trim_tail(Bin, Rest) when Bin =:= <<Rest/binary, "\n">> -> trim_tail(Rest);
trim_tail(Bin, Rest) when Bin =:= <<Rest/binary, "\t">> -> trim_tail(Rest);
trim_tail(Bin, Rest) when Bin =:= <<Rest/binary, "\r">> -> trim_tail(Rest);
trim_tail(Bin, _) -> Bin.

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
    case binary:split(TagHeader, [<<" ">>]) of
        [Tag] -> {Tag, []};
        [Tag, Attrs] -> {Tag, tag_attrs(Attrs)}
    end.

tag_attrs(<<Blank, Attrs/binary>>) when ?IS_BLANK(Blank) -> 
    tag_attrs(Attrs);
tag_attrs(<<>>) ->
    [];
tag_attrs(Attrs) ->
    case binary:split(Attrs, <<"=">>) of
        [Key, Value1] ->
            [Value2, Rest] = attr_value(Value1),
            [{trim_tail(Key), Value2}|tag_attrs(Rest)]
    end.

attr_value(<<Blank, Value/binary>>) when ?IS_BLANK(Blank) ->
    attr_value(Value);
attr_value(<<Quote, Value/binary>>) when ?IS_QUOTE(Quote) ->
    binary:split(Value, <<Quote>>);
attr_value(<<>>) ->
    <<>>.

tag_content(<<Blank, Bin/binary>>, Parent) when ?IS_BLANK(Blank) ->
    tag_content(Bin, Parent);
tag_content(<<"</", Bin1/binary>>, Parent) ->
    Len = size(Parent), 
    <<Parent:Len/binary, ">", Bin/binary>> = Bin1,
    {[], Bin};
tag_content(<<"<", _/binary>> = Bin, Parent) ->
    {Tag, Rest1} = tag(Bin),
    {Content, Rest2} = tag_content(Rest1, Parent),
    {[Tag|Content], Rest2};
tag_content(Bin, Parent) ->
    [Text, Rest] = binary:split(Bin, <<"</", Parent/binary, ">">>),
    {[Text], Rest}.



%% Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

parse_test_() -> 
    [
    ?_assertEqual({<<"html">>, [], []},
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        parse(<<"\n\n\n<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        parse(<<"\n<html></html>\n">>)),
    ?_assertEqual({<<"html">>, [], []},
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html ></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\"></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\" ></html>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []},
        parse(<<"<html xmlns='w3c' />\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html/>\n">>)),
    ?_assertEqual({<<"html">>, [], []}, 
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\"/>\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k  =  \"v\" />\n">>)),
    ?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []},
        parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html k=\"v\" />\n">>))
  ].

-endif.
