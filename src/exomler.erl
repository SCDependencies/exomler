-module(exomler).

%% API
-export([decode/1, decode_document/1]).
-export([encode/1, encode_document/1]).

%% API
decode_document(Bin) ->
    exomler_dom_decoder:decode_document(Bin).

decode(Bin) ->
    exomler_dom_decoder:decode(Bin).

encode_document(Entity) ->
    exomler_dom_encoder:encode_document(Entity).

encode(Entity) ->
    exomler_dom_encoder:encode(Entity).
