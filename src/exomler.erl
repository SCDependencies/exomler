-module(exomler).

%% API
-export([decode/1]).
-export([encode/1]).

%% API
decode(Bin) ->
    exomler_dom_decoder:decode(Bin).

encode({Tag, Attrs, Content}) ->
    exomler_dom_encoder:encode({Tag, Attrs, Content}).
