-module(exomler).

%% API
-export([encode/1]).
-export([decode/1]).

%% API
encode({Tag, Attrs, Content}) ->
    exomler_dom_encoder:start({Tag, Attrs, Content}).

decode(Bin) ->
    exomler_dom_decoder:start(Bin).
