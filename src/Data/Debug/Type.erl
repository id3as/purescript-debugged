-module(data_debug_type@foreign).

-export([ tupleListToMap/1
        , listToTuple/1
        ]).

tupleListToMap(List) ->
    maps:from_list(List).

listToTuple(List) ->
    list_to_tuple(List).
