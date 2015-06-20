-module(rawstring).

-export([hello/0]).

hello() ->
    "\\r\\n\"".
