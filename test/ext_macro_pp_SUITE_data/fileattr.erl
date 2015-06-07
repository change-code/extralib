-module(fileattr).

-file("filename.erl", 1).

-export([hello/0]).

hello() ->
    world.
