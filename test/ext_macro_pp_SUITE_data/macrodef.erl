-module(macrodef).

-export([hello/0]).

-ifndef(A).
-define(X, 1).
-else.
-define(X, 2).
-endif.

-define(A, 3).

-ifdef(X).
-define(Y, 4).
-else.
-define(Y, 5).
-endif.

-undef(A).

-ifdef(A).
-define(Z, 6).
-else.
-define(Z, 7).
-endif.


hello() ->
    ?X + ?Y + ?Z.
