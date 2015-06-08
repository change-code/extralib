-module(ext_stream).

-export(
   [ nil/0,
     cons/2,
     from_list/1,
     to_list/1,
     append/2,
     reverse/1,
     reverse/2]).

-export_type([stream/1]).

-type cell(T) :: 'nil' | {'cons', T, stream(T)}.
-type stream(T) :: fun(() -> cell(T)).


-spec nil() -> 'nil'.
nil() ->
    nil.


-spec cons(Elem, Stream1) -> Stream2 when
      Elem    :: T,
      Stream1 :: stream(T),
      Stream2 :: stream(T),
      T       :: term().
cons(Elem, Stream) ->
    fun () ->
            {cons, Elem, Stream}
    end.


-spec from_list(List) -> Stream when
      List   :: [T],
      Stream :: stream(T),
      T      :: term().
from_list(List) ->
    fun () ->
            case List of
                [] ->
                    nil;
                [H|T] ->
                    {cons, H, from_list(T)}
            end
    end.


-spec to_list(Stream) -> List when
      List   :: [T],
      Stream :: stream(T),
      T      :: term().
to_list(Stream) ->
    case Stream() of
        nil ->
            [];
        {cons, H, T} ->
            [H|to_list(T)]
    end.


-spec append(Stream1, Stream2) -> Stream3 when
      Stream1 :: stream(T),
      Stream2 :: stream(T),
      Stream3 :: stream(T),
      T       :: term().
append(A, B) ->
    fun () ->
            case A() of
                nil ->
                    B();
                {cons, H, T} ->
                    {cons, H, append(T, B)}
            end
    end.


-spec reverse(Stream1) -> Stream2 when
      Stream1 :: stream(T),
      Stream2 :: stream(T),
      T       :: term().
reverse(Stream) ->
    reverse(Stream, fun nil/0).


-spec reverse(Stream1, Tail) -> Stream2 when
      Stream1 :: stream(T),
      Tail    :: stream(T),
      Stream2 :: stream(T),
      T       :: term().
reverse(Stream, Tail) ->
    case Stream() of
        nil ->
            Tail;
        {cons, H, T} ->
            reverse(T, cons(H, Tail))
    end.
