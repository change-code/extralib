-module(ext_syntax_trans).

-export([parse_transform/2]).


parse_transform(Forms, Options) ->
    Includes = proplists:get_all_values(i, Options),
    Macros =
        [ case Macro of
              {d, K} -> K;
              {d, K, V} -> {K, V}
          end
          || Macro <- proplists:lookup_all(d, Options)],

    [{attribute, _, file, {FileName, 1}}|_] = Forms,
    [{Module, Fun, ParserOptions}] =
        [ Parser
          || {attribute, _, compile, {parser, Parser}} <- Forms ],

    {ok, Forms1} =
        Module:Fun(
          FileName,
          [{includes, Includes}, {macros, Macros}
           |ParserOptions]),

    Forms2 =
        [ Form
          || Form <- Forms1,
             case Form of
                 {attribute, _, compile, {parse_transform, _}} ->
                     false;
                 {attribute, _, compile, {parser, _}} ->
                     false;
                 _ ->
                     true
             end ],

    case proplists:get_bool(verbose, Options) of
        true ->
            Tree = erl_syntax:form_list(Forms2),
            io:format(
              "Source code after ext_syntax_trans:~n~s~n",
              [erl_prettypr:format(Tree)]);
        _ ->
            ok
    end,

    Forms2.
