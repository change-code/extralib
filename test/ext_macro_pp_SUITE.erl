-module(ext_macro_pp_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_hello_world,
     test_macro_define,
     test_file_include,
     test_file_attribute].

test_hello_world(Config) ->
    test_epp("hello.erl", [], Config).

test_macro_define(Config) ->
    test_epp("macrodef.erl", [], Config).

test_file_include(Config) ->
    test_epp("fileinc.erl", [], Config).

test_file_attribute(Config) ->
    test_epp("fileattr.erl", [], Config).

test_epp(FileName, Options, Config) ->
    FullName = filename:join(?config(data_dir, Config), FileName),
    {ok, _} = Expected =
        epp:parse_file(FullName, Options),
    io:format("~p~n", [Expected]),
    Expected =
        ext_epp:parse_file(FullName, [{passes, [ext_macro_pp]}|Options]),
    ok.
