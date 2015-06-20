-module(ext_rawstring_pp_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_rawstring_pp].

test_rawstring_pp(Config) ->
    File1 = filename:join(?config(data_dir, Config), "rawstring1.erl"),
    File2 = filename:join(?config(data_dir, Config), "rawstring2.erl"),
    {ok, [_|Expected]} =
        epp:parse_file(File1, []),
    io:format("~p~n", [Expected]),
    {ok, [_|Got]} =
        ext_epp:parse_file(File2, [{passes, [ext_tokenline_pp, ext_rawstring_pp]}, {scan_options, [text]}]),
    Expected = Got,     
    ok.
