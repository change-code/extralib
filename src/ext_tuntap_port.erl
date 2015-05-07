-module(ext_tuntap_port).

-export([start/1, send/2]).


start(Options) ->
    Args = [<<"tap0">>],

    Mode =
        case proplists:get_value(mode, Options, tap) of
            tun ->
                <<"tun">>;
            tap ->
                <<"tap">>
        end,

    Args1 = [<<"--mode=", Mode/binary>>|Args],
    Args2 =
        case proplists:is_defined(multi_queue, Options) of
            true ->
                [<<"--multi_queue">>|Args1];
            false ->
                Args1
        end,

    Dir = code:lib_dir(extralib, priv),

    Pid =
        erlang:open_port(
          {spawn_executable, [Dir, "/ext_tuntap_port"]},
          [{packet, 2},
           binary,
           {args, Args2}]),

    {ok, Pid}.


send(Port, Data) ->
    erlang:port_command(Port, Data).
