-module(ext_tuntap_port).

-export([start/1, start/2, send/2]).


start(Name) ->
    start(Name, []).


start(Name, Options) ->
    Name1 = name_to_binary(Name),
    Args = [Name1],

    Mode =
        case proplists:get_value(mode, Options) of
            tun ->
                <<"tun">>;
            tap ->
                <<"tap">>;
            undefined ->
                case Name1 of
                    <<"tun", _/binary>> ->
                        <<"tun">>;
                    <<"tap", _/binary>> ->
                        <<"tap">>
                end
        end,

    Args1 = [<<"--mode=", Mode/binary>>|Args],
    Args2 =
        case proplists:is_defined(multi_queue, Options) of
            true ->
                [<<"--multi_queue">>|Args1];
            false ->
                Args1
        end,

    Args3 =
        case proplists:lookup(netns, Options) of
            none ->
                Args2;
            {netns, Netns} ->
                Netns1 = name_to_binary(Netns),
                [<<"--netns=", Netns1/binary>>|Args2]
        end,

    Dir = code:lib_dir(extralib, priv),

    Pid =
        erlang:open_port(
          {spawn_executable, [Dir, "/ext_tuntap_port"]},
          [{packet, 2},
           binary,
           {args, Args3}]),

    {ok, Pid}.


name_to_binary(Name)
  when is_binary(Name) ->
    Name;
name_to_binary(Name)
  when is_atom(Name) ->
    atom_to_binary(Name, latin1);
name_to_binary(Name)
  when is_list(Name) ->
    list_to_binary(Name).


send(Port, Data) ->
    erlang:port_command(Port, Data).
