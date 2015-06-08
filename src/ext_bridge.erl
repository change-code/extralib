-module(ext_bridge).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([rx_filter/1]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    {ok,
     #{nics => dict:new(),
       macs => dict:new()}}.

handle_call({add_nic, Pid}, _From, State = #{nics := NICs}) ->
    case dict:find(Pid, NICs) of
        {ok, _} ->
            {reply, already_exist, State};
        error ->
            {TXPid, TXFilter} = gen_server:call(Pid, get_tx_filter),
            RXID = gen_server:call(Pid, {attach_rx_filter, self(), {?MODULE, rx_filter, []}}),
            {reply, ok, State#{nics := dict:store(Pid, {RXID, TXPid, TXFilter, sets:new()}, NICs)}}
    end;
handle_call({delete_nic, Pid}, _From, State = #{nics := NICs, macs := MACs}) ->
    case dict:find(Pid, NICs) of
        {ok, {_,_,_,NICMacs}} ->
            MACs1 =
                sets:fold(
                  fun (Mac, MacsAcc) ->
                          dict:erase(Mac, MacsAcc)
                  end,
                  MACs,
                  NICMacs),
            {reply, ok, State#{nics := dict:erase(Pid, NICs), macs := MACs1}};
        error ->
            {reply, not_found, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({From, {data, <<_:7, 1:1, _/binary>>=Data}}=Packet, State) ->
    %% multicast
    State1 = #{nics:=NICs} = record_mac(From, Data, State),
    broadcast_packet(Packet, NICs),
    {noreply, State1};
handle_info({From, {data, <<DST:48, _/binary>>=Data}}=Packet, State) ->
    %% unicast
    State1 = #{nics:=NICs, macs:=MACs} = record_mac(From, Data, State),
    case dict:find(DST, MACs) of
        {ok, Pid} ->
            {_, TxPid, TxFilter, _} = dict:fetch(Pid, NICs),
            send_packet(Packet, TxPid, TxFilter);
        error ->
            broadcast_packet(Packet, NICs)
    end,
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


record_mac(_From, <<_:48, _:7, 1:1, _/binary>>, State) ->
    %% multicast
    State;
record_mac(From, <<_:48, Src:48, _/binary>>, State = #{macs:=MACs, nics:=NICs}) ->
    %% unicast
    case dict:find(Src, MACs) of
        {ok, From} ->
            State;
        {ok, Pid} ->
            MACs1 = dict:store(Src, From, MACs),
            {RxID1, TxPid1, TxFilter1, NICMacs1} = dict:fetch(Pid, NICs),
            NICs1 = dict:store(Pid, {RxID1, TxPid1, TxFilter1, sets:del_element(Src, NICMacs1)}, NICs),
            {RxID2, TxPid2, TxFilter2, NICMacs2} = dict:fetch(From, NICs1),
            NICs2 = dict:store(From, {RxID2, TxPid2, TxFilter2, sets:add_element(Src, NICMacs2)}, NICs1),
            State#{macs:=MACs1, nics:=NICs2};
        error ->
            MACs1 = dict:store(Src, From, MACs),
            {RxID, TxPid, TxFilter, NICMacs} = dict:fetch(From, NICs),
            NICs1 = dict:store(From, {RxID, TxPid, TxFilter, sets:add_element(Src, NICMacs)}, NICs),
            State#{macs := MACs1, nics := NICs1}
    end.


send_packet(Packet, To, {M, F, A}) ->
    case apply(M, F, [Packet|A]) of
        skip ->
            ok;
        {ok, Data} ->
            To ! {self(), {data, Data}}
    end.


broadcast_packet(Packet, NICs) ->
    dict:map(
      fun(_, {_, TxPid, TxFilter, _}) ->
              send_packet(Packet, TxPid, TxFilter)
      end,
      NICs).


rx_filter({_, {data, Data}}) ->
    {ok, Data}.
