-module(ext_tuntap_nic).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([tx_filter/1]).


start_link(Name, Options) ->
    gen_server:start_link(?MODULE, [Name, Options], []).


init([Name, Options]) ->
    {ok, RX} = ext_tuntap_rx_dispatcher:start_link(Name, Options),
    RXBuffer = gen_server:call(RX, get_buffer),
    {ok, TX} = ext_tuntap_tx_dispatcher:start_link(RXBuffer),
    TXBuffer = gen_server:call(TX, get_buffer),

    {ok,
     #{rx    => RX,
       rxbuf => RXBuffer,
       tx    => TX,
       txbuf => TXBuffer,
       rxfilter_count => 0
      }}.


handle_call({attach_rx_filter, Pid, {M,F,A}}, _From,
            State = #{rxfilter_count := Count,
                      rx             := RX
                     }) ->
    case gen_server:call(RX, {attach_filter, Count, Pid, {M,F,A}}) of
        ok ->
            {reply, {ok, Count}, State#{rxfilter_count := Count + 1}};
        error ->
            {reply, error, State}
    end;
handle_call({replace_rx_filter, ID, {M,F,A}}, _From, State = #{rx := RX}) ->
    Reply = gen_server:call(RX, {replace_rx_filter, ID, {M,F,A}}),
    {reply, Reply, State};
handle_call({detach_rx_filter, ID}, _From, State = #{rx := RX}) ->
    Reply = gen_server:call(RX, {detach_rx_filter, ID}),
    {reply, Reply, State};
handle_call(get_tx_filter, _From, State = #{txbuf := TXBuffer}) ->
    {reply, {TXBuffer, {?MODULE, tx_filter, []}}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


tx_filter({_, {data, Data}}) ->
    {ok, Data}.
