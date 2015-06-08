-module(ext_packet_buffer).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link(Options) ->
    start_link(self(), Options).

start_link(Parent, Options) ->
    gen_server:start_link(?MODULE, [Parent, Options], []).


init([Parent, Options]) ->
    State = #{
      owner  => Parent,
      length => 0,
      size   => proplists:get_value(size, Options, 32),
      queue  => queue:new(),
      active => proplists:get_value(active, Options, false)
     },
    {ok, State}.


handle_call({setopts, Opts}, {Owner, _}, State = #{owner:=Owner}) ->
    State1 = update_active(Opts, State),
    State2 = send(State1),
    State3 = update_size(Opts, State2),
    State4 = drop(State3),
    {reply, ok, State4};
handle_call({controlling_process, NewPid}, {Owner, _}, State = #{owner:=Owner}) ->
    link(NewPid),
    unlink(Owner),
    {reply, ok, State#{owner:=NewPid}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({_, {data, _}}=Packet, State) ->
    State1 = push(Packet, State),
    State2 = send(State1),
    State3 = drop(State2),
    {noreply, State3};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


push(Packet, State = #{queue:=Queue, length:=Length}) ->
    State#{
      queue  := queue:in(Packet, Queue),
      length := Length + 1
     }.


send(State = #{active:=false}) ->
    State;
send(State = #{length:=0}) ->
    State;
send(State = #{active:=true, owner:=Owner, queue:=Queue, length:=Length}) ->
    {{value, Packet}, Queue1} = queue:out(Queue),
    Owner ! Packet,
    send(State#{queue:=Queue1, length:=Length-1});
send(State = #{active:=1, owner:=Owner, queue:=Queue, length:=Length}) ->
    {{value, Packet}, Queue1} = queue:out(Queue),
    Owner ! Packet,
    State#{active:=false, queue:=Queue1, length:=Length-1};
send(State = #{active:=N, owner:=Owner, queue:=Queue, length:=Length})
  when N > 1 ->
    {{value, Packet}, Queue1} = queue:out(Queue),
    Owner ! Packet,
    State#{active:=N-1, queue:=Queue1, length:=Length-1}.


drop(State = #{size:=Size, length:=Length})
  when Length =< Size ->
    State;
drop(State = #{queue:=Queue, length:=Length}) ->
    State#{
      queue:=queue:drop_r(Queue),
      length:=Length - 1}.


update_active(Opts, State = #{active:=Active}) ->
    NewActive =
        case proplists:lookup(active, Opts) of
            error ->
                Active;
            {active, false} ->
                false;
            {active, true} ->
                true;
            {active, N} ->
                case Active of
                    true ->
                        N;
                    false ->
                        N;
                    _ ->
                        Active + N
                end
        end,
    State#{active:=NewActive}.


update_size(Opts, State = #{size:=Size}) ->
    NewSize = proplists:get_value(size, Opts, Size),
    State#{size:=NewSize}.
