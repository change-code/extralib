-module(ext_packet_dispatcher).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link(Options) ->
    start_link(self(), Options).


start_link(Parent, Options) ->
    gen_server:start_link(?MODULE, [Parent, Options], []).


init([Parent, Options]) ->
    Buffer =
        case proplists:get_value(buffer, Options, false) of
            false ->
                self();
            true ->
                {ok, Pid} = ext_packet_buffer:start_link([{active, 1}]),
                Pid
        end,

    {ok,
     #{owner    => Parent,
       buffer   => Buffer,
       filters  => dict:new()
      }}.


handle_call(get_buffer, {Owner, _}, State = #{owner:=Owner, buffer:=Buffer}) ->
    {reply, Buffer, State};
handle_call({attach_filter, ID, Pid, {M,F,A}}, {Owner, _}, State = #{owner:=Owner, filters:=Filters}) ->
    {reply, ok, State#{filters:=dict:store(ID, {Pid, {M,F,A}}, Filters)}};
handle_call({replace_filter, ID, {M,F,A}}, {Owner, _}, State = #{owner:=Owner, filters:=Filters}) ->
    {reply, ok, State#{filters:=dict:update(ID, fun({R,_}) -> {R,{M,F,A}} end, Filters)}};
handle_call({detach_filter, ID}, {Owner, _}, State = #{owner:=Owner, filters:=Filters}) ->
    {reply, ok, State#{filters:=dict:erase(ID, Filters)}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({_, {data, _}} = Packet, State = #{owner:=Owner, filters:=Filters, buffer:=Buffer}) ->
    dict:map(
      fun (_, {To, {M,F,A}}) ->
              case apply(M, F, [Packet|A]) of
                  skip ->
                      ok;
                  {ok, Data} ->
                      To ! {Owner, {data, Data}}
              end
      end,
      Filters),

    case Buffer =:= self() of
        true ->
            ok;
        false ->
            gen_server:call(Buffer, {setopts, [{active, 1}]})
    end,

    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
