%%%-------------------------------------------------------------------
%%% @author ahmetturk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2018 16:17
%%%-------------------------------------------------------------------
-module(couch_custom_uuids).
-author("ahmetturk").

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([new/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-include_lib("couch/include/couch_db.hrl").
-record(state, {
  base,
  padding_str,
  sequence,
  prefix,
  max_sequence,
  max_sequence_increase,
  volatile_length
}).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

new() ->
  gen_server:call(?MODULE, create).

init([]) ->
  ok = couch_config:register(
    fun("custom_uuids", _) -> gen_server:cast(?MODULE, change) end
  ),
  {ok, new_state()}.

terminate(_Reason, _State) ->
  ok.

handle_call(create, _From, State) ->
  Sequence = State#state.sequence,
  Result = list_to_binary(
    State#state.prefix ++
    couch_custom_uuids_util:pad_string(
      couch_custom_uuids_util:to_base_string(State#state.sequence, State#state.base),
      State#state.volatile_length,
      State#state.padding_str
    )
  ),
  case Sequence >= State#state.max_sequence of
    true ->
      {reply, Result, new_state()};
    _ ->
      {reply, Result, State#state{sequence = Sequence + inc(State#state.max_sequence_increase)}}
  end.

handle_cast(change, _State) ->
  {noreply, new_state()};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

inc(MaxSequenceIncrease) ->
  crypto:rand_uniform(1, MaxSequenceIncrease).

new_state() ->
  ReadingBase = list_to_integer(couch_config:get("custom_uuids", "base", "16")),
  ReadingLength = list_to_integer(couch_config:get("custom_uuids", "length", "6")),
  ReadingPrefixLength = list_to_integer(couch_config:get("custom_uuids", "prefix_length", "0")),
  ReadingPaddingStr = couch_config:get("custom_uuids", "padding_str", "0"),

  Base = couch_custom_uuids_util:clamp(ReadingBase, 2, 64),
  Length = couch_custom_uuids_util:clamp(ReadingLength, 6),
  PaddingStr = couch_custom_uuids_util:substring(ReadingPaddingStr, 0, 1),
  PrefixLength = couch_custom_uuids_util:clamp(ReadingPrefixLength, 0, Length - 3),

  VolatileLength = Length - PrefixLength,
  MaxSequenceIncrease = couch_custom_uuids_util:int_pow(Base, VolatileLength div 2),
  MaxSequence = couch_custom_uuids_util:int_pow(Base, VolatileLength) - MaxSequenceIncrease,
  Prefix = random(Base, PrefixLength),
  #state{
    base = Base,
    padding_str = lists:nth(1,PaddingStr),
    sequence = inc(MaxSequenceIncrease),
    max_sequence = MaxSequence,
    max_sequence_increase = MaxSequenceIncrease,
    prefix = Prefix,
    volatile_length = VolatileLength
  }.

random(_Base, 0) ->
  "";
random(Base, Length) ->
  Max = couch_custom_uuids_util:int_pow(Base, Length) - 1,
  Min = couch_custom_uuids_util:int_pow(Base, Length - 1),
  couch_custom_uuids_util:to_base_string(crypto:rand_uniform(Min, Max), Base).
