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
  max_sequence_increase
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
  Result = list_to_binary(State#state.prefix ++ format(State#state.sequence, State)),
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
  ReadingPaddingStr = couch_config:get("custom_uuids", "padding_str", "0"),
  Base =
    case ReadingBase > 36 of
      true ->
        36;
      false ->
        case ReadingBase < 10 of
          true ->
            10;
          false ->
            ReadingBase
        end
    end,
  Length =
    case ReadingLength < 6 of
      true ->
        6;
      false ->
        ReadingLength
    end,
  PaddingStr =
    case length(ReadingPaddingStr) == 1 of
      true ->
        ReadingPaddingStr;
      false ->
        [H | _] = ReadingPaddingStr,
        H
    end,

  MaxSequenceIncrease = pow(Base, 3),
  MaxSequence = pow(Base, 6) - MaxSequenceIncrease,
  PrefixLength = Length - 6,
  Prefix = random(Base, PrefixLength),
  #state{
    base = Base,
    padding_str = PaddingStr,
    sequence = inc(MaxSequenceIncrease),
    max_sequence = MaxSequence,
    max_sequence_increase = MaxSequenceIncrease,
    prefix = Prefix
  }.

random(_State, 0) ->
  "";
random(Base, 1) ->
  format(crypto:rand_uniform(1, Base), Base);
random(Base, Length) ->
  Max = pow(Base, Length),
  Min = pow(Base, Length - 1),
  format(crypto:rand_uniform(Min, Max), Base).

format(Number, #state{base = Base, padding_str = PaddingStr}) ->
  Format = "~6." ++ integer_to_list(Base) ++ "." ++ PaddingStr ++ "b",
  io_lib:format(Format, [Number]);
format(Number, Base) ->
  integer_to_list(Number, Base).

pow(Base, Pow) ->
  trunc(math:pow(Base, Pow)).