%%%-------------------------------------------------------------------
%%% @author ahmetturk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2018 16:39
%%%-------------------------------------------------------------------
-module(couch_custom_uuids_util).
-author("ahmetturk").

%% API
-export([pad_string/3, pad_string/4, to_base_string/2]).
-export([clamp/3, clamp/2, substring/2, substring/3]).
-export([int_pow/2]).

clamp(Number, Min, Max) ->
  case Number > Max of
    true ->
      Max;
    false ->
      case Number < Min of
        true ->
          Min;
        false ->
          Number
      end
  end.

clamp(Number, Min) ->
  case Number < Min of
    true ->
      Min;
    false ->
      Number
  end.

substring(String, Start) ->
  {_, SecondPart} = lists:split(Start, String),
  SecondPart.

substring(String, Start, Length) ->
  SecondPart = substring(String, Start),
  {FirstPart, _} = lists:split(Length, SecondPart),
  FirstPart.

pad_string(String, Length, PadStr) ->
  pad_string(String, Length, PadStr, left).
pad_string(String, Length, PadStr, left) ->
  string:right(String, Length, PadStr);
pad_string(String, Length, PadStr, right) ->
  string:left(String, Length, PadStr).

int_pow(Number, Power) ->
  trunc(math:pow(Number, Power)).

to_base_string(Number, Base) ->
  string:replace(string:replace(base_util:number_to_base_string(Number, Base), "+", "-"), "/", "_").
