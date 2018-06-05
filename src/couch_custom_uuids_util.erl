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
-export([to_base_string/2, to_base_string/3, pad_string/3, pad_string/4]).
-export([clamp/3, clamp/2, substring/2, substring/3]).
-export([int_pow/2]).

get_base_char(Index, Lookup) ->
  [lists:nth(Index + 1, Lookup)].

to_base_string(Number, Base) when is_integer(Number), is_integer(Base), Base =< 64 ->
  Lookup = [
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "-",
    "_"
  ],
  to_base_string(Number, Base, Lookup).

to_base_string(Number, Base, Lookup) when is_integer(Number), is_integer(Base), Base > 1 ->
  case Number < Base of
    true ->
      get_base_char(Number, Lookup);
    false ->
      to_base_string(Number div Base, Base, Lookup) ++ get_base_char(Number rem Base, Lookup)
  end.

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
  string:left(String, Length, PadStr);
pad_string(String, Length, PadStr, right) ->
  string:right(String, Length, PadStr).

int_pow(Number, Power) ->
  trunc(math:pow(Number, Power)).