-module(jaro).

%% Taken from 'Elixir.String' 
%% (pretty-printed from abstract forms in beam file)
%%
%% Functions copied from 'Elixir.String':
%%
%% * jaro_distance/2 --> renamed to distance/2
%% * chars_and_length/1
%% * match/3,4,5
%% * submatch/5
%% * detect/5 --> Call to 'Elixir.Enum':reverse/2 replaced by lists:reverse/2
%% * proceed/2
%%
%% graphemes/1 is replaced by a function which calls string:to_graphemes/1.

-export([distance/2]).

%% @doc Computes the Jaro Distance between two strings.
%%      The value is a float in the range [0.0, 1.0]
-spec distance(string(), string()) -> float().
distance(_string@1, _string@1) -> 1.0;
distance(__string@1, <<>>) -> 0.0;
distance(<<>>, __string@1) -> 0.0;
distance(_string1@1, _string2@1) ->
    {_chars1@1, _len1@1} = chars_and_length(_string1@1),
    {_chars2@1, _len2@1} = chars_and_length(_string2@1),
    case match(_chars1@1, _len1@1, _chars2@1, _len2@1) of
      {0, __trans@1} -> 0.0;
      {_comm@1, _trans@1} ->
	  (_comm@1 / _len1@1 + _comm@1 / _len2@1 +
	     (_comm@1 - _trans@1) / _comm@1)
	    / 3
    end.

chars_and_length(_string@1) ->
    _chars@1 = graphemes(_string@1),
    {_chars@1, erlang:length(_chars@1)}.

match(_chars1@1, _chars2@1, _lim@1) ->
    match(_chars1@1, _chars2@1, {0, _lim@1}, {0, 0, -1}, 0).
match(_chars1@1, _len1@1, _chars2@1, _len2@1) ->
    case _len1@1 < _len2@1 of
      false -> match(_chars2@1, _chars1@1, _len1@1 div 2 - 1);
      true -> match(_chars1@1, _chars2@1, _len2@1 div 2 - 1)
    end.
match([_char@1 | _rest@1], _chars@1, _range@1, _state@1,
      _idx@1) ->
    {_chars@2, _state@2} = submatch(_char@1, _chars@1,
				    _range@1, _state@1, _idx@1),
    case _range@1 of
      {_lim@1, _lim@1} ->
	  match(_rest@1, erlang:tl(_chars@2), _range@1, _state@2,
		_idx@1 + 1);
      {_pre@1, _lim@2} ->
	  match(_rest@1, _chars@2, {_pre@1 + 1, _lim@2}, _state@2,
		_idx@1 + 1)
    end;
match([], _, _, {_comm@1, _trans@1, _}, _) ->
    {_comm@1, _trans@1}.

submatch(_char@1, _chars@1, {_pre@1, _} = _range@1,
	 _state@1, _idx@1) ->
    case detect(_char@1, _chars@1, _range@1) of
      nil -> {_chars@1, _state@1};
      {_subidx@1, _chars@2} ->
	  {_chars@2,
	   proceed(_state@1, _idx@1 - _pre@1 + _subidx@1)}
    end.

detect(_char@1, _chars@1, {_pre@1, _lim@1}) ->
    detect(_char@1, _chars@1, _pre@1 + 1 + _lim@1, 0, []).
detect(__char@1, __chars@1, 0, __idx@1, __acc@1) -> nil;
detect(__char@1, [], __lim@1, __idx@1, __acc@1) -> nil;
detect(_char@1, [_char@1 | _rest@1], __lim@1, _idx@1,
       _acc@1) ->
    {_idx@1,
     lists:reverse(_acc@1, [nil | _rest@1])};
detect(_char@1, [_other@1 | _rest@1], _lim@1, _idx@1,
       _acc@1) ->
    detect(_char@1, _rest@1, _lim@1 - 1, _idx@1 + 1,
	   [_other@1 | _acc@1]).

proceed({_comm@1, _trans@1, _former@1}, _current@1) ->
    case _current@1 < _former@1 of
      false -> {_comm@1 + 1, _trans@1, _current@1};
      true -> {_comm@1 + 1, _trans@1 + 1, _current@1}
    end.

%% ------

%% The Elixir graphemes/1 is calling 'Elixir.String.Unicode':graphemes/1
graphemes(String) ->
    string:to_graphemes(String).
