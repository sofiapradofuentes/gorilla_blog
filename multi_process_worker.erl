-module(multi_process_worker).
-export([init/0]).

%----------------------------------------------
% Public API
%----------------------------------------------

init() ->
  % starting the loop with an empty accumulated map
  loop(#{}).

%----------------------------------------------
% Internal functions (not exported)
%----------------------------------------------

% This function is waiting for lines to process
loop(MapAcc) ->
  receive
    {CallerPID, eof} ->
      % if we received and eof message that means we have to return the result map to the caller process
      CallerPID ! {self(), MapAcc},
      ok;
    {_CallerPID, Line} ->
      % we received a line to process, lets split it in words
      ListOfWords = re:split(Line," "),

      % apply the `add_to_map/2` function to each word and adding it to the accumulated map
      % this will return a new map with the new words added
      NewMapAcc = lists:foldl(fun add_to_map/2, MapAcc, ListOfWords),

      % wait for more lines with the accumulate map updated
      loop(NewMapAcc)
    end.

  % function applied to each word, it converts the words to uppercase in order to deal `Sofia` and `sofia`
  % as the same word ('SOFIA')
  add_to_map(Word, Map) ->
    % convert the word to uppercase
    UpperWord = string:uppercase(Word),

    % check how many times that word exist in the map
    Value = maps:get(UpperWord, Map, 0),

    % update the value and return a new map
    maps:put(UpperWord, Value + 1, Map).
