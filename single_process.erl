-module(single_process).
-export([count_words/0]).

%----------------------------------------------
% Public API
%----------------------------------------------

count_words() ->
  % getting the current time in milliseconds
  Begining = get_timestamp(),

  % open the file and getting the 'Device'
  Device = open_file(),

  % starting reading the book line by line
  Result = read_lines(Device, #{}),

  % calculating the elapsed time
  ElapsedTime = (get_timestamp() - Begining) / 1000,
  io:format("elapsed time: ~p seconds ~n", [ElapsedTime]),

  Result.

%----------------------------------------------
% Internal functions (not exported)
%----------------------------------------------

% This function will open the "big_text.txt" file and return the related Device. In Erlang this
% Device is like a process in charge of reading our file
open_file() ->
  {ok, Device} = (file:open("book.txt", read)),
  Device.

read_lines(Device, MapAcc) ->

  % we are going to read the file line by line
  case file:read_line(Device) of
    {ok, Line} ->
      % split the line in words
      ListOfWords = re:split(Line, " "),

      % apply the `add_to_map/2` function to each word and adding it to the accumulated map
      % this will return a new map with the new words added
      NewMapAcc = lists:foldl(fun add_to_map/2, MapAcc, ListOfWords),

      % read the next line
      read_lines(Device, NewMapAcc);
    eof ->
      % eof is returned when the text is finished, if that is the case we return the accumulated map
      MapAcc
  end.

% function applied to each word, it converts the words to uppercase in order to deal `Sofia` and `sofia`
% as the same word ('SOFIA')
add_to_map(Word, Map) ->
  % convert the word to uppercase
  UpperWord = string:uppercase(Word)
,

  % check how many times that word exist in the map
  Value = maps:get(UpperWord, Map, 0),

  % update the value and return a new map
  maps:put(UpperWord, Value + 1, Map).

% helper in order to get the current time in milliseconds
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec) * 1000 + round(Micro / 1000).
