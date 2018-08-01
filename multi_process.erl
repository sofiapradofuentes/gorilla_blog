-module(multi_process).
-export([count_words/0]).

%----------------------------------------------
% Public API
%----------------------------------------------

count_words() ->
  % getting the current time in milliseconds
  Begining = get_timestamp(),

  % starting a bunch of worker processes and returning a list with the process ids
  ListOfPids = create_workers(9),

  % open the file and getting the 'Device'
  Device = open_file(),

  % starting reading the book line by line.
  Result = read_lines(Device, ListOfPids, 0),

  % calculating the elapsed time
  ElapsedTime = (get_timestamp() - Begining) / 1000,
  io:format("elapsed time: ~p seconds ~n", [ElapsedTime]),

  Result.

%----------------------------------------------
% Internal functions (not exported)
%----------------------------------------------

% this function will create N number of workers. Each worker will start with the function
% `init/0` defined in the `module multi_process_worker`.
create_workers(NumberOfWorkers) ->
  [spawn(multi_process_worker, init, []) || _X <- lists:seq(1, NumberOfWorkers)].

% This function will open the "big_text.txt" file and return the related Device. In Erlang this
% Device is like a process in charge of reading our file
open_file() ->
  {ok, Device} = (file:open("book.txt", read)),
  Device.

% this function reads the file line by line and send the line to the next worker process. It needs 3
% parameters, the Device in order to read the file, the list with all the worker PIDs and a counter,
% this counter is needed for dispatching the line to the next worker.
read_lines(Device, AllWorkers, Counter) ->
  % we are going to read the file line by line
  case file:read_line(Device) of
    {ok, Line} ->
      % getting the next worker process
      WorkerPid = next_worker_pid(AllWorkers, Counter),

      % sending the line to the next worker
      WorkerPid ! {self(), Line},

      % lets read a new line and update counter
      read_lines(Device, AllWorkers, Counter + 1);
    eof ->
      % eof is returned when the text is finished, if that is the case we let know all the workers
      % that the file is finished so we are going to wait for the responses
      lists:foreach(fun(Pid) ->
        Pid ! {self(), eof}
      end, AllWorkers),

      % lets wait fo the worker responses
      waiting_response(#{}, length(AllWorkers))
  end.

% This function will wait for the worker responses. It needs 2 parameters, an accumulated map where we
% are going to store the result and the number of pending responses.
waiting_response(MapResult, 0) ->
  % if we arrive to 0 pending responses that means all the workers already answered so we return the
  % accumulated Map
  MapResult;
waiting_response(MapAcc, PendingResponses) ->
  % waiting for messages
  receive
    {_PidWorker, MapResult} ->
      % we have received a response message with a Result Map. We have to check which map is bigger
      % (The accumulated MapAcc or MapResult) because that will make the next function more performant.
      NewMapAcc = case maps:size(MapAcc) > maps:size(MapResult) of
          true ->
            merge_maps(MapAcc, MapResult);
          false ->
            merge_maps(MapResult, MapAcc)
      end,

      % the maps have been merged, lets update the MappAcc and wait for more responses
      waiting_response(NewMapAcc, PendingResponses - 1)
  end.

merge_maps(LargeMap, SmallMap) ->
  maps:fold(fun add_to_map/3, LargeMap, SmallMap).

add_to_map(Word, Value, AccIn) ->
  Value2 = maps:get(Word, AccIn, 0),
  maps:put(Word, Value +Value2, AccIn).

next_worker_pid(AllWorkerPIDs, Counter) ->
  Index = Counter rem length(AllWorkerPIDs),
  lists:nth(Index + 1, AllWorkerPIDs).

% helper in order to get the current time in milliseconds
 get_timestamp() ->
   {Mega, Sec, Micro} = os:timestamp(),
   (Mega * 1000000 + Sec)*1000 + round(Micro / 1000).
