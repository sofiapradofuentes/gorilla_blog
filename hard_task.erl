-module(hard_task).
-export([execute/1]).

%----------------------------------------------
% Public API
%----------------------------------------------

% This Function will execute `hard_task` many times (given by TaskTimes variable). It creates a
% new process per task.
execute(TaskTimes) ->
  % getting the current time in milliseconds
  Begining = get_timestamp(),

  % store the self pid in order to send it to the spawn processes
  SelfPID = self(),

  % Lets spawn `TaskTimes` processes and execute the hard_task function
  [spawn(fun() ->
           hard_task(SelfPID)
         end) || _X <- lists:seq(1, TaskTimes)],

  % Now we wait for the spawn processes to send back a message saying they are finished
  ok = waiting_response(TaskTimes),

  % calculating the elapsed time
  ElapsedTime = (get_timestamp() - Begining) / 1000,
  io:format("elapsed time: ~p seconds ~n", [ElapsedTime]),
  ok.

%----------------------------------------------
% Internal functions (not exported)
%----------------------------------------------

waiting_response(0) ->
  ok;
waiting_response(PendingResponses) ->
  receive
    done ->
      waiting_response(PendingResponses - 1)
    end.

% This function is the "Hard Task", it will wait for 1 second and then will reply back a `done` message
% to the CallerPID
hard_task(CallerPID) ->
  timer:sleep(1000), %let's simulate a super time consuming hard task!!!
  CallerPID ! done. % send the message `done` to the caller process

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro / 1000).
