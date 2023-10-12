-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.

start(ServerAtom) ->  
    genserver:start(ServerAtom, cleint:initial_state() ,fun client:handle/2). %this is a Synchronous call
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
    % TODO Implement function
    % Return ok
