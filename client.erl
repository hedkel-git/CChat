-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->

    % Converting Channel to an atom and
    % checking if it has been created or not
    ChannelAtom = list_to_atom(Channel),

%    case whereis(ChannelAtom) of
%        % If channel does not exist, we tell server
%        % to create it and add ourselves to it.
%        undefined ->
%            
%            %what if server doesn't exist?
%            Result = genserver:request(St#client_st.server, {create_join, Channel, ChannelAtom, self()}),
%            % do we need some sort of check for the result of request?
%            
%            {reply, Result, St};
%            %{reply, ok, St};
%
%        % If channel does exist, we request to join it directly.
%        _Pid ->
%            Result = genserver:request(ChannelAtom, {join, self()}),
%            {reply, Result, St}
%    end;


    case whereis(ChannelAtom) of
        % If channel does not exist, we tell server
        % to create it and add ourselves to it.
        undefined ->
            
            ServerAtom = St#client_st.server,
            case whereis(ServerAtom) of
                undefined ->
                    {reply, {error, server_not_reached, "Server is offline"}, St};

                _Pid ->
                    case catch(genserver:request(ServerAtom,
                                {create_join, Channel, ChannelAtom, self()}) ) of
                        
                        timeout_error ->
                            {reply, {error, server_not_reached, "Server is timed out"}, St};
                        
                        Result ->
                            {reply, Result, St}
                    end
            end;

        % If channel does exist, we request to join it directly.
        _Pid ->
            Result = genserver:request(ChannelAtom, {join, self()}),
            {reply, Result, St}
    end;


% Leave channel
handle(St, {leave, Channel}) ->

    ChannelAtom = list_to_atom(Channel),

    case whereis(ChannelAtom) of
        undefined ->
            {reply, {error, user_not_joined, "Channel does not exist"}, St};

        _Pid ->
            {reply, genserver:request(ChannelAtom, {leave, self()}), St}
    end;



% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->

    ChannelAtom = list_to_atom(Channel),

    case whereis(ChannelAtom) of
        undefined ->
            {reply, {error, user_not_joined, "Channel does not exist"}, St};
        _Pid ->
            Result = genserver:request(ChannelAtom, {message_send, self(), St#client_st.nick, Msg}),
            {reply, Result, St}
    end;


% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;



% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;


% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    io:fwrite("message recieved~n"),
    io:format("pid: ~p~n", [self()]),


    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
