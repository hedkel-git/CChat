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

    % this would be needed if a client wanted to change their
    % name before joining any channel... 
    % but this is not allowed and collides with some of the tests
    % catch(genserver:request(ServerAtom, {add_nickname, Nick})),

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
handle(St = #client_st{nick = Nick, server = Server}, {join, Channel}) ->
    % Checking if the server exists/online
    case whereis(Server) of
        % If server not found, reply with an error 
        undefined ->
            {reply, {error, server_not_reached, "Server is occupied"}, St};
    
        _Pid ->
            % Try and catch timeout 
            % Send a request to server for joining channel 
            try genserver:request(Server, {join, list_to_atom(Channel), self(), Nick}) of
                Response ->
                    {reply, Response, St}
            catch
                timeout_error ->
                    {reply, {error, server_not_reached, "Server is occupied"}, St}
            end
    end;


% Leave channel
handle(St = #client_st{nick = Nick, server = Server}, {leave, Channel}) ->

    % Convert from channel's string to atom 
    ChannelAtom = list_to_atom(Channel),
    % Check if channel exists 
    case whereis(ChannelAtom) of
        % If channel does not exist, reply with an error 
        undefined ->
            {reply, {error, server_not_reached, "Channel does not exist"}, St};

        _Pid ->
            % Send a request directly to channel for leaving the channel
            Response = genserver:request(ChannelAtom, {leave, Nick, self()}),
            {reply, Response, St}
    end;


% Sending message (from GUI, to channel)
handle(St = #client_st{nick = Nick, server = Server}, {message_send, Channel, Msg}) ->

    ChannelAtom = list_to_atom(Channel),
    % Check if channel exists 
    case whereis(ChannelAtom) of
        undefined ->
             % If channel does not exist, reply with an error 
            {reply, {error, server_not_reached, "Channel does not exist"}, St};
    
        _Pid ->
            % Send a request directly to channel for sending message 
            Response = genserver:request(ChannelAtom, {message_send, self(), Nick, Msg}),
            {reply, Response, St}
    end;


% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St = #client_st{nick = OldNick, server = Server}, {nick, NewNick}) ->
    % Send a request to server for changing nick 
    case genserver:request(Server, {change_nick, OldNick, NewNick}) of
        ok ->
            {reply, ok, St#client_st{nick = NewNick}};
        _else ->
            {reply, {error, nick_taken, "Nickname is taken"}, St}
    end;



% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;


% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St = #client_st{nick = Nick, server = Server}, quit) ->
    % Any cleanup should happen here, but this is optional
    % Send a request to server for leaving all channels 
    {reply, genserver:request(Server, {leave_all_channels, Nick, self()}), St};


% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
