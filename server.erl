-module(server).
-export([start/1,stop/1, serverHandle/2, channelHandle/2]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process id
    genserver:start(ServerAtom, createServer(), fun server:serverHandle/2).


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, {stop_all_channels}),
    genserver:stop(ServerAtom).

% ---------------------------------------------------------------------------
% The structure of a server
-record(server, {
    clients,   % list of clients nicknames
    channels   % list of channels
    }).

% Initial server-state
createServer() -> #server{clients = [], channels = []}.

% Adds a channel to the servers state.
% Used when a new channel is created.
addChannelToServer(Server = #server{channels = Channels}, ChannelAtom) ->
    Server#server{channels = [ChannelAtom | Channels]}.

% Adds a channel and a nickname to the servers state.
% Used when a new channel is created.
addChannelAndClient(Server = #server{channels = Channels, clients = Clients}, ChannelAtom, ClientName) ->
    Server#server{clients  = [ClientName | Clients],
                  channels = [ChannelAtom | Channels]}.

% Adds a nickname to the servers state.
% Used when a client joins an existing channel.
addClientToServer(Server = #server{clients = Clients}, ClientName) ->
    Server#server{clients = [ClientName | Clients]}.

% Removes a nickname from the servers state.
% Used when a client quits or when a client successfully changes nickname.
removeClientFromServer(Server = #server{clients = Clients}, ClientName) ->
    Server#server{clients = lists:filter(fun(Name) -> Name =/= ClientName end, Clients)}.


% Join a channel
serverHandle(Server = #server{clients = Clients}, {join, ChannelAtom, ClientPid, ClientName}) ->

    % Checking if channel exists
    case whereis(ChannelAtom) of
        % channel doesn't exist
        undefined ->
            % creating channel 
            genserver:start(ChannelAtom,
                            createChannel(ChannelAtom, ClientPid),
                            fun channelHandle/2),
            
            % Adding channel and client to servers state
            {reply, ok, addChannelAndClient(Server, ChannelAtom, ClientName)};

        % If channel does exist
        _Pid ->
            % Asking channel instead
            Response = genserver:request(ChannelAtom, {join, ClientPid}),

            % Adding client to servers state
            {reply, Response, addClientToServer(Server, ClientName)}
    end;


% This case only exists to add clients' initial names to server, but is not needed.
% serverHandle(Server, {add_nickname, Nickname}) ->
%    {reply, ok, addClientToServer(Server, Nickname)};

% Changing Nick/username 
serverHandle(Server = #server{clients = Nicknames}, {change_nick, OldNick, NewNick}) ->
    % Check if NewNick is taken  
    case lists:member(NewNick, Nicknames) of
        true ->
            {reply, {error, nick_taken, "Nickname is taken"}, Server};
        false ->
            % Removing old nick and adding new nick to server state 
            NewServer = addClientToServer(removeClientFromServer(Server, OldNick), NewNick),
            {reply, ok, NewServer}
    end;


% Server is stopped
serverHandle(Server = #server{channels = Channels}, {stop_all_channels}) ->
    % Stopping all the channels
    spawn(fun() -> [genserver:stop(Channel) || Channel <- Channels] end),
    {reply, ok, Server};


% A client has quit
serverHandle(Server = #server{channels = Channels}, {leave_all_channels, Nick, Client}) ->
    % Create a process to take care of a client leaving all channels
    spawn(fun() -> [genserver:request(Channel, {leave, Nick, Client}) || Channel <- Channels] end),
    % Changing server state by removing that client from Server state 
    {reply, ok, removeClientFromServer(Server, Nick)};


% Catch-all for any unhandled requests
serverHandle(Server, Data) ->
    {reply, {error, not_implemented, "Server does not handle this command"}, Server}.


% --------------------------------------------------------------------------
% The structure of a channel 
-record(channel, {
    nick,       % name of the channel, an atom
    clients     % list of the clients' PIDs
}).

% Initial state for channel 
createChannel(Nick, Client) ->
    #channel{nick = Nick, clients = [Client]}.

% Adding client to channel state 
addClientToChannel(Ch = #channel{clients = Clients}, Client) ->
    Ch#channel{clients = [Client | Clients]}.

% Remove client from channel state 
removeClientFromChannel(Ch = #channel{clients = Clients}, Client) ->
    Ch#channel{clients = lists:delete(Client, Clients)}.


% Joining channel
channelHandle(Ch = #channel{clients = Clients}, {join, Client}) ->
    
    % Check if client exists in channel state 
    case lists:member(Client, Clients) of
        true ->
            {reply, {error, user_already_joined, "Already joined"}, Ch};
        false ->
            % Changing channel state by adding the client to channel state
            {reply, ok, addClientToChannel(Ch, Client)}
    end;


% Leaving channel 
channelHandle(Ch = #channel{nick = ChannelAtom, clients = Clients}, {leave, Nick, Leaving}) ->
    % Check if client exist in channel state 
    case lists:member(Leaving, Clients) of
        true ->
            % Create a process to send a leaving message  
            spawn(fun() -> 
                  sendMessageTo(Clients, atom_to_list(ChannelAtom), Leaving,
                        "From Channel", Nick ++ " has left the channel")
                  end),
            % Changing channel state by removing that client 
            {reply, ok, removeClientFromChannel(Ch, Leaving)};

        false ->
            {reply, {error, user_not_joined, "Cannot leave unassociated channel"}, Ch}
    end;


% Sending message 
channelHandle(Ch = #channel{nick = ChannelAtom, clients = Clients},
              {message_send, Client, Nick, Msg}) ->
    % Checking if Client exists in channel state             
    case lists:member(Client, Clients) of
        true ->
            % Create a process to send message 
            spawn(fun() ->
                  sendMessageTo(Clients, atom_to_list(ChannelAtom), Client, Nick, Msg)
                  end),
            {reply, ok, Ch};

        false ->
            {reply, {error, user_not_joined, "Cannot send message in unassociated channel"}, Ch}
    end;


% Catch-all for any unhandled requests
channelHandle(Ch, Data) ->
    {reply, {error, not_implemented, "Channel does not handle this command"}, Ch}.



% Local function to handle sending messages 
% no clients to send message to
sendMessageTo([], _Channel, _Sender, _Nick, _Msg) -> ok;

% we do not display the message to the sender.
sendMessageTo([Client | Clients], Channel, Sender, Nick, Msg) when Client =:= Sender ->
    sendMessageTo(Clients, Channel, Sender, Nick, Msg);

sendMessageTo([Client | Clients], Channel, Sender, Nick, Msg) ->
    genserver:request(Client,{message_receive, Channel, Nick, Msg}),
    sendMessageTo(Clients, Channel, Sender, Nick, Msg).



