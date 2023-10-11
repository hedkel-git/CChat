-module(server).
-export([start/1,stop/1, serverHandle/2, channelHandle/2]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process id
    genserver:start(ServerAtom, [], fun server:serverHandle/2).


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->

    genserver:stop(ServerAtom).

% ---------------------------------------------------------------------------

serverHandle(Server, {create_join, ChannelName, ChannelAtom, Client}) ->

    genserver:start(ChannelAtom, createChannel(ChannelName, Client), fun channelHandle/2),
    {reply, ok, [ChannelAtom | Server]};


serverHandle(Server, {leave_all_channels, Client}) ->
    {reply, ok, Server};


%leaveAllChannels([], {leave_all_channels, _Client}) ->
%    ok;
%leaveAllChannels([Channel | Channels], {leave_all_channels, Client}) ->
%    genserver:request(Channel, {})



serverHandle(Server, Data) ->
    {reply, {error, not_implemented, "Server does not handle this command"}, Server}.

% ---------------------------------------------------------------------------
-record(channel, {
    nick,       % name of the channel
    clients     % list of the clients' PIDs
}).

createChannel(Nick, Client) ->
    #channel{nick = Nick, clients = [Client]}.

addClient(Ch = #channel{clients = Clients}, Client) ->
    Ch#channel{clients = [Client | Clients]}.

removeClient(Ch = #channel{clients = Clients}, Client) ->
    Ch#channel{clients = lists:delete(Client, Clients)}.


channelHandle(Ch = #channel{clients = Clients}, {join, Client}) ->
    case lists:member(Client, Clients) of
        true ->
            {reply, {error, user_already_joined, "Already joined"}, Ch};
        false ->
            {reply, ok, addClient(Ch, Client)}
    end;


%channelHandle(Ch = #channel{clients = Clients}, {leave, Client}) ->
%    case lists:member(Client, Clients) of
%        true ->
%            {reply, ok, removeClient(Ch, Client)};
%        false ->
%            {reply, {error, user_not_joined, "Cannot leave unassociated channel"}, Ch}
%    end;

channelHandle(Ch = #channel{clients = Clients}, {leave, Channel, Nick, Leaver}) ->
    case lists:member(Leaver, Clients) of
        true ->
            spawn(fun() -> 
                  sendMessageTo(Clients, Channel, Leaver, "From Channel", Nick ++ " has left the channel")
                  end),

            {reply, ok, removeClient(Ch, Leaver)};
        false ->
            {reply, {error, user_not_joined, "Cannot leave unassociated channel"}, Ch}
    end;

channelHandle(Ch = #channel{nick = ChannelName, clients = Clients},{message_send, Client, Nick, Msg}) ->
    case lists:member(Client, Clients) of
        true ->
            spawn(fun() -> 
                  sendMessageTo(Clients, ChannelName, Client, Nick, Msg)
                  end),
            {reply, ok, Ch};
        false ->
            {reply, {error, user_not_joined, "Cannot send message in unassociated channel"}, Ch}
    end;


channelHandle(Ch, Data) ->
    {reply, {error, not_implemented, "Channel does not handle this command"}, Ch}.



sendMessageTo([], _ChannelName, _Sender, _Nick, _Msg) -> ok;

sendMessageTo([Client | Clients], ChannelName, Sender, Nick, Msg) when Client =:= Sender ->
    % we do not display the message to the sender.
    sendMessageTo(Clients, ChannelName, Sender, Nick, Msg);

sendMessageTo([Client | Clients], ChannelName, Sender, Nick, Msg) ->
    % requesting the client to 'show' the
    % message with the help of a new process

%is this necessary??
%    spawn(fun() ->
%          genserver:request(Client,{message_receive, ChannelName, Nick, Msg})
%          end),
    genserver:request(Client,{message_receive, ChannelName, Nick, Msg}),

    sendMessageTo(Clients, ChannelName, Sender, Nick, Msg).




