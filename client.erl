-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server,% atom of the chat server
    channel = []
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


handle(St , {join, Channel}) ->
    % io:format("Channel: ~p ~n", [St#client_st.channel]),

    case whereis(St#client_st.server) of
        undefined ->
           {reply,{error,server_not_reached,"Server already join"}, St};
        
        _ ->
            case lists:member(Channel, St#client_st.channel) of 
                true ->   
                    {reply,{error,user_already_joined,"Channel already join"}, St};
                false ->   
                    {reply, ok , St#client_st{channel = [Channel] ++ St#client_st.channel}}
            end
    end;
        

% Leave channel 
handle(St, {leave, Channel}) -> 
    case whereis(St#client_st.server) of
        undefined ->
            {reply, {error,user_not_joined,"Channel not exists"}, St};
        _ ->
            case lists:member(Channel, St#client_st.channel) of
                true ->
                    {reply, ok, St#client_st{channel = St#client_st.channel --[Channel]}};
                false ->
                    {reply, {error, user_not_joined, "Not joined"}, St}
            end

    
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
        case whereis(St#client_st.server) of
            undefined ->
                {reply, {error,user_not_joined , "user not joined"}, St} ;
            _ ->
                ok = genserver:request(St#client_st.server, {message_send, Channel, self(),  Msg}),
                {reply, ok, St}
            end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    case St#client_st.nick =:= NewNick of
        true ->
            {reply, {error,nick_taken,"name already exists"}, St};
        false ->
              {reply, ok, St#client_st{nick = NewNick}}
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
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
