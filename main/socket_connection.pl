:- use_module(library(streampool)).


/* SERVER SIDE CODE */
server(Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, In, _Out),
    add_stream_to_pool(In, accept(Socket)),
    thread_create(stream_pool_main_loop, _ThreadID, []).

kill(Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_close_socket(Socket).

accept(Socket) :-
    tcp_accept(Socket, Slave, Peer),
    tcp_open_socket(Slave, In, Out),
    add_stream_to_pool(In, client(In, Out, Peer)).

client(In, Out, _Peer) :-
    read_line_to_codes(In, Command),
    close(In),
    process_command(Command, In, Out),
    close(Out),
    delete_stream_from_pool(In).

process_command(Command, _In, Out) :-
    set_stream(Out, alias(outAlias)),
    (
	Command = "owl" ->
	new(ME, modelExporter),
	send(ME, export, outAlias),
	free(ME)
    ;
	Command = "<request>newquestion</request>" ->
	format(outAlias, '<question id="1"><text>What\'s the earth\'s shape?</text><answer>flat</answer><answer type="solution">spherical</answer></question>', [])
    ;
	Command = "<answer type=\"quiz\"><question id=\"1\" solved=\"true\" /></answer>" ->
	format(outAlias, '<answer>ok</answer>', [])
    ;
	format(outAlias, 'Please to meet you: ~s~n', [Command])
    ).

/* CLIENT SIDE CODE */
client(Server, Port) :-
    tcp_socket(Socket),
    tcp_connect(Socket, Server:Port),
    tcp_open_socket(Socket, ReadFd, WriteFd),
    format(WriteFd, 'owl\n', []),
    close(WriteFd),
    set_stream(ReadFd, timeout(10)),
    read_stream_to_codes(ReadFd, TermCodes),
    atom_codes(Term, TermCodes),
    %catch(read(ReadFd, Term), _, (close(ReadFd), fail)),
    %read_line_to_codes(ReadFd, OWLFile),
    format('~w\n', [Term]),
    close(ReadFd).


