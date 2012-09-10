-module(ejabberd_zlib_test).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 34567).
-define(TIMEOUT, 100).

echo_test() ->
    start_the_server(echo_server),

    Client = start_the_client(),
    Msg = <<"Hello!">>,
    ?assertEqual(ok, gen_tcp:send(Client, Msg)),
    ?assertEqual({ok, Msg}, gen_tcp:recv(Client, 0)),
    ?assertEqual(ok, gen_tcp:close(Client)),

    stop_the_server().

compressed_echo_test() ->
    start_the_server(echo_server),

    Client = start_the_client(),
    {ok, ZClient} = ejabberd_zlib:enable_zlib(gen_tcp, Client, self()),
    Msg = <<"Hello!">>,
    ?assertEqual(ok, ejabberd_zlib:send(ZClient, Msg)),
    ?assertEqual({ok, Msg}, ejabberd_zlib:recv(ZClient, 0)),
    ?assertEqual(ok, ejabberd_zlib:close(ZClient)),

    stop_the_server().

chunked_echo_test() ->
    start_the_server(chunked_echo_server),

    Client = start_the_client(),
    {ok, ZClient} = ejabberd_zlib:enable_zlib(gen_tcp, Client, self()),
    Msg = <<"Hello!">>,
    ?assertEqual(ok, ejabberd_zlib:send(ZClient, Msg)),
    ?assertEqual({ok, Msg}, ejabberd_zlib:recv(ZClient, 0)),
    ?assertEqual(ok, ejabberd_zlib:close(ZClient)),

    stop_the_server().

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

start_the_server(HandlerModule) ->
    {ok, Pid} = gen_tcp_server:start_link(HandlerModule, ?PORT),
    put(server, Pid),
    timer:sleep(?TIMEOUT).

stop_the_server() ->
    gen_tcp_server:stop(get(server)),
    timer:sleep(?TIMEOUT).

start_the_client() ->
    {ok, Client} = gen_tcp:connect({127,0,0,1}, ?PORT,
                                   [{active, false}, binary]),
    Client.
