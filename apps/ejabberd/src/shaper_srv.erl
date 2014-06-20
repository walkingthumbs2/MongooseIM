%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc Shared shapers.
%%% @end
%%%-------------------------------------------------------------------
-module(shaper_srv).
-behaviour(gen_server).
-include_lib("ejabberd/include/ejabberd.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         child_specs/0,
         wait/4]).

%% ------------------------------------------------------------------
%% Record definitions
%% ------------------------------------------------------------------

-record(state, {
        %% Maximum ammount of milliseconds to wait
        max_delay :: non_neg_integer(),
        %% How many seconds to store each shaper
        ttl :: non_neg_integer(),
        shapers :: dict(),
        a_times :: dict()
    }).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

child_specs() ->
    [child_spec(ProcName) ||  ProcName <- worker_names(<<>>)].

child_spec(ProcName) ->
    {ProcName,
     {?MODULE, start_link, [ProcName]},
     permanent,
     5000,
     worker,
     [?MODULE]}.

start_link(ProcName) ->
    gen_server:start_link({local, ProcName}, ?MODULE, [], []).

worker_prefix() ->
    "ejabberd_shaper_".

worker_count(_Host) ->
    10.

worker_names(Host) ->
    [worker_name(Host, N) || N <- lists:seq(0, worker_count(Host) - 1)].

worker_name(_Host, N) ->
    list_to_atom(worker_prefix() ++ integer_to_list(N)).

select_worker(Host, Tag) ->
    N = worker_number(Host, Tag),
    worker_name(Host, N).

worker_number(Host, Tag) ->
    erlang:phash2(Tag, worker_count(Host)).

%% @doc Shapes the caller from executing the action.
-spec wait(_Host, _Action, _FromJID, _Size) -> ok | {error, max_delay_reached}.
wait(Host, Action, FromJID, Size) ->
    gen_server:call(select_worker(Host, FromJID), {wait, Host, Action, FromJID, Size}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    State = init_dicts(#state{
        max_delay = proplists:get_value(max_delay, Args, 3000),
        ttl = proplists:get_value(ttl, Args, 120)
    }),
    GCInt = proplists:get_value(gc_interval, Args, 30),
    timer:send_interval(timer:seconds(GCInt), delete_old_shapers),
    {ok, State}.

handle_call({wait, Host, Action, FromJID, Size},
            From, State=#state{max_delay=MaxDelayMs}) ->
    Key = new_key(Host, Action, FromJID),
    Shaper = find_or_create_shaper(Key, State),
    State1 = update_access_time(Key, now(), State),
    case shaper:update(Shaper, Size) of
        {UpdatedShaper, 0} ->
            {reply, ok, save_shaper(Key, UpdatedShaper, State1)};
        {UpdatedShaper, DelayMs} when DelayMs < MaxDelayMs ->
            reply_after(DelayMs, From, ok),
            {noreply, save_shaper(Key, UpdatedShaper, State1)};
        {_, _} ->
            {reply, {error, max_delay_reached}, State1}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(delete_old_shapers, State) ->
    %?DEBUG("Deleted old shapers", []),
    {noreply, delete_old_shapers(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

new_key(Host, Action, FromJID) ->
    {Host, Action, FromJID}.

find_or_create_shaper(Key, #state{shapers=Shapers}) ->
    case dict:find(Key, Shapers) of
        {ok, Shaper} -> Shaper;
        error -> create_shaper(Key)
    end.

update_access_time(Key, Now, State=#state{a_times=Times}) ->
    State#state{a_times=dict:store(Key, Now, Times)}.

save_shaper(Key, Shaper, State=#state{shapers=Shapers}) ->
    State#state{shapers=dict:store(Key, Shaper, Shapers)}.

init_dicts(State) ->
    State#state{shapers=dict:new(), a_times=dict:new()}.

delete_old_shapers(State=#state{shapers=Shapers, a_times=Times, ttl=TTL}) ->
    Min = subtract_seconds(now(), TTL),
    %% Copy recently modified shapers
    dict:fold(fun
        (_, ATime, Acc) when ATime < Min -> Acc; %% skip too old
        (Key, ATime, Acc) ->
            Shaper = dict:fetch(Key, Shapers),
            update_access_time(Key, ATime, save_shaper(Key, Shaper, Acc))
        end, init_dicts(State), Times).

create_shaper(Key) ->
    shaper:new(request_shaper_name(Key)).

request_shaper_name({Host, Action, FromJID}) ->
    get_shaper_name(Host, Action, FromJID, default_shaper()).

default_shaper() ->
    none.

get_shaper_name(Host, Action, FromJID, Default) ->
    case acl:match_rule(Host, Action, FromJID) of
        deny -> Default;
        Value -> Value
    end.

%% It is a small hack
%% This function calls this in more efficient way:
%% timer:apply_after(DelayMs, gen_server, reply, [From, Reply]).
reply_after(DelayMs, {Pid, Tag}, Reply) ->
    erlang:send_after(DelayMs, Pid, {Tag, Reply}).

subtract_seconds({MegaSecs, Secs, MicroSecs}, SubSecs) ->
    {MegaSecs - (SubSecs div 1000000), Secs - (SubSecs rem 1000000), MicroSecs}.
