%% Copyright (c) 2021-2022 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(c_agent).

-include_lib("kernel/include/logger.hrl").

-export([start_link/2, start_link/3, stop/1,
         call/2]).
-export([init/4]). % has to be exported to be called by spawn_link/3

-export_type([options/0, error_reason/0, result/0, result/1,
              name/0, ref/0,
              handle_message_ret/1, handle_call_ret/1, handle_call_ret/2]).

-optional_callbacks([terminate/1, handle_message/2, handle_call/3]).

-type options() ::
        #{domain => [atom()],
          start_timeout => timeout(),
          args => [term()]}.

-type error_reason() ::
        {already_started, pid()}
      | {init, term()}.

-type result() ::
        ok | {error, error_reason()}.

-type result(Type) ::
        {ok, Type} | {error, error_reason()}.

-type name() ::
        atom().

-type ref() ::
        pid() | name().

-type handle_message_ret(State) ::
        {ok, State}
      | {stop, State}.

-type handle_call_ret(State) ::
        handle_call_ret(term(), State).

-type handle_call_ret(Response, State) ::
        {ok, Response, State}
      | {stop, State}.

-callback init(State) -> {ok, State} | {error, term()} when
    State :: term().

-callback terminate(State) -> ok when
    State :: term().

-callback handle_message(Message, State) -> handle_message_ret(State) when
    Message :: term(),
    State :: term().

-callback handle_call(Request, pid(), State) ->
  handle_call_ret(Response, State) when
    Request :: term(),
    Response :: term(),
    State :: term().

-spec start_link(module(), options()) -> result(pid()).
start_link(Module, Options) ->
  ?LOG_DEBUG("starting child (module: ~p)", [Module], #{domain => [c_agent]}),
  Pid = spawn_link(?MODULE, init, [undefined, Module, Options, self()]),
  handle_starting_child(Pid, Pid, Options).

-spec start_link(name(), module(), options()) -> result(pid()).
start_link(Name, Module, Options) ->
  ?LOG_DEBUG("starting child ~0tp (module: ~p)", [Name, Module],
             #{domain => [c_agent]}),
  case whereis(Name) of
    undefined ->
      Pid = spawn_link(?MODULE, init, [Name, Module, Options, self()]),
      handle_starting_child(Name, Pid, Options);
    Pid ->
      ?LOG_ERROR("child ~0tp already started", [Name], #{domain => [c_agent]}),
      {error, {already_started, Pid}}
  end.

-spec handle_starting_child(name() | pid(), pid(), options()) -> result(pid()).
handle_starting_child(Id, Pid, Options) ->
  StartTimeout = maps:get(start_timeout, Options, 5000),
  receive
    ok ->
      ?LOG_DEBUG("child ~0tp started", [Id],
                 #{domain => [c_agent], pid => Pid}),
      {ok, Pid};
    {error, Reason} ->
      ?LOG_ERROR("cannot start child ~0tp: ~tp", [Id, Reason],
                 #{domain => [c_agent], pid => Pid}),
      {error, Reason}
  after StartTimeout ->
      ?LOG_ERROR("child ~0tp timed out", [Id],
                 #{domain => [c_agent], pid => Pid}),
      unlink(Pid),
      exit(Pid, kill),
      receive
        {'EXIT', Pid, _} ->
          {error, start_timeout}
      end
  end.

-spec stop(ref()) -> ok.
stop(Ref) ->
  Mon = erlang:monitor(process, Ref),
  Ref ! {c_agent, stop},
  receive
    {'DOWN', Mon, _, _, _} ->
      ok
  end.

-spec call(ref(), term()) -> term().
call(Ref, Req) ->
  Mon = erlang:monitor(process, Ref),
  Ref ! {c_agent, {request, Req, self()}},
  receive
    {c_agent, {response, Res}} ->
      erlang:demonitor(Mon),
      Res;
    {'DOWN', Mon, _, RefPid, _} ->
      error({call_recipient_exit, RefPid})
  end.

-spec init(name() | undefined, module(), options(), pid()) -> no_return().
init(Name, Module, Options, Parent) ->
  maybe_register_name(Name, Parent),
  set_domain(Name, Options),
  Args = maps:get(args, Options, []),
  try
    Module:init(Args)
  of
    {ok, State} ->
      Parent ! ok,
      main(State, Module, Options);
    {error, Reason} ->
      ?LOG_ERROR("initialization failure: ~tp", [Reason]),
      Parent ! {error, {init, Reason}}
  catch
    error:Reason:Trace ->
      ?LOG_ERROR("initialization error: ~tp~n~tp", [Reason, Trace]),
      Parent ! {error, {init, Reason}};
    exit:Reason:Trace ->
      ?LOG_ERROR("initialization exit: ~tp~n~tp", [Reason, Trace]),
      Parent ! {error, {init, Reason}};
    throw:Reason ->
      ?LOG_ERROR("initialization exception: ~tp", [Reason]),
      Parent ! {error, {init, Reason}}
  end.

-spec main(term(), module(), options()) -> ok.
main(State, Module, Options) ->
  receive
    {c_agent, stop} ->
      ?LOG_DEBUG("stopping"),
      terminate(State, Module, Options);
    {c_agent, {request, Request, From}} ->
      handle_call(Request, From, State, Module, Options);
    Msg ->
      handle_message(Msg, State, Module, Options)
  end.

-spec terminate(term(), module(), options()) -> no_return().
terminate(State, Module, _Options) ->
  ?LOG_DEBUG("terminating"),
  case erlang:function_exported(Module, terminate, 2)  of
    true ->
      try
        Module:terminate(State)
      catch
        error:Reason:Trace ->
          ?LOG_ERROR("termination error: ~tp~n~tp", [Reason, Trace]);
        exit:Reason:Trace ->
          ?LOG_ERROR("termination exit: ~tp~n~tp", [Reason, Trace]);
        throw:Reason ->
          ?LOG_ERROR("termination exception: ~tp", [Reason])
      after
        exit(normal)
      end;
    false ->
      exit(normal)
  end.

-spec handle_call(term(), pid(), term(), module(), options()) -> ok.
handle_call(Req, From, State, Module, Options) ->
  case erlang:function_exported(Module, handle_call, 3)  of
    true ->
      try
        Module:handle_call(Req, From, State)
      of
        {ok, Res, State2} ->
          From ! {c_agent, {response, Res}},
          main(State2, Module, Options);
        {stop, State2} ->
          terminate(State2, Module, Options)
      catch
        error:Reason:Trace ->
          ?LOG_ERROR("call handling error: ~tp~n~tp", [Reason, Trace]),
          main(State, Module, Options);
        exit:Reason:Trace ->
          ?LOG_ERROR("call handling exit: ~tp~n~tp", [Reason, Trace]),
          main(State, Module, Options);
        throw:Reason ->
          ?LOG_ERROR("call handling exception: ~tp", [Reason]),
          main(State, Module, Options)
      end;
    false ->
      ?LOG_INFO("unhandled call from ~p: ~tp", [From, Req]),
      main(State, Module, Options)
  end.

-spec handle_message(term(), term(), module(), options()) -> ok.
handle_message(Msg, State, Module, Options) ->
  case erlang:function_exported(Module, handle_message, 2)  of
    true ->
      try
        Module:handle_message(Msg, State)
      of
        {ok, State2} ->
          main(State2, Module, Options);
        {stop, State2} ->
          terminate(State2, Module, Options)
      catch
        error:Reason:Trace ->
          ?LOG_ERROR("message handling error: ~tp~n~tp", [Reason, Trace]),
          main(State, Module, Options);
        exit:Reason:Trace ->
          ?LOG_ERROR("message handling exit: ~tp~n~tp", [Reason, Trace]),
          main(State, Module, Options);
        throw:Reason ->
          ?LOG_ERROR("message handling exception: ~tp", [Reason]),
          main(State, Module, Options)
      end;
    false ->
      ?LOG_INFO("unhandled message: ~tp", [Msg]),
      main(State, Module, Options)
  end.

-spec maybe_register_name(name() | undefined, pid()) -> ok.
maybe_register_name(undefined, _) ->
  ok;
maybe_register_name(Name, Parent) ->
  try
    register(Name, self()),
    ok
  catch
    error:_ ->
      Parent ! {error, {already_started, self()}},
      exit(normal)
  end.

-spec set_domain(name() | undefined, options()) -> ok.
set_domain(undefined, Options) ->
  case maps:find(domain, Options) of
    {ok, Domain} ->
      logger:update_process_metadata(#{domain => Domain});
    error ->
      ok
  end;
set_domain(Name, Options) ->
  Domain = maps:get(domain, Options, [Name]),
  logger:update_process_metadata(#{domain => Domain}).
