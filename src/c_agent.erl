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

-export([start_link/2, start_link/3, stop/1, stop/2]).
-export([init/4]). % has to be exported to be called by spawn_link/3

-export_type([options/0, error_reason/0, result/0, result/1,
              name/0, ref/0]).

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

-callback init(State) -> {ok, State} | {error, term()}.
-callback terminate(term()) -> ok.

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
  stop(Ref, normal).

-spec stop(ref(), term()) -> ok.
stop(Ref, Reason) ->
  Mon = erlang:monitor(process, Ref),
  Ref ! {c_agent, {stop, Reason}},
  receive
    {'DOWN', Mon, _, _, _Reason} ->
      ok
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
    {c_agent, {stop, Reason}} ->
      ?LOG_DEBUG("stopping (reason: ~0tp)", [Reason]),
      exit(Reason);
    Msg ->
      ?LOG_INFO("message: ~tp", [Msg]),
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
