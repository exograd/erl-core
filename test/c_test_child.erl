-module(c_test_child).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/2, stop/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0, exception_spec/0]).

-type options() ::
        #{init_error => term(),
          init_exception => exception_spec()}.

-type exception_spec() ::
        {throw, term()}
      | {error, term()}
      | {exit, term()}.

-type state() ::
        #{name := atom()}.

-spec start_link(atom(), options()) -> c_gen_server:start_ret().
start_link(Name, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Options], []).

-spec stop(c_gen_server:ref(), term()) -> ok.
stop(Ref, Reason) ->
  gen_server:stop(Ref, Reason, infinity).

-spec init(list()) -> c_gen_server:init_ret(state()).
init([Name, Options]) ->
  logger:update_process_metadata(#{domain => [sup, test_child, Name]}),
  ?LOG_INFO("starting (options ~0tp)", [Options]),
  maybe_signal_init_exception(Options),
  case maps:find(init_error, Options) of
    {ok, Term} ->
      {stop, Term};
    error ->
      {ok, #{name => Name, options => Options}}
  end.

-spec maybe_signal_init_exception(options()) -> ok.
maybe_signal_init_exception(#{init_exception := {throw, Term}}) ->
  throw(Term);
maybe_signal_init_exception(#{init_exception := {error, Term}}) ->
  error(Term);
maybe_signal_init_exception(#{init_exception := {exit, Term}}) ->
  exit(Term);
maybe_signal_init_exception(_) ->
  ok.

-spec terminate(c_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, _State) ->
  ?LOG_INFO("terminating"),
  ok.

-spec handle_call(term(), {pid(), c_gen_server:request_id()}, state()) ->
        c_gen_server:handle_call_ret(state()).
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> c_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> c_gen_server:handle_info_ret(state()).
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.
