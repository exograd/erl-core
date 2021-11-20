-module(c_static_sup).

-behaviour(c_sup).

-export([start_link/0, stop/0]).
-export([children/0]).

-spec start_link() -> c_sup:start_ret().
start_link() ->
  c_sup:start_link({local, ?MODULE}, ?MODULE, #{}).

-spec stop() -> ok.
stop() ->
  c_sup:stop(?MODULE).

-spec children() -> c_sup:child_specs().
children() ->
  #{a =>
      #{start => fun c_test_child:start_link/2,
        start_args => [a, #{}]},
    b =>
      #{start => fun c_test_child:start_link/2,
        start_args => [b, #{}]},
    c =>
      #{start => fun c_test_child:start_link/2,
        start_args => [c, #{}]}}.
