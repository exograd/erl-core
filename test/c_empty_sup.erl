-module(c_empty_sup).

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
  #{}.
