% @doc rolnik public API.
% @end
-module(rolnik).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> rolnik_sup:start_link().

stop(_State) -> ok.
