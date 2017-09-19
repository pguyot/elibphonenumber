
-module(elibphonenumber_sup).
-author("silviu.caragea").

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        worker(phonenumber_to_carrier)
    ],
    {ok, {{one_for_one, 1000, 1}, Children}}.

worker(Name) ->
    worker(Name, 5000).

worker(Name, WaitForClose) ->
    {Name, {Name, start_link, []}, permanent, WaitForClose, worker, [Name]}.