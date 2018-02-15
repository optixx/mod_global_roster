-module(mod_global_roster).

-behavior(gen_mod).

-include("ejabberd.hrl").

-export([start/2, stop/1, on_presence_joined/4, on_presence_left/4]).

start(Host, _Opts) ->
  ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence_joined, 50),
  ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_presence_left, 50),
  ok.

stop(Host) ->
  ejabberd_hooks:remove(set_presence_hook, Host, ?MODULE, on_presence_joined, 50),
  ejabberd_hooks:remove(unset_presence_hook, Host, ?MODULE, on_presence_left, 50),
  ok.
  
on_presence_joined(User, Server, _Resource, _Packet) ->
  {ok, Client} = client(Server),
  case string:prefix(_Resource, "userlike-dashboard-") of
    nomatch ->
      eredis:q(Client, ["HSET", User, _Resource, "chat"]),
      eredis:q(Client, ["PUBLISH", "slot:online", User]);
    _ ->
      eredis:q(Client, ["HSET", User, _Resource, "dashboard"])
  end,
  none.

on_presence_left(User, Server, _Resource, _Status) ->
  {ok, Client} = client(Server),
  eredis:q(Client, ["HDEL", User,_Resource]),
  case string:prefix(_Resource, "userlike-dashboard-") of
    nomatch ->
      eredis:q(Client, ["PUBLISH", "slot:offline", User])
  end,
  none.

client(Server) ->
  case whereis(eredis_driver) of
    undefined ->
      case eredis:start_link("cache-master", 6379, 5) of
        {ok, Client} ->
          register(eredis_driver, Client),
          {ok, Client};
        {error, Reason} ->
          {error, Reason}
      end;
    Pid ->
      {ok, Pid}
  end.
