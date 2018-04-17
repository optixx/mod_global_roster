-module(mod_global_roster).

-behavior(gen_mod).
-behavior(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-export([
  start/2, stop/1, reload/3, depends/2, mod_opt_type/1,
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  on_presence_joined/4, on_presence_left/4]).

-record(entry, {user, server, resource, status}).
-record(state, {options, seen, host}).


% gen_mod impl

start(Host, Opts) ->
  gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
  gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, _OldOpts) ->
  Proc = gen_mod:get_module_proc(Host, ?MODULE),
  gen_server:cast(Proc, {reload, NewOpts}),
  ok.

depends(_, _) -> [].

mod_opt_type(hosts) ->
  fun (L) -> [iolist_to_binary(H) || H <- L] end;
mod_opt_type(slot_http_username) -> fun iolist_to_binary/1;
mod_opt_type(slot_http_password) -> fun iolist_to_binary/1;
mod_opt_type(api_url) -> fun iolist_to_binary/1;
mod_opt_type(http_timeout) ->
  fun (infinity) -> infinity;
      (I) when is_integer(I), I > 0 -> I
  end;
mod_opt_type(_) -> [hosts, slot_http_username, slot_http_password, api_url, http_timeout].


% gen_server impl

init([Host, Opts]) ->
  process_flag(trap_exit, true),
  Proc = gen_mod:get_module_proc(Host, ?MODULE),
  Seen = ets:new(Proc, [private]),
  ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence_joined, 50),
  ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_presence_left, 50),
  {ok, #state{options = Opts, seen = Seen, host = Host}}.

handle_call(Request, _From, State) ->
  ?ERROR_MSG("Received unexpected call: ~p", [Request]),
  {reply, none, State}.

handle_cast({add, #entry{server = Server} = Entry}, #state{options = Opts} = State) ->
  case lists:member(Server, gen_mod:get_opt(hosts, Opts)) of
    true -> process_add(Entry, State);
    false -> ok
  end,
  {noreply, State};

handle_cast({reload, NewOpts}, State) ->
  {noreply, State#state{options = NewOpts}};

handle_cast(Request, State) ->
  ?ERROR_MSG("Received unexpected cast: ~p", [Request]),
  {noreply, State}.

handle_info({http, ReplyInfo}, State) ->
  case ReplyInfo of
    {_RequestId, {error, Reason}} ->
      ?ERROR_MSG("Failed sending roster HTTP request: ~p", [Reason]);
    {{_HttpVersion, Code, _HttpReason}, _Headers, Body} when Code > 399 ->
      ?ERROR_MSG("Got unexpected ~w response: ~p", [Code, Body]);
    _ -> ok
  end,
  {noreply, State};

handle_info(Request, State) ->
  ?ERROR_MSG("Received unexpected info: ~p", [Request]),
  {noreply, State}.

terminate(_Reason, #state{host = Host, seen = Seen}) ->
  ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_presence_joined, 50),
  ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_presence_left, 50),
  ets:delete(Seen),
  ok.


% hooks

on_presence_joined(User, Server, Resource, _Packet) ->
  Proc = gen_mod:get_module_proc(Server, ?MODULE),
  gen_server:cast(Proc, {add, #entry{user = User, server = Server, resource = Resource, status = online}}),
  none.

on_presence_left(User, Server, Resource, _Status) ->
  Proc = gen_mod:get_module_proc(Server, ?MODULE),
  gen_server:cast(Proc, {add, #entry{user = User, server = Server, resource = Resource, status = offline}}),
  none.


% internal functions

format_entry(Entry) ->
  {lists:zip(record_info(fields, entry), tl(tuple_to_list(Entry)))}.

process_add(#entry{status = Status, user = User, resource = Resource} = Entry, #state{options = Opts, seen = Seen}) ->
  case Status of
    online ->
      case ets:insert_new(Seen, {{User, Resource}}) of
        true -> send_entry(Entry, Opts);
        false -> ok
      end;
    offline ->
      ets:delete(Seen, {User, Resource}),
      send_entry(Entry, Opts)
  end,
  ok.

send_entry(Entry, Opts) ->
  Url = gen_mod:get_opt(api_url, Opts),
  User = gen_mod:get_opt(slot_http_username, Opts),
  Pass = gen_mod:get_opt(slot_http_password, Opts),
  Timeout = gen_mod:get_opt(http_timeout, Opts),
  Header = [{"Authorization", "Basic " ++ base64:encode_to_string(iolist_to_binary([User, <<":">>, Pass]))}],
  Body = jiffy:encode(format_entry(Entry)),
  HttpOpts = [{ssl, [{versions, ['tlsv1.2']}, {verify, verify_none}]}, {timeout, Timeout}, {connect_timeout, Timeout}],
  ClientOpts = [{sync, false}],
  httpc:request(post, {binary_to_list(Url), Header, "application/json", Body}, HttpOpts, ClientOpts),
  ok.
