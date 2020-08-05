-module(rcl_memkv_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rcl_memkv_sup:start_link() of
      {ok, Pid} ->
          ok = riak_core:register([{vnode_module, rcl_memkv_key_store_vnode}]),
          ok = riak_core_node_watcher:service_up(rcl_memkv_key_store, self()),

          {ok, Pid};
      {error, Reason} ->
          {error, Reason}
    end.

stop(_State) ->
    ok.
