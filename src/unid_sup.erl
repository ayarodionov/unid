%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2020 Anatoly Rodionov.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%%-------------------------------------------------------------------
%% @doc unid top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(unid_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, which_children/0, count_children/0]).

-define(SERVER, ?MODULE).

% @doc Starts {@module}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% @doc Initiates  {@module}.
init([]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 1},
    ChildSpecs = case application:get_env(unid, node_id, false) of 
    	false  -> [];
    	NodeId ->
    		[#{
    			id      => unid_srv,
    			start   => {unid_srv, start_link, [NodeId]},
    			type    => worker,
    			restart => permanent,
    			modules => [unid_srv]
    		}]
    end,
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

which_children() ->
    supervisor:which_children(?MODULE).

count_children() ->
    supervisor:count_children(?MODULE).
