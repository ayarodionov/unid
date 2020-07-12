
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc <b>Problem</b>
% 
% Imagine you are building a system to assign unique numbers to each resource that 
% you manage. You want the ids to be guaranteed unique i.e. no UUIDs.  
% Since these ids are globally unique, each id can only be given out at most once. 
% The ids are 64 bits long.
% 
% Your service is composed of a set of nodes, each running one process serving ids.
% A caller will connect to one of the nodes and ask it for a globally unique id.  
% There are a fixed number of nodes in the system, up to 1024.  Each node has a numeric id, 
% 0 &lt;= id &lt;= 1023. Each node knows its id at startup and that id never changes for the node.
% 
% The task is to implement get_id.  When a caller requests a new id, the node it connects 
% to calls its internal get_id function to get a new, globally unique id.
% 
% Assume that any node will not receive more than 100,000 requests per second.
% 
% <hr/>
% Please write answers to the following discussion questions and 
% include them in your solution as comments:
% <ol>
% <li>
% Please describe your solution to get_id and why it is correct i.e. guaranteed globally unique.
% 
% We will divide the 64 bit UUID into three parts. 
% <ol>
% <li> 10 bit node_id, to represent an ID in the range of 0-1023</li>
% <ol><li> node_id is guaranteed unique per node in cluster </li></ol>
% <li> Timestamp in seconds of 32 bits length sufficient to guaranty uniqueness time requirement and is updated when:</li>
% <ol>
% <li>Process restarts, during a normal application restart or a crash</li>
% <li>When counter reaches maximum value (8388607)</li>
% </ol>
% <li> Counter occupies rest 22 bits</li>
% <ol>
% <li>Counter increments with each request from 0 to maximum value (8388607)</li>
% <li>When maximum is reached timestamp part of UUID is updated and counter restarts at 0</li>
% </ol>
% </ol>
% 
% </li>
% 
% <li>
% Please explain how your solution achieves the desired performance i.e. 100,000 or more requests per second per node.
% How did you verify this?
% 
% Almost all calls required only one fast arithmetics operation - adding 1, and modifying record in one 
% standard Erlang call back. Only one of 8388607 calls requires calling one extra function mk_base
% which contains four basic arithmetics operations and declared inline.
% 
% Real performance can't be tested until request mechanism is specified. 
% (Is it rpc call, HTTP request, tcp or upd connection.) From my experience this will be the 
% most time consuming part.
% 
% Unitest includes speed test for direct get_id calls.
% 
% </li>
% <li>
% Please enumerate possible failure cases and describe how your solution correctly handles each case.
% How did you verify correctness?
% Some example cases:
% <ol>
% <li>
% How do you manage uniqueness after a node crashes and restarts?  
% 
% On a process restart, regardless of cause, the timestamp part of the UUID is updated with 1 second resolution. 
% On start the process will also sleep to make sure it can't restart more than 1 time per second.
% 
% </li>
% <li>
% How do you manage uniqueness after the entire system fails and restarts?
% 
% The entire system is made up of independent nodes which can be restarted
% independently or at the same time. The only requirements are that the system can't have two nodes provisioned with same node_id,
% and that system time is configured consistently between restarts. 
% 
% </li>
% <li>
% How do you handle software defects?
%
% <ul>
% <li> erlang:system_time is monotonic function in all latest implementations, but it is not
% strictly monotonic. But one second interval is large enough and the function will be called 
% only during restart or after 83.9 seconds in case of 100000 calls per second rate, so
% there is no chances that erlang:system_time will return the same value twice.
% </li>
% <li> 32 bits unsigned integer will be eventually repeated. Original UNIX used 32 singed
% bits integer to store seconds. 
% One second after 03:14:07 UTC 2038-01-19 this representation will overflow.
% On 64 bit boxes Erlang uses 64 bits signed integer to overcome this problem. 
% For this reason I added conversion seconds to least significant 32 bits.
% Taken into account that maximum  singed integer is 2147483647 and there is 86400
% seconds in UNIX day, application will work correctly for next 68 years.
% </li>
% <li>Solution depends on NTP service. If the application is moved to another box with
% no NTP service or malfunctioning NTP service then application may repeats ids.
% </li>
% <li>Solution is not fully protected against configuration errors. 
% Application will not start if node_id is not in the [0, 1023] range,
% but there is no check that all nodes have unique node_id.</li>
% <li>I also wrote some simple unitest cases.</li>
% </ul>
% 
% </li>
% </ol>
% </li>
% </ol>
% <hr/>
% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------------------------

-module(unid_srv).
-behavior(gen_server).
-vsn(1.0).

%
%-----------------------------------------------------------------------------------------------
-export([start_link/1, init/1]).
-export([handle_cast/2, handle_call/3]).
-export([get_id/0, node_id/0, info/0, timestamp/0]).
-export([tc/1]).

%-----------------------------------------------------------------------------------------------
-ifdef(TEST).
-export([mk_base/2, start/1]).
-else.
-compile({inline, [mk_base/2]}).
-endif.

%-----------------------------------------------------------------------------------------------
-define(START_DELAY, 1000).
-define(MAX_22, 8388607).
-define(MAX_32, 4294967295).
-define(CNTR_LENGTH, 22).
-define(TIME_LENGTH, 32).

%-----------------------------------------------------------------------------------------------
-record(s_t, {
	node_id     :: non_neg_integer(),
	base        :: non_neg_integer(),
	cntr  = 0   :: non_neg_integer()
    }).
-type s_t() :: #s_t{}.

%-----------------------------------------------------------------------------------------------
% Interfaces
%-----------------------------------------------------------------------------------------------
-ifdef(TEST).
-spec start(non_neg_integer()) -> {ok, pid()}.
% @doc Starts {@module}.
start(NodeId) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {NodeId}, []).
-endif.

-spec start_link(non_neg_integer()) -> {ok, pid()}.
% @doc Starts {@module}.
start_link(NodeId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {NodeId}, []).

-spec init({non_neg_integer()}) -> {ok, s_t()}. 
% @doc Initiates  {@module}.
init({NodeId})  when 0 =< NodeId andalso NodeId =< 1023 ->
	timer:sleep(?START_DELAY),   % make sure that time intervals not overlapped after restart
    {ok, #s_t{
    	node_id = NodeId,
    	base = mk_base(NodeId, erlang:system_time(seconds))
    	}}.

%-----------------------------------------------------------------------------------------------
-spec get_id() -> non_neg_integer().
% @doc Returns unique id
get_id() ->
    gen_server:call(?MODULE, get_id).

-spec node_id() -> non_neg_integer().
% @doc Returns node id
node_id() ->
    gen_server:call(?MODULE, node_id).

-spec info() -> s_t().
% @doc Returns information about current state
info() ->
    gen_server:call(?MODULE, info).

-spec timestamp() -> non_neg_integer().
% @doc Returns time stamp since the epoch in milliseconds.
timestamp() -> erlang:system_time(milli_seconds).

%-----------------------------------------------------------------------------------------------
% Callbacks
%-----------------------------------------------------------------------------------------------
% To avoid warning
handle_cast(_, ST) ->
    {noreply, ST}.

handle_call(get_id, _From, ST) when ST#s_t.cntr =< ?MAX_22 ->
    {reply, ST#s_t.base bor ST#s_t.cntr, ST#s_t{cntr = ST#s_t.cntr + 1}};

handle_call(get_id, _From, ST) ->
	Base = mk_base(ST#s_t.node_id, erlang:system_time(seconds)),
    {reply, Base, ST#s_t{base = Base, cntr = 1}};

handle_call(node_id, _From, ST) ->
    {reply, ST#s_t.node_id, ST};

handle_call(info, _From, ST) ->
    {reply, ST, ST}.

%-----------------------------------------------------------------------------------------------
% private
%-----------------------------------------------------------------------------------------------
-spec mk_base(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
% @doc Creates base for id calculation
mk_base(NodeId, Seconds) ->
	((NodeId bsl ?TIME_LENGTH) bor (Seconds band ?MAX_32)) bsl ?CNTR_LENGTH.

%-----------------------------------------------------------------------------------------------
-spec repeat(non_neg_integer()) -> ok.
% @doc Repeats get_id() N times
repeat(0) -> ok;
repeat(N) -> _ = get_id(), repeat(N - 1).

-spec tc(non_neg_integer()) -> non_neg_integer().
% @doc Testing get_id speed.
% 
% For example: unid_srv:tc(100).
% @param N number of repetition
% @returns time in microseconds (0.000001 of a second)
tc(N) -> 
    {T, _} = timer:tc(fun() -> repeat(N) end),
    T.
%-----------------------------------------------------------------------------------------------
