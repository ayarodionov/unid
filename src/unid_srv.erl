
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
% <hr/>
% First of all let separate ids generated by different nodes. We need ten bits for representing
% numbers in the range 0 &lt;= node_id &lt;= 1023. So ids will be represented as 
% <pre> id = node_id*2**(64-10) + rest; where 0 &lt;= rest &lt; 2**54 </pre>
% So for each node_id its id range is 
% <pre> [node_id*2**54, (node_id+1)*2**54) ([ , ) is semi interval.)</pre>
% and the ranges do not overlap. 
% 
% To guarantee that after the application restarts ids will be unique
% we divide <i>rest</i> into two parts:
% <pre>rest = seconds*2**(54-32) + cntr; where 0 &lt;= cntr &lt; 2**22 </pre>
% For the same reason id ranges corresponding to different seconds do not overlap.
% 
% node_id is a fixed number, but seconds changes in two cases:
% <ol>
% <li> applications starts or restarts</li>
% <li> cntr value reaches maximum value 2**22-1</li>
% </ol>
% To calculate new seconds value erlang:system_time(seconds) is called. 
% This may cause different problems
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
% </ul>
% <hr/>
% Please write answers to the following discussion questions and 
% include them in your solution as comments:
% <ol>
% <li>
% Please describe your solution to get_id and why it is correct i.e. guaranteed globally unique.
% 
% This was explained in details already.
% 
% </li>
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
% </li>
% <li>
% Please enumerate possible failure cases and describe how your solution correctly handles each case.
% How did you verify correctness?
% Some example cases:
% <ol>
% <li>
% How do you manage uniqueness after a node crashes and restarts?  
% 
% This was explained in details already.
% 
% </li>
% <li>
% How do you manage uniqueness after the entire system fails and restarts?
% 
% The entire system contains of interdependent nodes which can be restarted
% independently.
% 
% </li>
% <li>
% How do you handle software defects?
% 
% Possible software problems are already discussed.
% 
% I also wrote some simple unitest cases. They are really basic, but while
% working on them I found bug in Erlang/OTP: 
% <a href="https://bugs.erlang.org/browse/ERL-1308">ERL-1308</a>.
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

%-----------------------------------------------------------------------------------------------
% -define(TEST, true).
-ifndef(TEST).
-compile({inline, [mk_base/2]}).
-else.
-export([mk_base/2]).
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