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

-module(unid_test).

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------

-define(CNTR_LENGTH, 22).
-define(TIME_LENGTH, 32).
-define(MAX_32, 4294967295).
-define(MAX_PER_SECOND, 100000).
-define(ONE_SEC, 1000000).  % I sec in microseconds

%-----------------------------------------------------------------------------------------------

% @doc Testing correctness of binary arithmetics usage.
% Theoretically it will not work if Erlang does not use
% 64 bit integers
mk_base_test() ->
	A = round(math:pow(2,22)),
	B = round(math:pow(2,22+32)),
	?assertEqual(
		0, 
		unid_srv:mk_base(0, 0)),
	?assertEqual(
		A,
		unid_srv:mk_base(0, 1)),
	?assertEqual(
		B,
		unid_srv:mk_base(1, 0)),
	R1 = A + B,
	?assertEqual(
		R1,
		unid_srv:mk_base(1, 1)),
	R2 = 3*A + 5*B,
	?assertEqual(
		R2,
		unid_srv:mk_base(5, 3)),
	R3 = R2 + 7,
	?assertEqual(
		R3,
		unid_srv:mk_base(5, 3) bor 7),
	R4 = (((3 + ?MAX_32) * A) rem ?MAX_32) + 5*B + 7, 
	?assertEqual(
		R4,
		unid_srv:mk_base(5, 3) bor 7).
 
% @doc Testing get_id uniqueness and speed test
get_id_test() ->
	{ok, _} = unid_srv:start(111),
	?assertNot(unid_srv:get_id() == unid_srv:get_id()),
	?assertNot(unid_srv:get_id() == unid_srv:get_id()),
	?assertNot(unid_srv:get_id() == unid_srv:get_id()),
	?assert(unid_srv:tc(?MAX_PER_SECOND) =< ?ONE_SEC).

%-----------------------------------------------------------------------------------------------
