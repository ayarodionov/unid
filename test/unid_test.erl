-module(unid_test).

-include_lib("eunit/include/eunit.hrl").


-define(CNTR_LENGTH, 22).
-define(TIME_LENGTH, 32).
-define(MAX_32, 4294967295).


mk_base_test() ->
	A = round(math:pow(2,22)),
	B = round(math:pow(2,22+32)),
	?assertMatch(
		0, 
		unid_srv:mk_base(0, 0)),
	?assertMatch(
		A,
		unid_srv:mk_base(0, 1)),
	?assertMatch(
		B,
		unid_srv:mk_base(1, 0)),
	R1 = A + B,
	?assertMatch(
		R1,
		unid_srv:mk_base(1, 1)),
	R2 = 3*A + 5*B,
	?assertMatch(
		R2,
		unid_srv:mk_base(5, 3)),
	R3 = R2 + 7,
	?assertMatch(
		R3,
		unid_srv:mk_base(5, 3) bor 7),
	R4 = (((3 + ?MAX_32) * A) rem ?MAX_32) + 5*B + 7, 
	?assertMatch(
		R4,
		unid_srv:mk_base(5, 3) bor 7),
	ok.
 
