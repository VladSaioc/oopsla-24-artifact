// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_territoryIDList  ?? // mand territoryIDList line 337
typedef Chandef {
	chan sync = [0] of {bool};
	chan enq = [0] of {bool};
	chan deq = [0] of {bool,bool};
	chan sending = [0] of {bool};
	chan rcving = [0] of {bool};
	chan closing = [0] of {bool};
	int size = 0;
	int num_msgs = 0;
	bool closed = false;
}



init {
	chan child_GetTerritoryV2ListByIDs3370 = [1] of {int};
	run GetTerritoryV2ListByIDs337(def_var_territoryIDList,child_GetTerritoryV2ListByIDs3370);
	child_GetTerritoryV2ListByIDs3370?0;
stop_process:skip
}

proctype GetTerritoryV2ListByIDs337(int var_territoryIDList;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousGetTerritoryV2ListByIDs3533530 = [1] of {int};
	chan channel_ch = [var_territoryIDList] of {int};
		for(i : 0.. var_territoryIDList-1) {
		for10: skip;
		run AnonymousGetTerritoryV2ListByIDs353353(channel_ch,child_AnonymousGetTerritoryV2ListByIDs3533530);
		run receiver(child_AnonymousGetTerritoryV2ListByIDs3533530);
		for10_end: skip
	};
	for10_exit: skip;


	if 	:: var_territoryIDList-1 != -3 ->
				for(i : 0.. var_territoryIDList-1) {
			for20: skip;
						channel_ch?0;


			if
			:: true ->
				goto for20_end
			:: true;
			fi;
			for20_end: skip
		};
		for20_exit: skip
	:: else ->
		do
		:: true ->
			for21: skip;
						channel_ch?0;


			if
			:: true ->
				goto for21_end
			:: true;
			fi;
			for21_end: skip
		:: true ->
			break
		od;
		for21_exit: skip
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype AnonymousGetTerritoryV2ListByIDs353353(chan channel_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
		channel_ch!0;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
