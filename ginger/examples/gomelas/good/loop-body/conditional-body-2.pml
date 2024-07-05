// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_queryKeys277  ?? // mand queryKeys line 277
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
	chan child_GetDriversFromLocation2600 = [1] of {int};
	run GetDriversFromLocation260(child_GetDriversFromLocation2600);
	child_GetDriversFromLocation2600?0;
stop_process:skip
}

proctype GetDriversFromLocation260(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousGetDriversFromLocation2932930 = [1] of {int};
	int var_queryKeys = def_var_queryKeys277; // opt var_queryKeys


	if 	:: true ->
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
		goto stop_process
	:: true;
	fi;
	chan resChan_ch = [var_queryKeys] of {int};


	if 	:: true ->
		goto stop_process
	:: true;
	fi;
		for(i : 0.. var_queryKeys-1) {
		for10: skip;
		run AnonymousGetDriversFromLocation293293(resChan_ch,child_AnonymousGetDriversFromLocation2932930);
		run receiver(child_AnonymousGetDriversFromLocation2932930);
		for10_end: skip
	};
	for10_exit: skip;


	if 	:: var_queryKeys-1 != -3 ->
				for(i : 0.. var_queryKeys-1) {
			for20: skip;
						resChan_ch?0;
			for20_end: skip
		};
		for20_exit: skip
	:: else ->
		do
		:: true ->
			for21: skip;
						resChan_ch?0;
			for21_end: skip
		:: true ->
			break
		od;
		for21_exit: skip
	fi;


	if 	:: true ->
		goto stop_process
	:: true;
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype AnonymousGetDriversFromLocation293293(chan outchan_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;


	if 	:: true ->
				outchan_ch!0;
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
				outchan_ch!0;
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
				outchan_ch!0;
		goto stop_process
	:: true;
	fi;
		outchan_ch!0;
	goto stop_process;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
