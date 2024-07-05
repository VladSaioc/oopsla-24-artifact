// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_drivers  ?? // mand drivers line 1114
#define ub_for1150_2  ??
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
	chan child_getDriverBGCStatusV211140 = [1] of {int};
	run getDriverBGCStatusV21114(def_var_drivers,child_getDriverBGCStatusV211140);
	child_getDriverBGCStatusV211140?0;
stop_process:skip
}

proctype getDriverBGCStatusV21114(int var_drivers;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousgetDriverBGCStatusV2111711170 = [1] of {int};
	chan ch_ch = [0] of {int};
		for(i : 0.. var_drivers-1) {
		for10: skip;
		run AnonymousgetDriverBGCStatusV211171117(ch_ch,child_AnonymousgetDriverBGCStatusV2111711170);
		run receiver(child_AnonymousgetDriverBGCStatusV2111711170);
		for10_end: skip
	};
	for10_exit: skip;


	if
	:: 0 != -2 && ub_for1150_2-1 != -3 ->
				for(i : 0.. ub_for1150_2-1) {
			for21: skip;
						ch_ch?0;


			if 			:: true;
			:: true;
			fi;
			for21_end: skip
		};
		for21_exit: skip
	:: else ->
		do
		:: true ->
			for20: skip;
						ch_ch?0;


			if 			:: true;
			:: true;
			fi;
			for20_end: skip
		:: true ->
			break
		od;
		for20_exit: skip
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype AnonymousgetDriverBGCStatusV211171117(chan ch_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;


	if 	:: true ->
				ch_ch!0;
		goto stop_process
	:: true ->


		if 		:: true ->
						ch_ch!0;
			goto stop_process
		:: true;
		fi
	fi;


	if 	:: true ->
				ch_ch!0;
		goto stop_process
	:: true;
	fi;
		ch_ch!0;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
