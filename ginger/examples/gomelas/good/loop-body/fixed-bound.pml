// num_comm_params=0
// num_mand_comm_params=0
// num_opt_comm_params=0

#define default true
#define ub_for53_2  ??
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
	chan child_increment400 = [1] of {int};
	run increment40(child_increment400);
	child_increment400?0;
stop_process:skip
}

proctype increment40(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_Anonymousincrement47470 = [1] of {int};
	chan errCh_ch = [1] of {int};
	int var_addrs = 3;
	for(i : 0.. var_addrs-1) {
		for10: skip;
		run Anonymousincrement4747(errCh_ch,child_Anonymousincrement47470);
		run receiver(child_Anonymousincrement47470);
		for10_end: skip
	};
	for10_exit: skip;


	if
	:: 0 != -2 && ub_for53_2-1 != -3 ->
				for(i : 0.. ub_for53_2-1) {
			for21: skip;
						errCh_ch?0;
			for21_end: skip
		};
		for21_exit: skip
	:: else ->
		do
		:: true ->
			for20: skip;
						errCh_ch?0;
			for20_end: skip
		:: true ->
			break
		od;
		for20_exit: skip
	fi;


	if
	:: true ->
		goto stop_process
	:: true;
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype Anonymousincrement4747(chan errCh_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
		errCh_ch!0;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
