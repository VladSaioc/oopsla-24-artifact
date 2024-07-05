// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_addrs83 ?? // mand addrs line 83
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
	chan child_processWithBackupRequest660 = [1] of {int};
	run processWithBackupRequest66(child_processWithBackupRequest660);
	child_processWithBackupRequest660?0;
stop_process:skip
}

proctype processWithBackupRequest66(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousprocessWithBackupRequest99831 = [1] of {int};
	chan child_AnonymousprocessWithBackupRequest87830 = [1] of {int};
	int var_addrs = def_var_addrs83; // opt var_addrs
	chan chResults_ch = [var_addrs] of {int};


	if 	:: true ->
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
		goto stop_process;
	:: true;
	fi;
	run AnonymousprocessWithBackupRequest8783(chResults_ch,child_AnonymousprocessWithBackupRequest87830);
	run receiver(child_AnonymousprocessWithBackupRequest87830);
	if 	:: true ->
		goto stop_process
	:: true ->
		run AnonymousprocessWithBackupRequest9983(chResults_ch,child_AnonymousprocessWithBackupRequest99831);
		run receiver(child_AnonymousprocessWithBackupRequest99831);
		if 		:: true ->
			goto stop_process;
		chResults_ch?0;


		if 		:: true ->
			if 			:: true ->
				goto stop_process;
			chResults_ch?0;
			goto stop_process;
			fi;
			for30_exit: skip;
			for30_end: skip
		:: true ->
			goto stop_process;
		fi;
		break;
		fi;
		for20_exit: skip;
		for20_end: skip;
		break;
	chResults_ch?0;


	if 	:: true ->
		goto stop_process
	:: true;
	fi;
	goto stop_process
	fi;
	for10_exit: skip;
	for10_end: skip;
	stop_process: skip;
	child!0
}
proctype AnonymousprocessWithBackupRequest8783(chan chResults_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
		chResults_ch!0;
	stop_process: skip;
	child!0
}
proctype AnonymousprocessWithBackupRequest9983(chan chResults_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
		chResults_ch!0;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
