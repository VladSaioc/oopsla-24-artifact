// num_comm_params=2
// num_mand_comm_params=2
// num_opt_comm_params=0

#define default true
#define def_var_fliprActiveUsers152  ?? // mand fliprActiveUsers line 152
#define def_var_nonExpiredActiveUsers154  ?? // mand nonExpiredActiveUsers line 154
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
	chan child_SyncActiveUsers1260 = [1] of {int};
	run SyncActiveUsers126(child_SyncActiveUsers1260);
	child_SyncActiveUsers1260?0;
stop_process:skip
}

proctype SyncActiveUsers126(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousSyncActiveUsers1551550 = [1] of {int};
	int var_nonExpiredActiveUsers = def_var_nonExpiredActiveUsers154; // opt var_nonExpiredActiveUsers
	int var_fliprActiveUsers = def_var_fliprActiveUsers152; // opt var_fliprActiveUsers


	if
	:: true ->
		goto stop_process
	:: true;
	fi;


	if
	:: true ->
		goto stop_process
	:: true;
	fi;
	chan sem_ch = [var_fliprActiveUsers] of {int};
		for(i : 0.. var_nonExpiredActiveUsers-1) {
		for10: skip;
		run AnonymousSyncActiveUsers155155(sem_ch,child_AnonymousSyncActiveUsers1551550);
		run receiver(child_AnonymousSyncActiveUsers1551550);
		for10_end: skip
	};
	for10_exit: skip;


	if
	:: 0 != -2 && var_fliprActiveUsers-1 != -3 ->
				for(i : 0.. var_fliprActiveUsers-1) {
			for21: skip;
						sem_ch?0;

			for21_end: skip
		};
		for21_exit: skip
	:: else ->
		do
		:: true ->
			for20: skip;
						sem_ch?0;


			if
			:: true ->
				goto for20_end
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
proctype AnonymousSyncActiveUsers155155(chan sem_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
