// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_inputs  ?? // mand inputs line 168
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
	chan child_Batch1680 = [1] of {int};
	run Batch168(def_var_inputs,child_Batch1680);
	child_Batch1680?0;
stop_process:skip
}

proctype Batch168(int var_inputs;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousBatch1781780 = [1] of {int};
	chan outputChan_ch = [var_inputs] of {int};
		for(i : 0.. var_inputs-1) {
		for10: skip;
		run AnonymousBatch178178(outputChan_ch,child_AnonymousBatch1781780);
		run receiver(child_AnonymousBatch1781780);
		for10_end: skip
	};
	for10_exit: skip;


	if
	:: 0 != -2 && var_inputs-1 != -3 ->
				for(i : 0.. var_inputs-1) {
			for21: skip;
						outputChan_ch?0;
			for21_end: skip
		};
		for21_exit: skip
	:: else ->
		do
		:: true ->
			for20: skip;
						outputChan_ch?0;
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
proctype AnonymousBatch178178(chan outputChan_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;


	if
	:: true ->
		if
		:: true -> outputChan_ch!0; goto stop_process
		:: true;
		fi;
		outputChan_ch!0
	:: true ->
		if
		:: true ->
			if
			:: true -> outputChan_ch!0; goto stop_process
			:: true;
			fi;
			outputChan_ch!0
		:: true ->
			if
			:: true ->
				if
				:: true -> outputChan_ch!0; goto stop_process
				:: true;
				fi;
				outputChan_ch!0
			:: true ->
				if
				:: true ->
					if
					:: true -> outputChan_ch!0; goto stop_process
					:: true;
					fi;
					outputChan_ch!0
				:: true -> outputChan_ch!0;
				fi
			fi
		fi
	fi;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
