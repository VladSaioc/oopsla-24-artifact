// num_comm_params=3
// num_mand_comm_params=1
// num_opt_comm_params=2

#define def_var_k 	?? // mand k line 16
#define def_var_l1  ?? // opt l1 line 16
#define def_var_l2  ?? // opt l2 line 16
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
	chan child_f16 = [1] of {int};
	run f16(def_var_k,def_var_l1,def_var_l2,child_f16);
	child_f16?0;
	stop_process:skip
}

proctype f16(int var_k;int var_l1;int var_l2;chan child) {
	bool closed; 
	bool ok;
	int i;
	int j;
	bool state = true;
	int num_msgs;
	chan child_receiver91 = [1] of {int};
	chan child_sender30 = [1] of {int};
	chan ch_ch = [def_var_k] of {int};
	
	for(i : 0.. var_l1-1) {
		for11: skip;
		run sender3(ch_ch, var_l1, child_sender30);
		for11_end: skip;
	};
	for11_exit: skip;
	
	for(j : 0.. var_l2-1) {
		for21: skip;
		run receiver9(ch_ch,var_l2,child_receiver91);
		for21_exit: skip;
	};
	for21_exit: skip;
	stop_process: skip;
	child!0;
}

proctype sender3(chan ch_ch;int var_x;chan child) {
	ch_ch!0;
}

proctype receiver9(chan ch_ch;int var_y;chan child) {
	ch_ch?0;
	ch_ch?0;
}
