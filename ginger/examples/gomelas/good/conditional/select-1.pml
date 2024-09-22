// num_comm_params=3
// num_mand_comm_params=1
// num_opt_comm_params=2

#define def_var_k 	?? // mand k line 16
#define def_var_l1  ?? // opt l1 line 16
#define def_var_l2  ?? // opt l2 line 16

init {
	chan child_f16 = [1] of {int};
	run f16(def_var_k,def_var_l1,def_var_l2,child_f16);
	child_f16?0;
	stop_process:skip
}

proctype f16(int var_k;int var_l1;int var_l2;chan child) {
	chan child_receiver91 = [1] of {int};
	chan child_sender30 = [1] of {int};
	chan ch_ch = [def_var_k] of {int};

	run sender3(ch_ch,var_l1,child_sender30);
	run receiver9(ch_ch,var_l2,child_receiver91);
	child_receiver91?0;
	stop_process: skip;
	child!0
}
proctype sender3(chan ch_ch;int var_x;chan child) {
	int i;

	for(i : 0.. var_x-1) {
			for11: skip;

			ch_ch!0;
			for11_end: skip
		};
		for11_exit: skip;
	stop_process: skip;
	child!0
}

proctype receiver9(chan ch_ch;int var_y;chan child) {
	int i;
	int q1;
	int q2;

	for(i : 0.. var_y-1) {
		for21: skip;
		ch_ch?q1;
		ch_ch?q2;
		for21_end: skip
	};
	for21_exit: skip;

	if
	:: ch_ch?0 ->
		ch_ch?0;
	:: true;
	fi;
	ch_ch?0;
	if
	:: def_var_k == 0 -> ch_ch?0;
	fi;
	stop_process: skip;
	child!0
}
