// num_comm_params=2
// num_mand_comm_params=2
// num_opt_comm_params=0

#define default true
#define def_var_m_getBatchPredictChannelBufferSize242  2 // mand m.getBatchPredictChannelBufferSize(ctx) line 242
#define def_var_requestPages243  2 // mand requestPages line 243
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
	chan child_BatchPredict2290 = [1] of {int};
	run BatchPredict229(child_BatchPredict2290);
	child_BatchPredict2290?0;
	child_BatchPredict2290?0;
stop_process:skip
}

proctype BatchPredict229(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousBatchPredict2442440 = [1] of {int};
	int var_requestPages = def_var_requestPages243; // opt var_requestPages
	int var_m_getBatchPredictChannelBufferSize = def_var_m_getBatchPredictChannelBufferSize242; // opt var_m_getBatchPredictChannelBufferSize
	chan predictionChannel_ch = [0] of {int};


	if 	:: true ->
		goto stop_process
	:: true;
	fi;
		for(i : 0.. var_requestPages-1) {
		for10: skip;
		run AnonymousBatchPredict244244(predictionChannel_ch,child_AnonymousBatchPredict2442440);
		run receiver(child_AnonymousBatchPredict2442440);
		for10_end: skip
	};
	for10_exit: skip;


	if 	:: var_requestPages-1 != -3 ->
				for(i : 0.. var_requestPages-1) {
			for20: skip;
						predictionChannel_ch?_;
			for20_end: skip
		};
		for20_exit: skip
	:: else ->
		do
		:: true ->
			for21: skip;
						predictionChannel_ch?0;
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
proctype AnonymousBatchPredict244244(chan predictionChannel_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_BatchPredictPage2720 = [1] of {int};
	run BatchPredictPage272(predictionChannel_ch,child_BatchPredictPage2720);
	child_BatchPredictPage2720?0;
	stop_process: skip;
	child!0
}
proctype BatchPredictPage272(chan predictionChannel_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;


	if 	:: true ->
				predictionChannel_ch!0;
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
				predictionChannel_ch!0;
		goto stop_process
	:: true;
	fi;
		predictionChannel_ch!0;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
