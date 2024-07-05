// git_link=loc-363-ginger.pml
#define  default true
#define  x1 ??
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
  chan child_3 = [1] of {int};
  run fun2(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun2(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  int y10 = -(2);
  int y11 = x1;
  if
  :: true ->
    chan c12 = [y11] of {int};
    chan c13 = [1] of {int};
    goto stop_process;
  :: true ->
    chan c14 = [y11] of {int};
    chan c15 = [1] of {int};
    goto stop_process;
  :: true ->
    chan c16 = [y11] of {int};
    chan c17 = [1] of {int};
    goto stop_process;

  fi;
  stop_process: skip;
  child_4!0;
}