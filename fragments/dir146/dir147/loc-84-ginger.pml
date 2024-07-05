// git_link=loc-84-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
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
  chan child_5 = [1] of {int};
  run fun3(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = 0;
  int y15 = 0;
  int y16 = x2;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: (y15) == (0) ->
    goto stop_process;
  :: else  ->
  fi;
  chan c19 = [0] of {int};
  chan c20 = [y16] of {int};
  for(y10 : 0 .. (y14) - (1)) {
    for20: skip;
    c20!0;
    run fun4(c20, c19, child_13);
    run receiver(child_13);
    for20_end: skip;
  };
  for20_exit: skip;
  if
  :: ((0) != (-(2))) && (((y15) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y15) - (1)) {
      for31: skip;
      c19?0;
      for31_end: skip;
    };
    for31_exit: skip;
  :: else  ->
    do
    :: true ->
      for30: skip;
      c19?0;
      for30_end: skip;
    :: true ->
      break;

    od;
    for30_exit: skip;
  fi;
  run close(c19);
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(chan c22; chan c23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  c23!0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  c22?0;
  stop_process: skip;
  child_24!0;
}
proctype receiver(chan c) {
  c?0;
}