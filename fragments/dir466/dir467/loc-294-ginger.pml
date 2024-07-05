// git_link=loc-294-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_6 = [1] of {int};
  run fun4(x1, x3, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(int y7; int y8; chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  Wgdef w16 ;
  int y17 = 0;
  int y18 = x2;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c20 = [y7] of {int};
  run wg_monitor(w16);
  w16.update!y18;
  w16.update_ack?y11;
  assert y11;
  for(y12 : 0 .. (y18) - (1)) {
    for10: skip;
    run fun5(c20, w16, child_15);
    run receiver(child_15);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y8) - (1)) != (-(3)) ->
    for(y12 : 0 .. (y8) - (1)) {
      for20: skip;
      c20!0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c20!0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  w16.wait?0;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun5(chan c22; Wgdef w23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  defer1: skip;
  skip;
  w23.update!-(1);
  w23.update_ack?y26;
  assert y26;
  stop_process: skip;
  child_24!0;
}
proctype wg_monitor(Wgdef wg) {
  int i = 0;
  end: skip;
  do
  :: wg.update?i ->
    wg.Counter = (wg.Counter) + (i);
    wg.update_ack!(wg.Counter) >= (0);
  :: (wg.Counter) == (0) ->
    end1: skip;
    if
    :: wg.update?i ->
      wg.Counter = (wg.Counter) + (i);
      wg.update_ack!(wg.Counter) >= (0);
    :: wg.wait!0 ->


    fi;

  od;
}
proctype receiver(chan c) {
  c?0;
}