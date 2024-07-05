// git_link=loc-174-ginger.pml
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
  run fun4(x1, x2, child_6);
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
  int y17 = x3;
  chan c18 = [y7] of {int};
  if
  :: ((0) != (-(2))) && (((y7) - (1)) != (-(3))) ->
    for(y12 : 0 .. (y7) - (1)) {
      for11: skip;
      c18!0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      c18!0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  run wg_monitor(w16);
  for(y12 : 0 .. (y8) - (1)) {
    for20: skip;
    c18?0;
    w16.update!1;
    w16.update_ack?y11;
    assert y11;
    run fun5(c18, w16, child_15);
    run receiver(child_15);
    for20_end: skip;
  };
  for20_exit: skip;
  w16.wait?0;
  run close(c18);
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun5(chan c20; Wgdef w21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  c20!0;
  defer1: skip;
  skip;
  w21.update!-(1);
  w21.update_ack?y24;
  assert y24;
  stop_process: skip;
  child_22!0;
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