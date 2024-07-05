// git_link=loc-154-ginger.pml
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
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_6 = [1] of {int};
  run fun2(x1, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun2(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  Wgdef w17 ;
  int y18 = y7;
  run wg_monitor(w17);
  w17.update!y18;
  w17.update_ack?y10;
  assert y10;
  for(y11 : 0 .. (y7) - (1)) {
    for10: skip;
    run fun3(w17, child_16);
    run receiver(child_16);
    for10_end: skip;
  };
  for10_exit: skip;
  w17.wait?0;
  chan c19 = [0] of {int};
  chan c20 = [0] of {int};
  run fun4(c19, c20, w17, child_15);
  run receiver(child_15);
  run fun5(c19, c20, w17, child_14);
  run receiver(child_14);
  c19?0;
  c20?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun3(Wgdef w21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  defer1: skip;
  skip;
  w21.update!-(1);
  w21.update_ack?y24;
  assert y24;
  stop_process: skip;
  child_22!0;
}
proctype fun4(chan c28; chan c29; Wgdef w30; chan child_31) {
  bool y32 = false;
  bool y33 = false;
  int y34 = 0;
  bool y35 = true;
  int y36 = 0;
  c28!0;
  stop_process: skip;
  child_31!0;
}
proctype fun5(chan c37; chan c38; Wgdef w39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  c38!0;
  stop_process: skip;
  child_40!0;
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