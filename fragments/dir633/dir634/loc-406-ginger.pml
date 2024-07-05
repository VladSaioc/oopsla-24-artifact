// git_link=loc-406-ginger.pml
#define  default true
#define  x1 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_7 = [1] of {int};
  run fun2(child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun2(chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  Wgdef w18 ;
  int y19 = x1;
  run wg_monitor(w18);
  w18.update!y19;
  w18.update_ack?y10;
  assert y10;
  run fun3(w18, child_17);
  run receiver(child_17);
  run fun4(w18, child_16);
  run receiver(child_16);
  run fun5(w18, child_15);
  run receiver(child_15);
  run fun6(w18, child_14);
  run receiver(child_14);
  w18.wait?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun3(Wgdef w20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  defer1: skip;
  skip;
  w20.update!-(1);
  w20.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
}
proctype fun4(Wgdef w27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  defer1: skip;
  skip;
  w27.update!-(1);
  w27.update_ack?y30;
  assert y30;
  stop_process: skip;
  child_28!0;
}
proctype fun5(Wgdef w34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  defer1: skip;
  skip;
  w34.update!-(1);
  w34.update_ack?y37;
  assert y37;
  stop_process: skip;
  child_35!0;
}
proctype fun6(Wgdef w41; chan child_42) {
  bool y43 = false;
  bool y44 = false;
  int y45 = 0;
  bool y46 = true;
  int y47 = 0;
  defer1: skip;
  skip;
  w41.update!-(1);
  w41.update_ack?y44;
  assert y44;
  stop_process: skip;
  child_42!0;
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