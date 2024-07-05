// git_link=loc-547-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_7 = [1] of {int};
  run fun4(x1, child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun4(int y8; chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  int y17 = x3;
  Wgdef w18 ;
  int y19 = x2;
  run wg_monitor(w18);
  for(y12 : 0 .. (y8) - (1)) {
    for10: skip;
    w18.update!1;
    w18.update_ack?y11;
    assert y11;
    run fun5(w18, y17, y19, child_16);
    run receiver(child_16);
    for10_end: skip;
  };
  for10_exit: skip;
  w18.wait?0;
  for(y12 : y19 .. (4096) - (1)) {
    for20: skip;
    w18.update!1;
    w18.update_ack?y11;
    assert y11;
    run fun6(w18, child_15);
    run receiver(child_15);
    for20_end: skip;
  };
  for20_exit: skip;
  w18.wait?0;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun5(Wgdef w20; int y21; int y22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  w20.update!-(1);
  w20.update_ack?y25;
  assert y25;
  stop_process: skip;
  child_23!0;
}
proctype fun6(Wgdef w29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  w29.update!-(1);
  w29.update_ack?y32;
  assert y32;
  stop_process: skip;
  child_30!0;
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