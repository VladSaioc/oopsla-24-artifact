// git_link=loc-416-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_6 = [1] of {int};
  run fun3(x1, x2, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(int y7; int y8; chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  Wgdef w17 ;
  int y18 = 0;
  run wg_monitor(w17);
  for(y12 : 0 .. (y7) - (1)) {
    for20: skip;
    w17.update!1;
    w17.update_ack?y11;
    assert y11;
    run fun4(w17, child_16);
    run receiver(child_16);
    for20_end: skip;
  };
  for20_exit: skip;
  w17.wait?0;
  for(y12 : 0 .. (y8) - (1)) {
    for30: skip;
    w17.update!1;
    w17.update_ack?y11;
    assert y11;
    run fun5(w17, y7, child_15);
    run receiver(child_15);
    for30_end: skip;
  };
  for30_exit: skip;
  w17.wait?0;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun4(Wgdef w19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  w19.update!-(1);
  w19.update_ack?y22;
  assert y22;
  stop_process: skip;
  child_20!0;
}
proctype fun5(Wgdef w26; int y27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  w26.update!-(1);
  w26.update_ack?y30;
  assert y30;
  stop_process: skip;
  child_28!0;
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