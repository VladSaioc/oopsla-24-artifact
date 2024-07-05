// git_link=loc-422-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun4(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  int y15 = x2;
  int y16 = x1;
  run wg_monitor(w14);
  for(y10 : 0 .. (x3) - (1)) {
    for10: skip;
    w14.update!1;
    w14.update_ack?y9;
    assert y9;
    run fun5(w14, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  w14.wait?0;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun5(Wgdef w17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  int y24 = -(2);
  defer1: skip;
  skip;
  w17.update!-(1);
  w17.update_ack?y20;
  assert y20;
  stop_process: skip;
  child_18!0;
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