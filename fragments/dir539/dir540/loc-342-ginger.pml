// git_link=loc-342-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun3(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Wgdef w13 ;
  int y14 = x2;
  int y15 = x1;
  run wg_monitor(w13);
  for(y9 : 0 .. (y15) - (1)) {
    for10: skip;
    for(y9 : 0 .. (y14) - (1)) {
      for11: skip;
      w13.update!1;
      w13.update_ack?y8;
      assert y8;
      run fun4(w13, child_12);
      run receiver(child_12);
      for11_end: skip;
    };
    for11_exit: skip;
    w13.wait?0;
    for10_end: skip;
  };
  for10_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(Wgdef w16; chan child_17) {
  bool y18 = false;
  bool y19 = false;
  int y20 = 0;
  bool y21 = true;
  int y22 = 0;
  defer1: skip;
  skip;
  w16.update!-(1);
  w16.update_ack?y19;
  assert y19;
  stop_process: skip;
  child_17!0;
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