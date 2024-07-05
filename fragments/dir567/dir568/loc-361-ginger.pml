// git_link=loc-361-ginger.pml
#define  default true
#define  x1 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_4 = [1] of {int};
  run fun2(x1, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(int y5; chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Wgdef w13 ;
  run wg_monitor(w13);
  for(y9 : 0 .. (y5) - (1)) {
    for10: skip;
    w13.update!1;
    w13.update_ack?y8;
    assert y8;
    run fun3(w13, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(Wgdef w14; chan child_15) {
  bool y16 = false;
  bool y17 = false;
  int y18 = 0;
  bool y19 = true;
  int y20 = 0;
  defer1: skip;
  skip;
  w14.update!-(1);
  w14.update_ack?y17;
  assert y17;
  stop_process: skip;
  child_15!0;
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