// git_link=loc-289-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun3(x1, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun3(int y5; chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  Wgdef w12 ;
  int y13 = x2;
  run wg_monitor(w12);
  for(y9 : y13 .. (y5) - (1)) {
    for10: skip;
    w12.update!1;
    w12.update_ack?y8;
    assert y8;
    w12.update!-(1);
    w12.update_ack?y8;
    assert y8;
    for10_end: skip;
  };
  for10_exit: skip;
  w12.wait?0;
  stop_process: skip;
  child_6!0;
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