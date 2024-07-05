// git_link=loc-347-ginger.pml
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
  run fun4(x1, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  int y16 = x3;
  int y17 = x2;
  int y18 = -(2);
  run wg_monitor(w15);
  for(y11 : 0 .. (y17) - (1)) {
    for30: skip;
    w15.update!1;
    w15.update_ack?y10;
    assert y10;
    run fun5(w15, child_14);
    run receiver(child_14);
    for30_end: skip;
  };
  for30_exit: skip;
  w15.wait?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun5(Wgdef w19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  defer1: skip;
  skip;
  w19.update!-(1);
  w19.update_ack?y22;
  assert y22;
  stop_process: skip;
  child_20!0;
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