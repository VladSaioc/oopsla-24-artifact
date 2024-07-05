// git_link=loc-454-ginger.pml
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
  int y15 = x3;
  Wgdef w16 ;
  int y17 = x2;
  run wg_monitor(w16);
  w16.update!y15;
  w16.update_ack?y10;
  assert y10;
  for(y11 : 0 .. (x3) - (1)) {
    for20: skip;
    run fun5(w16, child_14);
    run receiver(child_14);
    for20_end: skip;
  };
  for20_exit: skip;
  w16.wait?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun5(Wgdef w18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  defer1: skip;
  skip;
  w18.update!-(1);
  w18.update_ack?y21;
  assert y21;
  stop_process: skip;
  child_19!0;
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