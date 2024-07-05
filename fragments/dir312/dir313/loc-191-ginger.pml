// git_link=loc-191-ginger.pml
#define  default true
#define  x1 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_3 = [1] of {int};
  run fun2(x1, child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun2(int y4; chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  Wgdef w11 ;
  int y12 = 0;
  run wg_monitor(w11);
  w11.update!y4;
  w11.update_ack?y7;
  assert y7;
  w11.wait?0;
  goto stop_process;
  stop_process: skip;
  child_5!0;
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