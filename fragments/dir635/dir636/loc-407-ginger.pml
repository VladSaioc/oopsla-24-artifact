// git_link=loc-407-ginger.pml
#define  default true
#define  x1 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_5 = [1] of {int};
  run fun2(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  int y15 = x1;
  run wg_monitor(w14);
  w14.update!y15;
  w14.update_ack?y8;
  assert y8;
  run fun3(w14, child_13);
  run receiver(child_13);
  run fun4(w14, child_12);
  run receiver(child_12);
  w14.wait?0;
  if
  :: true ->
    goto stop_process;
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;

  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(Wgdef w18; chan child_19) {
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
proctype fun4(Wgdef w25; chan child_26) {
  bool y27 = false;
  bool y28 = false;
  int y29 = 0;
  bool y30 = true;
  int y31 = 0;
  defer1: skip;
  skip;
  w25.update!-(1);
  w25.update_ack?y28;
  assert y28;
  stop_process: skip;
  child_26!0;
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