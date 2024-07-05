// git_link=loc-556-ginger.pml
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
  run fun3(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  int y15 = x2;
  int y16 = -(2);
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  run wg_monitor(w14);
  if
  :: true ->
    for(y10 : 0 .. (y15) - (1)) {
      for40: skip;
      w14.update!1;
      w14.update_ack?y9;
      assert y9;
      run fun4(w14, child_13);
      run receiver(child_13);
      for40_end: skip;
    };
    for40_exit: skip;
  :: true ->


  fi;
  w14.wait?0;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(Wgdef w20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  defer1: skip;
  skip;
  w20.update!-(1);
  w20.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
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