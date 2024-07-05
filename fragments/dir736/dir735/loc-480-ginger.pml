// git_link=loc-480-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun4(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun4(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  Wgdef w13 ;
  int y14 = x3;
  int y15 = x2;
  run wg_monitor(w13);
  for(y10 : 0 .. (y6) - (1)) {
    for10: skip;
    for(y10 : 0 .. (y15) - (1)) {
      for11: skip;
      if
      :: true ->
        goto for11_end;
      :: true ->


      fi;
      if
      :: true ->
        w13.update!1;
        w13.update_ack?y9;
        assert y9;

      fi;
      for11_end: skip;
    };
    for11_exit: skip;
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  stop_process: skip;
  child_7!0;
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