// git_link=loc-51-ginger.pml
#define  default true
#define  x1 ??
typedef Chandef {
  chan sync = [0] of {bool};
  chan enq = [0] of {bool};
  chan deq = [0] of {bool,bool};
  chan sending = [0] of {bool};
  chan rcving = [0] of {bool};
  chan closing = [0] of {bool};
  int size = 0;
  int num_msgs = 0;
  bool closed = false;
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
  chan c13 = [0] of {int};
  if
  :: true ->
    for(y9 : 0 .. (1024) - (1)) {
      for20: skip;
      for(y9 : 0 .. (4) - (1)) {
        for21: skip;
        run fun3(c13, y5, child_12);
        run receiver(child_12);
        for21_end: skip;
      };
      for21_exit: skip;
      if
      :: ((0) != (-(2))) && (((4) - (1)) != (-(3))) ->
        for(y9 : 0 .. (4) - (1)) {
          for28: skip;
          if
          :: ((y5) - (1)) != (-(3)) ->
            for(y9 : 0 .. (y5) - (1)) {
              for29: skip;
              c13?0;
              for29_end: skip;
            };
            for29_exit: skip;
          :: else  ->
            do
            :: true ->
              for210: skip;
              c13?0;
              for210_end: skip;
            :: true ->
              break;

            od;
            for210_exit: skip;
          fi;
          for28_end: skip;
        };
        for28_exit: skip;
      :: else  ->
        do
        :: true ->
          for25: skip;
          if
          :: ((y5) - (1)) != (-(3)) ->
            for(y9 : 0 .. (y5) - (1)) {
              for26: skip;
              c13?0;
              for26_end: skip;
            };
            for26_exit: skip;
          :: else  ->
            do
            :: true ->
              for27: skip;
              c13?0;
              for27_end: skip;
            :: true ->
              break;

            od;
            for27_exit: skip;
          fi;
          for25_end: skip;
        :: true ->
          break;

        od;
        for25_exit: skip;
      fi;
      for20_end: skip;
    };
    for20_exit: skip;

  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c17; int y18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  if
  :: ((y18) - (1)) != (-(3)) ->
    for(y22 : 0 .. (y18) - (1)) {
      for22: skip;
      c17!0;
      for22_end: skip;
    };
    for22_exit: skip;
  :: else  ->
    do
    :: true ->
      for23: skip;
      c17!0;
      for23_end: skip;
    :: true ->
      break;

    od;
    for23_exit: skip;
  fi;
  stop_process: skip;
  child_19!0;
}
proctype receiver(chan c) {
  c?0;
}