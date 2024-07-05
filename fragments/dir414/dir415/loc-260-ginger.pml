// git_link=loc-260-ginger.pml
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
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
}
init {
  chan child_4 = [1] of {int};
  run fun2(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  Mutexdef mu12 ;
  Mutexdef mu13 ;
  Mutexdef mu14 ;
  int y15 = x1;
  run mutex_monitor(mu14);
  run mutex_monitor(mu13);
  run mutex_monitor(mu12);
  chan c16 = [1] of {int};
  chan c17 = [y15] of {int};
  run fun3(mu13, mu12, c17, c16, child_11);
  child_11?0;
  stop_process: skip;
  child_5!0;
}
proctype fun3(Mutexdef mu18; Mutexdef mu19; chan c20; chan c21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  goto stop_process;
  stop_process: skip;
  child_22!0;
}
proctype mutex_monitor(Mutexdef m) {
  bool locked = false;
  end: skip;
  do
  :: true ->
    if
    :: (m.Counter) > (0) ->
      if
      :: m.RUnlock!true ->
        m.Counter = (m.Counter) - (1);
      :: m.RLock!true ->
        m.Counter = (m.Counter) + (1);

      fi;
    :: locked ->
      m.Unlock!true;
      locked = false;
    :: else  ->
      if
      :: m.Unlock!false ->

      :: m.Lock!true ->
        locked = true;
      :: m.RUnlock!false ->

      :: m.RLock!true ->
        m.Counter = (m.Counter) + (1);

      fi;
      end1: skip;
    fi;

  od;
}