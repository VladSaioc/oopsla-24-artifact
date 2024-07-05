# Appendix

For the artifact overview, check [README.md](./README.md#implementation).

## Comparison with the State-of-the-Art

Estimated time: 3 hours

For additional support of our claims about other analysis tools, alongside Gomela, we also bundle versions of GCatch and Goat. Spin the following container, which includes the experiment performed in [Benchmark experiments](./README.md#benchmark-experiments), as well as applications of Gomela, GCatch and Goat to examples presented in the paper (Fig. 1., Fig. 15.):
```shell
./run-complete.sh
```
The file `./motivating/example.go` contains the following functions: `GetResultsA` (Fig. 1., left), `GetResultsB` (Fig. 1., middle), `GetResultsC` (Fig. 1., right), `DivisionExample` (Fig. 15 (a)) and `UnverifiableExample` (Fig. 15 (b)). Directory `motivating` also includes `example_test.go` to allow Goat and GCatch to analyze the examples.

### GCatch

The results of running GCatch are stored at `/usr/app/gcatch-results.txt`. The report contains the following blocking errors for `GetResultsA`:
```
BMOC/Channel Safety :: Fragment analysis took 36.555707ms
Checking path combination [0/0]
----------Bug[2]----------
        Type: BMOC/Channel Safety       Reason: One or multiple channel operation is blocked/panic.
-----Blocking/Panic at:
        File: /usr/app/examples/src/motivating/example.go:12
-----Blocking/Panic Path NO. 0
ChanMake :/usr/app/examples/src/motivating/example.go:6:11       '✓'
Jump :/usr/app/examples/src/motivating/example.go:7:6    '✓'
If :/usr/app/examples/src/motivating/example.go:7:16     '✓'
Chan_op :/usr/app/examples/src/motivating/example.go:12:2        Blocking/Panic
Return :/usr/app/examples/src/motivating/example.go:12:2         '✗'
-----Path NO. 1         Entry func at: motivating.GetResultsA$1
```
For this report, GCatch explores the execution path where no iterations are performed by the loop at `./motivating/example.go:7`, which implies that no send operations are performed. As a result, GCatch correctly reports a blocking error for the receive operation at `./motivating/example.go:12`. However, it does not accurately capture the conditions under which the program fragment is free of partial deadlocks, and fails to detect blocking errors when the loop performs more than one iteration.

GCatch also reports a blocking error for `DivisionExample` for the send operations at `./motivating/example.go:60` and `./motivating/example.go:68`:
```
Checking path combination [0/0]
----------Bug[1]----------
        Type: BMOC/Channel Safety       Reason: One or multiple channel operation is blocked/panic.
-----Blocking/Panic at:
        File: /usr/app/examples/src/motivating/example.go:60
        File: /usr/app/examples/src/motivating/example.go:68
-----Path NO. 0         Entry func at: motivating.DivisionExample
ChanMake :/usr/app/examples/src/motivating/example.go:58:14      '✓'
Go :/usr/app/examples/src/motivating/example.go:59:2     '✓'
Jump :/usr/app/examples/src/motivating/example.go:62:6   '✓'
If :/usr/app/examples/src/motivating/example.go:62:16    '✓'
Go :/usr/app/examples/src/motivating/example.go:67:2     '✓'
Jump :/usr/app/examples/src/motivating/example.go:70:6   '✓'
If :/usr/app/examples/src/motivating/example.go:70:26    '✓'
Jump :/usr/app/examples/src/motivating/example.go:75:6   '✓'
If :/usr/app/examples/src/motivating/example.go:75:16    '✓'
Return :/usr/app/examples/src/motivating/example.go:75:29        '✓'
-----Blocking/Panic Path NO. 1
Chan_op :/usr/app/examples/src/motivating/example.go:60:8        Blocking/Panic
Return :/usr/app/examples/src/motivating/example.go:60:8         '✗'
-----Path NO. 2         Entry func at: motivating.DivisionExample$2
-----Blocking/Panic Path NO. 3
Chan_op :/usr/app/examples/src/motivating/example.go:68:8        Blocking/Panic
Return :/usr/app/examples/src/motivating/example.go:68:8         '✗'
-----Path NO. 4         Entry func at: motivating.DivisionExample$4
```
It similarly explores the execution paths where no loop iterations are performed for the loops at `./motivating/example.go:64`, `./motivating/example.go:72` and `./motivating/example.go:76`. By failing to model the relationship between the loop bounds it does not correctly identify the circumstances under which this program fragment will exhibit partial deadlocks.

Note that GCatch does not produce any reports for `GetResultsB`, `GetResultsC` and `UnverifiableExample` because they feature channels with parametric capacities, which GCatch does not support.

### Goat

The results of running Goat are stored at `/usr/app/goat-results.txt`. Goat only contains a report for `DivisionExample`:
```
{ t1 = make chan struct{} 0:int at ../examples/src/motivating/example.go:58:14 (motivating.DivisionExample) }

Potential blocked goroutine at superlocation: ⟨  [ DivisionExample:entry ] ⇒ [ ⊥ ]
  | [ DivisionExample:entry ] ↝ [ go t2() ] ⇒ [ send t0 <- struct{}{}:struct{} ]
  | [ DivisionExample:entry ] ↝ [ go t5() ] ⇒ [ send t0 <- struct{}{}:struct{} ]
  | [ DivisionExample:entry ] ↝ [ go t7() ] ⇒ [ send t0 <- struct{}{}:struct{} ]
  | [ DivisionExample:entry ] ↝ [ go t10() ] ⇒ [ ⊥ ]
⟩
Goroutine: [ DivisionExample:entry ] ↝ [ go t2() ]
Control location: [ send t0 <- struct{}{}:struct{} ]
Source: ../examples/src/motivating/example.go:60:8
Goroutine: [ DivisionExample:entry ] ↝ [ go t5() ]
Control location: [ send t0 <- struct{}{}:struct{} ]
Source: ../examples/src/motivating/example.go:64:9
Goroutine: [ DivisionExample:entry ] ↝ [ go t7() ]
Control location: [ send t0 <- struct{}{}:struct{} ]
Source: ../examples/src/motivating/example.go:68:8

Potential blocked goroutine at superlocation: ⟨  [ DivisionExample:entry ] ⇒ [ ⊥ ]
  | [ DivisionExample:entry ] ↝ [ go t2() ] ⇒ [ ⊥ ]
  | [ DivisionExample:entry ] ↝ [ go t5() ] ⇒ [ send t0 <- struct{}{}:struct{} ]
  | [ DivisionExample:entry ] ↝ [ go t7() ] ⇒ [ send t0 <- struct{}{}:struct{} ]
  | [ DivisionExample:entry ] ↝ [ go t10() ] ⇒ [ send t0 <- struct{}{}:struct{} ]
⟩
Goroutine: [ DivisionExample:entry ] ↝ [ go t10() ]
Control location: [ send t0 <- struct{}{}:struct{} ]
Source: ../examples/src/motivating/example.go:72:9
```
Goat employs a heuristic where it fixes the number of loop iterations to one to help it reason about programs with concurrency operations in correlated loops. However, in this case, the total number of concurrency operations performed in loops is correlated arithmetically through loop bounds, not by matching the number of syntactical loops in the program. As a result, Goat considers that `DivisionExample` only performs one receive operation at `./motivating/example.go:76`, causing it to report partial deadlocks for all send operations at `./motivating/example.go:60`, `./motivating/example.go:64`, `./motivating/example.go:68`, `./motivating/example.go:72`.

### Gomela

The results of Gomela are stored at `/usr/app/gomela-results/motivating/results.txt`. Promela constructs a Promela model schema where the values of concurrency parameters open (marked as `??`). It then constructs a Promela model for each concrete valuation of its concurrency parameters, by instantiating all concurrency parameter combination with values from a predefined set (default: {0, 1, 3}).

For `GetResultsA`, it reports the following:
```
Result for motivating:motivating++GetResultsA5.pml
def_var_x  =  0
...
Model deadlock : true.
...
Blocked operations :
1: 	 Recv	/usr/app/examples/src/motivating/example.go:12:4
-------------------------------
Result for motivating:motivating++GetResultsA5.pml
def_var_x  =  1
...
Model deadlock : false.
...
-------------------------------
Result for motivating:motivating++GetResultsA5.pml
def_var_x  =  3
...
Model deadlock : true.
...
Blocked operations :
1: 	 Send	/usr/app/examples/src/motivating/example.go:9:4
2: 	 Send	/usr/app/examples/src/motivating/example.go:9:4
```
It correctly identifies the potential deadlocks caused by the send operations at `./motivating/example.go:9` and receive operation at `./motivating/example.go:12` whenever `x ≠ 1`.

For `GetResultsB`, it reports the following:
```
Result for motivating:motivating++GetResultsB15.pml
def_var_x  =  0
...
Model deadlock : true.
...
1: 	 Recv	/usr/app/examples/src/motivating/example.go:22:4
-------------------------------
Result for motivating:motivating++GetResultsB15.pml
def_var_x  =  1
...
Model deadlock : false.
...
-------------------------------
Result for motivating:motivating++GetResultsB15.pml
def_var_x  =  3
...
Model deadlock : false.
...
```
It similarly correctly identifies the potential deadlock caused by the receive operation `./motivating/example.go:12` whenever `x = 0`.

For `GetResultsC`, which is partial deadlock free for all inputs, it does not report a deadlock. The analysis technique used by Gomela is more precise. However, the discovery of deadlocks for `GetResultsA` and `GetResultsB`,  is instead owed its choice of values with which the produced models are tested. Since Gomela does not reason symbolically about concurrency parameters, it cannot precisely describe the conditions under which partial deadlocks may occur.

This can be observed for `DivisionExample`, where Gomela fails to detect potential partial deadlocks that may occur whenever `readers ≤ -2`.

On the other hand, the testing-based approach of Gomela has certain advantages. For example, even though Gomela fails to precisely identify that `numLevels = complianceLevels` is a requirement for `UnverifiableExample`, it nonetheless reports potential deadlocks whenever `numLevels ≠ 0` and `numLevels ≠ complianceLevels`:
```
Result for motivating:motivating++UnverifiableExample38.pml
def_var_numLevels  =  1
def_var_complianceLevels  =  3
...
Model deadlock : true.
...
Blocked operations :
1: 	 Send	/usr/app/examples/src/motivating/example.go:47:4
2: 	 Send	/usr/app/examples/src/motivating/example.go:47:4
3: 	 Wait()	/usr/app/examples/src/motivating/example.go:51:2
-------------------------------
Result for motivating:motivating++UnverifiableExample38.pml
def_var_numLevels  =  3
def_var_complianceLevels  =  0
...
Model deadlock : true.
...
Blocked operations :
1: 	 Recv	/usr/app/examples/src/motivating/example.go:53:5
-------------------------------
Result for motivating:motivating++UnverifiableExample38.pml
def_var_numLevels  =  3
def_var_complianceLevels  =  1
...
Model deadlock : true.
...
Blocked operations :
1: 	 Recv	/usr/app/examples/src/motivating/example.go:53:5
-------------------------------
Result for motivating:motivating++UnverifiableExample38.pml
def_var_numLevels  =  1
def_var_complianceLevels  =  0
...
Model deadlock : true.
...
Blocked operations :
1: 	 Recv	/usr/app/examples/src/motivating/example.go:53:5
```

### Ginger

The results of running Ginger on each model produced by Promela is stored at `/usr/app/gomela-results/motivating/motivating`. For each model, the Ginger results are as follows:
* For `GetResultsA`, Ginger succeeds by using Strategy 2, and correctly infers that the program is partial deadlock free only if `x = 1`. The construct `iter(a, b)` is to be interpreted as "`a` if `b < a` and `b - a` otherwise":
```shell
# From /usr/app/gomela-results/motivating/motivating
cat motivating++GetResultsA5-ginger.pml-results.res

# Report should contain:
# Attempting Strat. 2
# ...
# Verification successful!
# Communication constraints:
#       1 <= iter(0, def_var_x'1) && iter(0, def_var_x'1) <= 1
```
* For `GetResultsB`, Ginger succeeds by using Strategy 2, and infers that the specifications of the program fragment that ensure partial deadlock freedom and channel capacity safety are `x > 0`:
```shell
# From /usr/app/gomela-results/motivating/motivating
cat motivating++GetResultsB15-ginger.pml-results.res

# Report should contain:
# Attempting Strat. 2
# ...
# Verification successful!
# Constraints from capacities:
#         0 <= def_var_x'1
# Communication constraints:
#         1 <= iter(0, def_var_x'1) && iter(0, def_var_x'1) <= 1 + def_var_x'1
```
* For `GetResultsC`, Ginger succeeds by using Strategy 1:
```shell
# From /usr/app/gomela-results/motivating/motivating
cat motivating++GetResultsC25-ginger.pml-results.res

# Report should contain:
# Attempting Strat. 1
# ...
# Verification successful!
```
* For `DivisionExample`, Ginger succeeds by using Strategy 2:
```shell
# From /usr/app/gomela-results/motivating/motivating
cat motivating++DivisionExample57-ginger.pml-results.res

# Report should contain:
# Attempting Strat. 2
# ...
# Verification successful!
# Communication constraints:
#       iter(0, 3 + def_var_readers'1 - 1) <= iter(0, def_var_readers'1 / 2) + iter(def_var_readers'1 / 2, def_var_readers'1) + 2
#    && iter(0, def_var_readers'1 / 2) + iter(def_var_readers'1 / 2, def_var_readers'1) + 2 <= iter(0, 3 + def_var_readers'1 - 1)
```
* For `UnverifiableExample`, Ginger fails to verify, as detailed in Section 7.2:
```shell
# From /usr/app/gomela-results/motivating/motivating
cat motivating++UnverifiableExample38-ginger.pml-results.res

# Report should end with:
# [ VERIFICATION ERROR | ... ]
```
