# Automated Verification of Parametric Channel-Based Process Communication (Paper 731) OOPSLA 2024 Artifact

## Introduction

Ginger is a tool aimed at reasoning about Go program fragments that involve concurrency and *concurrency parameters*. A Go program fragment is a snippet of code that locally declares some concurrency primitives (channels, wait groups, locks) and threads (*goroutines*) to concurrently perform a task. A fragment is defined as a set of functions arranged in a directed acyclic graph (DAG). The root of the DAG is the function that declares the concurrency primitives. Nodes and leaves are any callees that operate on the concurrency primitives or (transitively) propagate their values. Concurrency parameters are incoming values (integers or boolean) that influence the concurrent behaviour of the program e.g., channel capacities, bounds of loops performing concurrent operations (channel send/receive), etc.

The goal of Ginger is to precisely reason about Go program fragments, such that it may demonstrate the absence of *partial deadlocks*. Partial deadlocks occur whenever an individual process is blocked forever by attempting a concurrent operation e.g., attempting to receive a message from a channel to which no other thread will ever send. To achieve this, Ginger takes as input a Go program fragment, and translates it to Dafny. It then attempts to check for partial deadlock freedom by verifying that the translated fragment satisfies a postcondition stating that all running processes have terminated. In many cases (Section 7 - page 20), the program fragment is only partial deadlock free for a subset of valuations of its concurrency parameters. If so, Ginger employs heuristics to derive a proposition over concurrency parameters to be used as a *precondition* for the Dafny program. If verification succeds, it reports the precondition as sufficient (and, if provable, necessary) for guaranteeing partial deadlock freedom.

The Promela models partially support our claims about the proliferation of parametric concurrent program fragments, a subset of which may be modeled in VirGo.

The evaluation data set used in the paper is under review for release by Uber Technologies, Inc. For this reason, the following claims are NOT supported:
* The number of parametric fragments discovered in the original codebase.
* The number of parametric fragments translatable to VirGo.
* The number of parametric fragments that may be successfully verified, with breakdowns for each strategy.
* The number of concurrency parameters in the program fragments, with additional breakdowns about of how they are used in fragments e.g., as loop bounds or channel capacities.

Instead, we offer a smaller alternative benchmark with examples derived from common Go coding patterns. The artifact supports smaller-scale reproducible experiments that illustrate the capabilities of the tool, and support the following claims:
* An evaluation of Ginger over the motivating example and other examples presented in the paper.
* Aggregate metrics over the verification time for individual fragments and the number of verifications carried out for each strategy. The values are comparable with those in the paper.
* Examples of how different precondition strategies are used in the system.
* Comparisons with other open source tools (Gomela, Goat, GCatch) w.r.t. examples illustrated in the paper.

## Requirements

### Hardware

Recommended specifications: 8 cores, 32 GB RAM, 20 GB disk space, x86-64 architectures are recommended. Apple silicon machines use the ARM architecture and may run the Docker benchmark with some caveats (described below).

### Software

Recommended OS is Linux. Docker must be installed, and the Docker daemon in a running state. Recommended Docker version is 26.1.4.

##### MacOS on Apple silicon machines

Running the benchmark on Apple silicon (M1/M2/M3) machines with MacOS, has additional prerequisites because of the underlying ARM architecture. Some dependencies are only available for x86-64, so the following steps are recommended before attempting to build and run the Docker image:

1. Install Colima:

```shell
brew install colima
```

2. Create a VM with the following specifications (numeric values may be adjusted):

```shell
colima start --cpu 4 --memory 16 --disk 50 --arch amd64
```

Build scripts automatically perform a best effort attempt at setting this up if run with `-apple-silicon` as the first argument, e.g.:
```
./run-quick-start.sh -apple-silicon
```

**NOTE:** The overhead introduced when running the container through Colima is 10-15x. Assume this overhead for all following time estimations.

## Getting Started

### Building Docker image

Estimated time: 20 minutes

In this section, we quickly set up Ginger, run it on example fragments, and inspect the results.

The Docker container installs `vi` so that files may be manually inspected and edited.

1) The following command will start the Docker container that sets up Ginger and all of its dependencies (optionally with the `-apple-silicon` flag -- see above):
```shell
./run-quick-start.sh
```

   The script above will start the Docker container, run Ginger on the example inputs, and start a session within the container.

2) You can gather results of running on the fragment example with:
```shell
# Rooted at /usr/app
collect-metrics.sh "./fragments-examples"
```

3) The output should display the following:
```shell
Parametric fragments discovered (Table 1 - Column 2)
Channel :: found 4 matches
WaitGroup: :: found 3 matches
Both :: found 7 matches
Total :: found 14 matches

Expressible fragments (Table 1 - Column 3)
Channel :: found 2 matches
WaitGroup :: found 2 matches
Both :: found 5 matches
Total :: found 9 matches

Efficacy of verification back-end (Table 1 - Column 4)
Channel :: found 2 matches
WaitGroup :: found 2 matches
Both :: found 5 matches
Total :: found 9 matches

Efficacy of Strat. 1 (Table 1 - Column 5)
Channel :: found 0 matches
WaitGroup :: found 0 matches
Both :: found 2 matches
Total :: found 2 matches

Efficacy of Strat. 2 (Table 1 - Column 6)
Channel :: found 2 matches
WaitGroup :: found 2 matches
Both :: found 0 matches
Total :: found 4 matches

Efficacy of Strat. 3 (Table 1 - Column 7)
Channel :: found 0 matches
WaitGroup :: found 0 matches
Both :: found 1 matches
Total :: found 1 matches

Verification errors :: found 0 matches

Verification time (Table 2)
Strat. 1 :: Avg: 1801.71ms; P50: 1397ms; P90: 3346ms; Max: 3346ms
Performed: 7
Strat. 2 :: Avg: 2605ms; P50: 2540ms; P90: 3759ms; Max: 3759ms
Performed: 5
Strat. 3 :: Avg: 3058ms; P50: 3058ms; P90: 3058ms; Max: 3058ms
Performed: 1

Complexity metrics
FV occurrences  ::  Min: 2; Avg: 3.11; P50: 3; P90: 4; Max: 4
Unique FVs  ::  Min: 1; Avg: 1.22; P50: 1; P90: 2; Max: 2
```

4) *[Optional]* Individual result files are saved alongside fragment files in an identically named file suffixed with `-results.res`.  The contents may be dumped to `stdout` with `cat` or inspected with `vi`:
```shell
# Running from /usr/app
cat ./fragments-examples/dir12/dir13/loc-6-ginger.pml-results.res
# OR
vi ./fragments-examples/dir12/dir13/loc-6-ginger.pml-results.res
```
5) Terminate the Docker container session with the `exit` command.

###### Ginger results

The contents of a result file obtained by running Ginger on a Promela model that has been encoded to Dafny and verified successfully, are:
```shell
Profiling Go program parametricity:
Program is [...] parametric

VIRGo translation:
[...]

Attempting Strat. 1
...
Failed verification with Strat. 1
...

Attempting Strat. 2
...
Verification successful!
Constraints from capacities:
	0 <= x1'1
Communication constraints:
	iter(0, x1'1) <= iter(0, x1'1) + x1'1
...
```

The output is to be interpreted as follows:
1) `Profiling Go program parametricity:` performs a coarse approximation of program parametricity by inspecting the context in which concurrency parameters are used in the reconstructed Go program.
1) `VirGo translation` is the translation of the reconstructed Go program to VirGo.
1) `Attempting Strat.` are attempts at verification performed in the back-end and correspond to the different Strategies described in Section 4.4 (pages 12-13).
1) `Verification successful!` is reported for the first strategy (Section 4.4 - page 12) that successfully verifies the encoding, in order of execution. This is followed by any resulting non-trivial constraints over concurrency parameters.
  a) `Constraints from capacities:` are constraints derived from the use of concurrency parameters in channel capacity expressions.
  b) `Communication constraints:` are constraints derived from the use of concurrency operations.

## Step-by-Step instructions

## Reusability

#### Running Ginger end-to-end

Estimated time: 10 minutes within the Docker container

An end-to-end run of Ginger involves analyzing a Go package with Gomela first, and then running Ginger on the resulting models.

To run Ginger end-to-end, spin a Docker container through one of the following (in ascending order of time-to-spin): `./run-end-to-end.sh`, `./run-benchmark.sh` or `./run-complete.sh`. As an example, from within the Docker container, follow these steps to create and run Ginger on a small "Hello World" program:

1) Create a directory within the `examples/src` directory, and navigate to it:
```shell
# Running from /usr/app
mkdir -p ./examples/src/hello
cd ./examples/src/hello
```
2) Paste an example program into a new `hello-file.go` file:
```go
package hello

func HelloWorld() {
  ch := make(chan string)

  go func() {
    ch <- "Hello world!"
  }()

  println(<-ch)
}
```
```shell
cat > ./hello-file.go
# Paste contents and add new line
# Ctrl+D
```
3) Run `ginger-e2e` over the `hello` package:
```shell
ginger-e2e hello
```
4) Navigate to the results directory:
```shell
cd /usr/app/gomela-results/hello
```
5) Check that the Promela model has been created:
```shell
ls hello
# Should produce at least the following:
# hello++HelloWorld3.pml hello++HelloWorld3-ginger.pml hello++HelloWorld3-ginger.pml-results.res
```
6) If successful, navigate to the model directory:
```shell
cd hello
```
8) Inspect the Ginger results file:
```shell
cat hello++HelloWorld3-ginger.pml-results.res
```
9) If all steps succeed, the output should report that verification succeeded using Strategy 1.

The tool `ginger-e2e` runs Gomela over packages located at `/usr/app/examples/src`, writes the results at an appropriately named subdirectory within `/usr/app/gomela-results`, performs a run of Ginger for every resulting model in the subdirectory, and summarizes the results in a report formatted like those in [Getting Started](#getting-started).
You can recreate the steps above for your own packages, by replacing the name of the `hello` directory with your own. A Go package may then contain an arbitrary number of `.go` files. Basic familiarity with the language features of Go and its standard library is assumed. In this contained environment, any standard Go library may be freely imported.

Because Ginger leverages Gomela as a front-end, it inherits the limitations of Gomela in terms of analyzable code. For example, Gomela may not produce Promela whenever it encounters ambiguous aliases for concurrency primitives.

### Running standalone Ginger

In this section, we explain how to use Ginger as a standalone CLI tool. The Docker container is set up such that Ginger is included in the `PATH` environment:
```shell
ginger-exe args₁ args₂...
```
Try out the following example:
```shell
# Running from /usr/app:
ginger-exe ./ginger/examples/gomelas/good/simple/matching-loops.pml
```
Ginger can also be run through `stack`:
```shell
# Running in /usr/app/ginger
stack run -- args₁ args₂...
```
In either case, the first non-flag argument is the target file that Ginger should analyze. In addition, Ginger may be passed the following flags:
* `-dafny <path>` (default value: `dafny`): Specifies the path to the Dafny executable used by the verification back-end. The Docker container automatically exposes the `dafny` executable through `PATH`.
* `-virgo`: Instructs the Ginger front-end to use the VirGo parser with input files.
* `-skip-verification`: Skips running the verification back-end.
* `-color`: Colorizes output.

#### Running on Promela models

Running Ginger on a Promela model can be achieved by simply directing `ginger-exe` towards the file path of a Promela model:
```shell
# Running from /usr/app
ginger-exe ./ginger/examples/gomelas/good/simple/matching-loops.pml
```
The output will contain the same messages as when running on a VirGo program. Additionally, it will include a message showcasing the Go program reconstruction (see [Ginger results](#ginger-results)).

#### Running on VirGo programs

To run Ginger on a VirGo program, pass the `-virgo` flag to direct the front-end to the VirGo parser. The first non-flag argument should be the path to a VirGo file. The results will be printed to `stdout`. For example, when running the following in the Docker container:
```shell
ginger-exe -virgo /usr/app/ginger/examples/good/simple/motivating-example/motivating-n-cast-a.t
```
The output should contain some of the following messages:
```shell
VirGo program parametricity: chan-loop
...

Attempting Strat. 1
...
Failed verification with Strat. 1
Verification error: [...] A postcondition might not hold on this return path.
...

Attempting Strat. 2
...
Verification successful!
Communication constraints:
  1 <= iter(0, x) && iter(0, x) <= 1
...
```

#### Writing VirGo

VirGo programs must conform to the syntax specifications presented in Fig. 2. (Section 3 - page 3). Variable names (concurrency parameters, channels, wait groups, etc.) may only contain alphanumerics, apostrophes or underscores, and cannot start with a digit or apostrophe. The key syntactical difference is with `for` statements. In hand-written VirGo, `for` loops must be decorated with a loop variable as follows:
```
for 𝑥 : 𝑒 .. 𝑒 {
  ...
}
```
Loop variables follow the same lexical rules as other variables. Check `./ginger/examples/good/simple/matching-loops.t` for an example.

All VirGo variables used as names for loop indices and concurrency primitive declarations must be pairwise distinct.
All free variables used for channel capacities, loop bounds or in the guards of conditional statements are treated as concurrency parameters and may be reused throughout the VirGo program, in which case they are treated as the same symbolic value. However, they must still be distinct from loop indices and concurrency primitive declarations.

### Implementation

In this section, we outline the structure of the implementation of Ginger, found at the `ginger` directory of the artifact. Ginger is implemented in [Haskell](https://www.haskell.org/) using [Stack](https://docs.haskellstack.org/en/stable/) as the dedicated building tool and dependency manager (on top of [Cabal](https://www.haskell.org/cabal/)).

The structure of the implementation consists of two components:
* The application entry point, located in `app/Main.hs`, which describes how the implementation is exposed to the surrounding environment.
* The application source directory, `src` which contains the bulk of the implementation.

To understand the structure of Ginger, we break down its execution phases:
1) [Front-end](#front-end): parses Promela or VIRGo programs, translates Promela programs to VIRGo.
2) [Translation](#translation): translates VIRGo program to Dafny, applies precondition heuristics.
3) [Verification](#verification): emits and Dafny encoding, and applies various verification strategies.

#### Front-end
##### Parsers
The front-end of Ginger consists of several parsers derived from grammars written in [LBNF](https://bnfc.readthedocs.io/en/latest/lbnf.html), which may then be turned into parsers implemented in Haskell by using the BNFC parser generator. The grammars are located at `src/Promela.cf` and `src/VIRGo.cf` for the Promela and VIRGo front-ends, respectively. Lexers and parsers are derived from the LBNF specification using BNFC, in the form of [Alex](https://hackage.haskell.org/package/alex) and [Happy](https://hackage.haskell.org/package/happy) specifications, which may then be parsed with Alex and Happy.

The utility script `init.sh` sets up the lexical and syntactical Haskell modules and outputs the resulting source files to `src/Promela` and `src/VIRGo` respectively.

Using the Promela front-end as an example, the module containing the lexer and parser is written to `src/Promela/ParPromela.hs`, where the parser and lexer are exported as the `pSpec` and `myLexer` functions, respectively. The function `pSpec` is named such because the nonterminal `Spec` in `src/Promela.cf` has been designated as the parse tree root.
The AST emitted by BNFC is written to `src/Promela/AbsPromela.hs`, but, for convenience, we define an even more compact and ergonomic representation in `src/Promela/Ast.hs`. We refine the AST produced by `pSpec` to the one defined in `src/Promela/Ast.hs` by using the `getAst` function defined in `src/Promela/GetAst.hs`.
The parsing process of VirGo follows the same conventions, except with `VIRGo` replacing `Promela` in the names of files, and with `pProg` as the parsing function analogous to `pSpec`.

##### Sanity checks and utilities

Both front-ends have corresponding `Utilities.hs` modules in their respective directories that contain small supporting functions.

The VIRGo front-end has sanity checks in `src/VIRGo/SanityCheck.hs` that ensure no variable names are reused between concurrency primitives, loop indices and free variables.

##### Reconstructed Go

To ease operating on parsed Promela models, they are first translated to an intermediate representation corresponding to a subset of Go. This translation process "resugars" idiosyncrasies of Promela models produced by Gomela into the original Go features they represent. The Go subset does not support named functions, however, so all function calls are expanded with their definition at corresponding call sites. Function calls are treated as `go` invocations if marked as concurrent in the Promela model. The syntax of the Go subset is located at `src/Go/Ast.hs`.

To ensure termination, a sanity check ensures that the Promela model does not contain recursion. The transformation from Promela to Go is located at `src/Pipeline/Translation/Workflow.hs` as the `promelaToGo` function.

The translation from the Go subset to VIRGo is carried out by the `goToVIRGo` function exported by `src/Pipeline/Translation/Workflow.hs`. This process involves checking that the reconstructed Go program can be translated to VIRGo, and performing syntax transformations as described in Section 6 (pages 19-20).

If both `promelaToGo` and `goToVIRGo` succeed, the result is a VIRGo encoding of the original Promela model that may be encoded to Dafny.

#### Translation

The translation from VIRGo to Dafny is located in `src/Pipeline/VIRGoTranslation/Workflow.hs` as the `virgoToBackend` function. The result is an `Encoding` value, the type of which is defined at `src/Pipeline/VIRGoTranslation/Encoding.hs`.

An encoding (Sections 4 and 5, pages 6-19) encapsulates elements that are relevant for generating CoreDafny code. The core elements of the  syntactical encoding are:
* `processes`: The decomposition of VIRGo programs into processes (Section 4.2 - pages 8-9), where process encoding in `𝛯` defines a binding from process ids in `P` to program point bindings in `𝛷`. A program point binding is a binding from program point ids in `𝑁` to CoreDafny statements. Program point decomposition is located at `src/Pipeline/VIRGoTranslation/Processes.hs`. The abstract syntax of CoreDafny is defined in `src/Backend/Ast.hs`.
* `summaries`: Syntax summaries are described in Section 4.2 - page 9, and presented in the **Occurrences sets** (Fig. 6., Fig. 13.). Syntax summaries describe the syntactical occurrences of language constructs modeled by VIRGo, and contain relevant information about how they are modeled in the CoreDafny encoding, including relevant program points, variables and expressions. All summaries are defined in respective modules at `src/Pipeline/VIRGoTranslation/Summary`.

Summaries are used to construct the Dafny invariant that models the symbolic execution of the program (Section 4.3 - page 12). All invariants are located at `src/Pipeline/VIRGoTranslation/Invariant`.

The `Encoding` value also contains relevant clauses required by the Dafny encoding, from which to construct the precondition and postcondition:
* `post` represents the postcondition expression supplied to the verification back-end. Postcondition construction logic is found at `src/Pipeline/VIRGoTranslation/Clauses/Postcondition.hs`.
* `comprojection` and `wgprojection` are bindings from concurrency primitive names to the projected number of operations. These projections are at the center of the construction of the precondition used by Strategies 2 and 3 (Section 4.4 - page 13). The source code is found at `src/Pipeline/VIRGoTranslation/Clauses/CapPrecondition.hs`, `src/Pipeline/VIRGoTranslation/Clauses/CommPrecondition.hs` and `src/Pipeline/VIRGoTranslation/Clauses/WgPrecondition.hs`.

The expression that models whether the program may make progress in one of its processes is constructed using logic found at `src/Pipeline/VIRGotranslation/Enabled.hs`.

Ancillary information providing additional context used by the encoding, such as the mapping of program points to reachability clauses (Section 5.2 - page 15), can be found at `src/Pipeline/VIRGoTranslation/Context`.

#### Verification

The verification backend takes the encoding produced in the translation phase and a uses a strategy to generate CoreDafny code. The CoreDafny program is then verified using Dafny.

Strategies are defined at `src/Pipeline/Verification/Strategy.hs` and contain the following essential elements:
* `sname` and `shortName` denote the name of the strategy, which is used for identifying strategies in the final report.
* `description` provides a long-form description of the strategy.
* `realPrecondition` constructs from the given encoding the precondition central to the strategy:
  * **Strategy 1**: the precondition is `true` (assume partial deadlock freedom for all inputs)
  * **Strategies 2 and 3**: the precondition is constructed from the `comprojection` and `wgprojection` properties of the encoding, according to the strategy heuristics defined by the formula *balanced* (Section 4.4 - pages 12, 13).
* `makePrecondition` constructs from the given encoding the precondition as it will be used in the generated code:
  * **Strategy 1**: the precondition is `true`.
  * **Strategy 2**: the precondition is `true`.
  * **Strategy 3**: the precondition is the *balanced* formula, as a `requires` clause for the generated CoreDafny program.
* `makePostcondition` constructs from the given encoding the postcondition as it will be used in the generated code:
  * **Strategy 1**: the postcondition is `post encoding`.
  * **Strategy 2**: the postcondition is `balanced(V) <==> post encoding` for VIRGo program *V*, such that the verification may prove that the precondition `balanced(V)` is both *sufficient* and *necessary* i.e., program *V* is free of partial deadlocks if and only if `balanced(V)`.
  * **Strategy 3**: the postcondition is `post encoding`.

Using the encoding and strategy, the abstract syntax of a Dafny program is generated according to the logic at `src/Pipeline/Verification/Dafny.hs`.

The verification pipeline is found at `src/Pipeline/Verification/Runner.hs`. The `verify` function acts as the verification driver. It takes a VIRGo program as input and iterates over the list of strategies in order from Strategy 1 until Strategy 3, constructing the corresponding CoreDafny program and supplying to Dafny. Verification stops if a strategy succeeds at producing a verifiable model, as determined by output of Dafny the encoding, or once all strategies fail, in which case a verification error is reported.

## Appendix

This is a brief comparison with state of the art tools on selected examples.

### Comparison with the State-of-the-Art

Estimated time: 3 hours

For additional support of our claims about other analysis tools, alongside Gomela, we also bundle versions of GCatch and Goat. Spin the following container, which includes the experiment performed in [Getting Started](./README.md#getting-started), as well as applications of Gomela, GCatch and Goat to examples presented in the paper (Fig. 1., Fig. 15.):
```shell
./run-complete.sh
```
The file `./motivating/example.go` contains the following functions: `GetResultsA` (Fig. 1., left), `GetResultsB` (Fig. 1., middle), `GetResultsC` (Fig. 1., right), `DivisionExample` (Fig. 15 (a)) and `UnverifiableExample` (Fig. 15 (b)). Directory `motivating` also includes `example_test.go` to allow Goat and GCatch to analyze the examples.

#### GCatch

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

#### Goat

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

#### Gomela

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

#### Ginger

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
