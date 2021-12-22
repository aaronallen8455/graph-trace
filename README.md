# Graph Trace

A GHC plugin that causes your Haskell program to create a log file which can be
compiled to a graph for display with `Graphviz` or some other means. Contrary
to traditional debug tracing where all output is interleaved into a flat
sequence of lines, the graph structure produced by this plugin takes into
account the actual call graph of the program with individual function calls
represented as edges in the graph.

__Contents:__
- [Demonstration](#demonstration)
- [Quickstart](#quickstart)
- [User's guide](#users-guide)
  - [`Graph.Trace` plugin](#graphtrace-plugin)
  - [`graph-trace-viz` utility](#graph-trace-viz-utility)
- [Caveats](#caveats)

## Demonstration

Consider this simplistic program that greets the user by name:
```haskell
import Graph.Trace (TraceDeep, traceM)
import Data.Char (toUpper, toLower)

main :: TraceDeep => IO ()
main = do
  firstName <- prompt "Enter your first name"
  lastName <- prompt "Enter your last name"
  greet firstName lastName

prompt :: String -> IO String
prompt str = do
  putStrLn str
  input <- getLine
  traceM $ "input: " <> input
  pure $ capitalize input

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

greet :: String -> String -> IO ()
greet firstName lastName =
  putStrLn $ "Hello, " <> firstName <> " " <> lastName <> "!"
```

Using the `Graph.Trace` plugin along with the `graph-trace-viz` utility, we can
run this program to generate the following trace of the call graph:

![demo image](/images/demo.svg)

## Quickstart

1. Add the `graph-trace` package as a dependency to your project.
2. Enable the plugin by adding the following GHC options:
   `-fplugin=Graph.Trace -fplugin-opt Graph.Trace:trace-all -fno-full-laziness -fno-cse`.
   This can be placed in the `ghc-options` field of the cabal or
   package.yaml file (depending on whether you use cabal or stack to build).
   For example:

   in `package.yaml`:
   ```
   executables:
     my-exe:
       ...
       ghc-options:
         -fplugin=Graph.Trace
         -fplugin-opt Graph.Trace:trace-all
         -fno-full-laziness
         -fno-cse
   ```
   or in `foo.cabal`:
   ```
   executable my-exe
     ...
     ghc-options:
       -fplugin=Graph.Trace
       -fplugin-opt Graph.Trace:trace-all
       -fno-full-laziness
       -fno-cse
   ```
3. Build your project (`cabal build all` or `stack build`).
4. Running your program should now generate a file called `<executable-name>.trace`.
5. Install [Graphviz](https://graphviz.org) and the `graph-trace-viz` program.
   Invoke `graph-trace-viz` within the same directory as the trace file.
6. There should now be a file such as `<executable-name>.html` which can be
   viewed in your browser.

## User's Guide

### `Graph.Trace` plugin
To use this plugin simply add `graph-trace` as a package dependency and pass
the `-fplugin=Graph.Trace` option to GHC.
The main functionality of the `Graph.Trace` plugin is to automatically
instrument your code so that it will emit trace logging to a `*.trace` file
when run. There are two types of traces:
- __Function call__  
  This trace is emitted when the term returned by a function is evaluated to
  WHNF. Each emission of an entry trace generates a unique ID for that particular
  function invocation. The trace also includes the ID of the code from which the
  function was called (if applicable) so that a graph edge can be constructed
  between the two. Entry traces are only emitted for functions that have
  signatures.  If the function does not have a signature then it will simply
  inherit the ID of the calling context and will therefore not generate a new
  node in the call graph.

- __Debug trace__  
  Debug traces are textual messages that the user can emit from the body of a
  function. When rendered, they appear as plain text in the body of a call graph
  node. The API for debug traces matches that of the familiar `Debug.Trace`
  module and is available through the `Graph.Trace` module. A debug trace will
  only be emitted when the thing it is applied to is evaluated to WHNF.

The plugin gives you some control over how and when traces are emitted.  There
are a variety of constraints you can put on function signatures to control
tracing, all of which are exported by `Graph.Trace`:

- `Trace`  
  On its own, this constraint says that traces should be emitted for a given
  function using the name of that function as the identifier. Notably, function
  calls made from within the body of the function do not inherit this behavior
  and so will not emit traces unless otherwise instructed to do so.

- `TraceDeep`  
  This is similar to `Trace` except that function calls made within the function
  body will also emit traces even if they don't have a trace constraint (unless
  they are being muted). For example, putting `TraceDeep` on the `main` function
  will result in the full execution the program being traced.

- `TraceMute`  
  If a function has this constraint then any invocation of it as well as the
  function calls it makes will not emit any traces. This constraint overrides all
  the others and is inherited by function calls made from within its body.

- `TraceKey`  
  This constraint is the same as `Trace` but it takes a type level string
  argument which will be used as the function identifier instead of the
  function's actual name. Notably, if the identifier of a called function is
  overridden to be the same as that of the function calling it then its output
  will be placed in the graph node of the calling function rather than
  generating its own node. There is also a `TraceDeepKey` constraint that does
  the same thing but for `TraceDeep`.

If you want every function in your program to emit traces, you can use the
`trace-all` plugin option which effectively adds the `Trace` constraint to all
function definitons. To use this option, pass the `-fplugin-opt Graph.Trace:trace-all`
option to GHC in addition to `-fplugin=Graph.Trace`.

If a trace file already exists for your executable being then new entries will
be appended to that file. To start a new trace you'll need to rename or delete
the old file.

### `graph-trace-viz` utility
Once you've generated a `*.trace` file by compiling your program with the
`Graph.Trace` plugin and running it, the `graph-trace-viz` utility can render
the graph as an html document. It is dependent on
[Graphviz](https://graphviz.org), so you must install that on your system
(there should be an executable called `dot` on your PATH). Simply invoke
`graph-trace-viz` in the same directory as the `*.trace` file and it will write
an `*.html` document. By default it will read all trace files in the current
directory but you can specify the files by giving them as command line
arguments instead.

## Caveats

There are several known caveats you should be aware of:

- __Undesirable optimisations__  
  If you compile with the common sub-expression or full laziness optimisations,
  which are on by default for `O1` and `O2` settings, graph nodes that should
  be distinct can sometimes get merged into a single node. To prevent this, it
  is recommended that you turn these optimisations off using the `-fno-cse` and
  `-fno-full-laziness` flags when compiling with the `graph-trace` plugin.
- __Type class methods__  
  For type class instance methods to be traced correctly, the class method
  definitions must be in a package compiled with the plugin and you'll also
  need to put a type signature on the instance method declarations, which
  requires the `InstanceSigs` GHC extension.
- __Impredicative types__  
  If you have a function binding that takes a rank-n quantified type as a
  parameter, this can cause compilation with the plugin to fail. With GHC 9.2
  and above, giving a type signature to the binding will resolve the issue.
- The plugin does not support GHC versions less than 8.10
