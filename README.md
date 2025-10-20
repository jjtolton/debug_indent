# debug_indent

A lightweight, indented trace debugger for Scryer Prolog that automatically adds module and line number information to your debug output.

## Features

- **Indented trace output** - Visualize call depth with automatic indentation
- **Module and line tracking** - See exactly where each call originated
- **Term expansion magic** - Automatically transforms `$$` operators at compile time
- **Zero runtime overhead** - Only processes predicates that use the debug operator
- **Simple API** - Just prefix your goals with `$$`

## Installation

### Using bakage

[bakage](https://github.com/bakaq/bakage) is an experimental package manager for Scryer Prolog. To use `debug_indent` with bakage:

1. Download `bakage.pl` from the [releases](https://github.com/bakaq/bakage/releases) into your project
2. Add this to your `scryer-manifest.pl`:

```prolog
dependencies([
    dependency("debug_indent", git("https://github.com/jjtolton/debug_indent.git"))
]).
```

3. Install dependencies:

```bash
./bakage.pl install
```

4. Use in your code:

```prolog
:- use_module(bakage).
:- use_module(pkg(debug_indent)).
```

### Manual Installation

Clone the repository and load the module:

```prolog
:- use_module('path/to/debug_indent').
```

## Usage

### Basic Example

```prolog
:- module(example, [factorial/2]).
:- use_module(debug_indent).

factorial(0, 1) :- !.
factorial(N, F) :-
    $$ N > 0,
    $$ N1 is N - 1,
    $$ factorial(N1, F1),
    $$ F is N * F1.
```

From the REPL:

```prolog
?- factorial(3, F).
```

**Note**: `reset_trace_depth/0` is only needed when working from the terminal/REPL to reset counters between queries. When using [ediprolog](https://www.metalevel.at/ediprolog/) (recommended for interactive Prolog development in Emacs), the trace depth resets automatically for each query.

### Output

All trace output is formatted as proper Prolog terms for easy parsing and processing:

```prolog
trace(id:0, call, example:6, 3>0).
trace(id:0, exit, example:6, 3>0).
  trace(id:1, call, example:7, 2 is 3-1).
  trace(id:1, exit, example:7, 2 is 3-1).
    trace(id:2, call, example:8, factorial(2, _A)).
      trace(id:3, call, example:6, 2>0).
      trace(id:3, exit, example:6, 2>0).
      ...
```

Each trace term contains:
- `id:N` - Unique identifier for matching call/exit pairs
- Event type - `call`, `exit`, or `exception`
- `Module:LineNo` - Source location
- The goal being traced
- For exceptions: `trace(id:N, exception, Module:LineNo, Exception, Goal)`

### Recommended: Using with ediprolog

[ediprolog](https://www.metalevel.at/ediprolog/) provides seamless Prolog interaction within Emacs buffers. Download it here:
- [ediprolog.el](https://www.metalevel.at/ediprolog/ediprolog.el)
- [Documentation](https://www.metalevel.at/ediprolog/)

With ediprolog, you can evaluate queries directly in your source file and see the trace output inline.

## API

### Operators

- `$$ Goal` - Trace the execution of `Goal` with indentation and location info

### Predicates

- `reset_trace_depth/0` - Reset the trace depth counter and call counter to 0

### Enhanced Predicates (Internal)

- `'$$-'(Goal, Module, LineNo)` - Internal predicate created by term expansion

## How It Works

`debug_indent` uses Prolog's term expansion facility to automatically transform your code at load time:

1. When loading a file, `debug_indent` scans each clause for `$$` operators
2. If found, it extracts the module name and line number using `prolog_load_context/2`
3. Each `$$ Goal` is transformed to `'$$-'(Goal, Module, LineNo)`
4. At runtime, `'$$-'/3` prints indented trace messages with location information

This means zero runtime overhead for predicates that don't use `$$`, and minimal overhead for those that do.

## Examples

See `test_debug.pl` for more examples including:
- Recursive factorial with full tracing
- List membership checking
- Exception handling

## Requirements

- Scryer Prolog

## License

MIT License - see LICENSE file for details

## Contributing

Contributions welcome! Please feel free to submit issues and pull requests.
