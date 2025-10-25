# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`debug_indent` is a lightweight trace debugger for Scryer Prolog that uses term expansion to automatically instrument code with indented trace output. The trace output is formatted as proper Prolog terms for parsing and analysis.

## Testing

Run tests using ediprolog (recommended) or from the Scryer Prolog REPL:

```bash
# From Scryer Prolog REPL
scryer-prolog tests/test_debug.pl
```

For interactive testing with ediprolog in Emacs:
- Open a test file (e.g., `tests/test_debug.pl`)
- Place cursor on a query (e.g., `?- test_factorial.`)
- Run `M-x ediprolog-dwim` to execute the query inline

## Architecture

### Term Expansion System

The core mechanism uses Prolog's `term_expansion/2` hook in the `user` module to transform code at load time:

1. **Detection**: `user:contains_debug_op/1` (lines 24-41) recursively scans clause bodies for the `$$` operator, handling compound terms (`,`, `;`, `->`, `\+`)

2. **Transformation**: `user:expand_body/4` (lines 43-63) transforms each `$$ Goal` into `'$$-'(Goal, Module, LineNo)` while preserving the module and line number context from `prolog_load_context/2`

3. **Execution**: `'$$-'/3` (lines 106-125) executes the transformed goal with tracing:
   - Outputs `trace(id:N, call, Module:LineNo, Goal)` before execution
   - Outputs `trace(id:N, exit, Module:LineNo, Goal)` on success
   - Outputs `trace(id:N, exception, Module:LineNo, Exception, Goal)` on exception
   - Maintains indentation depth (2 spaces per level) via `$trace_depth/1`
   - Assigns unique IDs via `$trace_counter/1` to match call/exit pairs

### Trace Output Format

All trace output is structured as proper Prolog terms using `portray_clause/1`:

```prolog
trace(id:N, EventType, Module:LineNo, Goal)
```

Where:
- `id:N` - Unique trace identifier for matching call/exit pairs
- `EventType` - One of: `call`, `exit`, `exception`
- `Module:LineNo` - Source location as `module:line_number`
- `Goal` - The goal being traced
- For exceptions: `trace(id:N, exception, Module:LineNo, Exception, Goal)`

### State Management

The module maintains two dynamic predicates:
- `$trace_depth/1` - Current indentation depth (incremented on call, decremented on exit/exception)
- `$trace_counter/1` - Monotonically increasing trace ID counter

Call `reset_trace_depth/0` between queries when using the REPL to reset both counters. This is automatic when using ediprolog.

### Operator and Predicate Exports

The module exports:
- `op(900, fx, $$)` - The trace operator declaration
- `($$)/1` - Stub predicate (line 22) to satisfy the compiler; never called due to term expansion
- `'$$-'/3` - The actual runtime tracing predicate
- `reset_trace_depth/0` - Counter reset utility

The `$$` operator has prefix fixity (fx) with precedence 900, allowing it to prefix any goal.

## Key Implementation Details

- **Zero overhead for non-traced code**: Only clauses containing `$$` undergo term expansion
- **Works in user module**: Term expansion hooks are defined in `user` module to transform code across all modules that use `debug_indent`
- **No need for explicit module qualification**: `prolog_load_context/2` captures module and line number automatically during load time
- **Exception safety**: Uses `catch/3` to ensure depth counter is properly restored even when goals throw exceptions
