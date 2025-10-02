# minic

A compiler for a subset of C, written in Haskell. This project compiles a mini C language to x86-64 assembly using a multi-stage compilation pipeline.

## Features

- **Full compilation pipeline**: Source code → AST → Typed AST → MIR → x86-64 Assembly
- **Type checking**: Static type checking with support for:
  - Basic types: `int`, `char`, `bool`, `void`
  - Pointers and arrays
  - Functions
- **Optimizations**: Copy propagation and other MIR-level optimizations
- **Register allocation**: Graph coloring-based register allocation with spilling
- **Control flow analysis**: Liveness analysis and interference graph construction

## Supported Language Features

### Types
- `int` - 8-byte integers
- `char` - 1-byte characters
- `bool` - boolean values
- `void` - void type for functions
- Pointers (`int*`, `char*`, etc.)
- Arrays (`int[5]`, etc.)

### Statements
- Variable declarations with optional storage specifiers (`auto`, `static`)
- Array declarations with initialization
- Assignments
- Expression statements
- Control flow: `if`/`else`, `while`, `for`
- `return` statements

### Expressions
- Binary operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`, `^`
- Unary operators: `-`, `!`, `*` (dereference), `&` (address-of)
- Function calls
- Array access
- Literals: integers, characters

### Functions
- Function definitions with arguments and return types
- External function declarations
- Recursive functions

## Installation

### Prerequisites

- GHC 9.12.2 or later
- Cabal 3.0 or later
- GNU Assembler (`as`) for assembling the generated code
- A linker (e.g., `mold`, `ld`, or `gcc`)

### Building

```bash
# Clone the repository
git clone <repository-url>
cd minic

# Build with cabal
cabal build

# Install the executable (optional)
cabal install
```

## Usage

The compiler provides several commands to inspect different stages of compilation:

### Basic Commands

```bash
# Parse and show the AST
minic ast <input-file>

# Type check and show typed AST
minic semant <input-file>

# Generate and show MIR (Medium-level Intermediate Representation)
minic mir <input-file>

# Generate and show x86-64 assembly
minic x86 <input-file>

# Compile to assembly file
minic x86 <input-file> -o <output-file>
```

### Advanced MIR Commands

```bash
# Show liveness analysis
minic mir --live <input-file>

# Show register allocation
minic mir --color <input-file>

# Show interference graph
minic mir --interference <input-file>

# Show optimized MIR
minic mir --opt <input-file>

# Write MIR to file
minic mir <input-file> -o <output-file>
```

## Example

Given a source file `examples/test.c`:

```c
extern void print_char(int c);
extern void print(char c);

void print_int(int n) {
    int i = 0;
    if (n < 10) {
        print_char(48+n);
    } else {
        print_int(n/10);
        i = n%10;
        print_char(48+i);
    }
}

int fact(int n) {
    int result = 1;
    int i = 2;

    for (i = 2; i <= n; i = i + 1) {
        result = result * i;
    }

    return result;
}

void main() {
    int arr[3] = {10, 0, fact(5)};
    arr[1] = 20;
    print_int(arr[0]);
    print(' ');
    print_int(arr[1]);
    print(' ');
    print_int(arr[2]);
    print('\n');
}
```

### Compile and Run

```bash
# Generate assembly
minic x86 examples/test.c -o examples/test.s

# Assemble and link
cd examples
make

# Run
./test.exe
# Output: 10 20 120
```

## Project Structure

```
minic/
├── app/
│   └── Main.hs              # CLI entry point
├── src/
│   ├── Ast/                 # Abstract Syntax Tree
│   │   ├── Parse.hs         # Parser implementation
│   │   ├── Semant.hs        # Semantic analysis and type checking
│   │   ├── Types.hs         # AST type definitions
│   │   └── Lenses.hs        # Lens definitions for AST
│   ├── Mir/                 # Medium-level IR
│   │   ├── Translate.hs     # AST to MIR translation
│   │   ├── Types.hs         # MIR type definitions
│   │   ├── Liveness.hs      # Liveness analysis
│   │   ├── Interference.hs  # Interference graph construction
│   │   ├── Allocation.hs    # Register allocation
│   │   ├── CopyPropagation.hs # Copy propagation optimization
│   │   └── Lenses.hs        # Lens definitions for MIR
│   ├── X86/                 # x86-64 backend
│   │   ├── Translate.hs     # MIR to x86 translation
│   │   └── Types.hs         # x86 instruction definitions
│   ├── SymbolTable/         # Symbol table implementation
│   │   ├── Types.hs         # Symbol table types
│   │   └── Lenses.hs        # Lens definitions
│   ├── SymbolTable.hs       # Symbol table operations
│   ├── TypeSystem.hs        # Type system definitions
│   ├── Pipeline.hs          # Pipeline abstraction
│   └── CompilerPipeline.hs  # Compiler pipeline stages
├── examples/                # Example programs
│   ├── test.c               # Example C program
│   ├── print_char.s         # External print functions
│   └── Makefile             # Build script for examples
├── minic.cabal              # Cabal package definition
└── LICENSE.md               # MIT License
```

## Compiler Pipeline

The compiler uses a staged pipeline architecture:

1. **Parsing** (`Ast.Parse`): Source code → Raw AST
   - Uses Parsec for parsing
   - Generates block IDs for scoping

2. **Semantic Analysis** (`Ast.Semant`): Raw AST → Typed AST
   - Type checking
   - Symbol table construction
   - Scope analysis

3. **MIR Translation** (`Mir.Translate`): Typed AST → MIR
   - Converts to three-address code
   - Explicit control flow graph (CFG)
   - Stack allocation for local variables

4. **Optimization** (`Mir.CopyPropagation`): MIR → Optimized MIR
   - Copy propagation
   - Dead code elimination opportunities

5. **Register Allocation** (`Mir.Allocation`): MIR → Allocated MIR
   - Liveness analysis
   - Interference graph construction
   - Graph coloring register allocation
   - Spilling for registers that don't fit

6. **Code Generation** (`X86.Translate`): MIR → x86-64 Assembly
   - Instruction selection
   - AT&T syntax assembly output
   - Function prologue/epilogue generation

## Implementation Details

### Register Allocation

The compiler uses a graph coloring algorithm for register allocation:
- 8 general-purpose registers available (R1-R8, mapped to x86 R8-R15)
- Liveness analysis determines which temporaries are live simultaneously
- Interference graph represents conflicts between temporaries
- Greedy coloring with spilling to stack when necessary

### Symbol Table

The symbol table supports:
- Nested scoping with parent environment links
- Storage location tracking (stack offsets, registers, static data)
- Symbol types: variables, arguments, functions
- Storage specifiers: auto (stack), static (data section)

### MIR (Medium-level IR)

The MIR uses a CFG-based representation:
- Basic blocks with instructions and terminators
- Explicit control flow (jumps, branches, returns)
- Three-address code format
- Temporary variables with explicit types

## Dependencies

- **base** (≥ 4.17): Standard Haskell library
- **containers** (≥ 0.6): Maps, sets for symbol tables and graphs
- **parsec** (≥ 3.1): Parser combinators
- **mtl** (≥ 2.3): Monad transformers for state management
- **lens** (≥ 5.3): Composable accessors and setters
- **recursion-schemes** (≥ 5.2): Generic recursion patterns
- **optparse-applicative** (≥ 0.19): Command-line argument parsing

## Current Limitations

- Arrays as function arguments are not yet supported
- Limited standard library (requires external print functions)
- Pointer arithmetic is limited
- No preprocessor support
- No struct/union types
- No separate compilation (single-file only)

## Future Work

- [ ] Add comprehensive test suite
- [ ] Implement array function parameters
- [ ] Add more optimizations (constant folding, dead code elimination)
- [ ] Improve error messages with source locations
- [ ] Add struct/union support
- [ ] Implement a minimal standard library
- [ ] Add preprocessor support
- [ ] Support for floating-point types
