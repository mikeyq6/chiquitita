# Welcome to Chiquitita! 🎵

Chiquitita is a simple, readable programming language that uses indentation for structure. Named after the ABBA song, it aims to make programming as beautiful and clear as music.

## Getting Started

### Your First Program

Create a file called `hello.chiq` with this content:

```chiquitita
print "Hello World!"
```

### Running Your Program

```bash
dotnet run hello.chiq
```

## Language Syntax

### Basic Commands

- `print <text>` - Outputs text to the console (strings or text variables)
- Text strings are enclosed in double quotes: `"Hello World!"`
- `if <condition>` - Conditional execution
- `elsif <condition>` - Additional conditions
- `else` - Default case
- `while <condition>` - Loop while condition is true
- `foreach <type> <var> in <array>` - Iterate over array elements
- Conditions: `true`, `false`, or bool variables

### Variables

Chiquitita is strongly-typed. Declare variables with their type:

- `int <name> = <number>` - Integer variables
  - Decimal: `42`
  - Hexadecimal: `x4f` (prefix with 'x')
  - Binary: `b100101011` (prefix with 'b')
  - Octal: `o071` (prefix with 'o')
- `text <name> = "<string>"` - Text variables
- `float <name> = <decimal>` - Floating-point variables
- `bool <name> = true/false` - Boolean variables

**Variable Reassignment:**
- `<name> = <value>` - Update an existing variable (type must match)
- Note: Variables can only be declared once; attempting to redeclare a variable will result in an error

**Arrays:**
- `int[] <name> = [values]` - Integer array
- `text[] <name> = [values]` - Text array
- `float[] <name> = [values]` - Float array
- `bool[] <name> = [values]` - Boolean array
- Arrays must be initialized when declared
- Empty arrays: `int[] <name> = []`
- Arrays can contain literals or variables of the same type
- `<array>.length` - Returns the number of elements in an array
- `<array>[index]` - Access element at index (1-based indexing)

### Operations

**Arithmetic:**
- `+`, `-`, `*`, `/` - Arithmetic operations for integers and floats (mixed types promote to float)
- `mod` - Modulus operator (returns remainder after integer division, integers only)

**Comparison:**
- `>`, `<`, `>=`, `<=` - Comparison operators for numerical types (return bool)
- `equals` - Equality comparison for all types (returns bool)

**Logical:**
- `and`, `or`, `xor` - Logical operators for boolean types

**Text:**
- `.` - Text concatenation
- `{{variable}}` - Convert variables to strings within text
- `{{expression}}` - Evaluate expressions within text

### Methods

Define methods with return types and parameters:

- `[return type] [name]([param type] [param name], ...)` - Method definition
- Return types: `int`, `text`, `float`, `bool`, or `void`
- `chiquitita [expression]` - Return statement (required for non-void methods)
- `chiquitita` - Early return from void methods (optional)
- Method calls: `methodName(arg1, arg2)` or as statements for void methods
- Methods can be declared anywhere in the file

**Important Rules:**
- Non-void methods MUST use `chiquitita` to return a value
- Void methods CANNOT return a value
- Language keywords cannot be used as variable, method, or parameter names
- Variables can only be declared once (redeclaration is not allowed)

### Code Structure

Chiquitita uses **indentation** instead of braces or semicolons:

```chiquitita
print "Welcome to Chiquitita!"
print "This is line 2"
print "This is line 3"
```

### Comments

Use `//` for single-line comments:

```chiquitita
// This is a comment
print "Hello World!"  // This is also a comment
```

## Examples

### Simple Hello World
```chiquitita
print "Hello World!"
```

### Multiple Lines
```chiquitita
print "Welcome to Chiquitita!"
print "A beautiful programming language"
print "Made for readability"
```

### Conditional Statements
```chiquitita
if true
    print "This will always run"
else
    print "This won't run"
```

### Multiple Conditions
```chiquitita
if false
    print "First condition"
elsif true
    print "Second condition - this runs!"
else
    print "Default case"
```

### Variables
```chiquitita
text greeting = "Hello World!"
int number = 42
float pi = 3.14
bool isTrue = true

print greeting
print "The answer is not here"

if isTrue
    print "This condition is true!"
```

### Arithmetic and Text Operations
```chiquitita
int a = 2
int b = 3
int c = a + b
int d = a * b
int e = a / b
int f = 50 mod 8  // Modulus: remainder after division

// Different number formats
int hex = x1f
int bin = b1010
int oct = o77

float x = 3.14
float y = 2.86
float z = x + y

// Mixed types (result is always float)
int num = 5
float mixed = num + 3.14
float product = num * 2.5
float quotient = num / 2.0

text hello = "Hello"
text world = hello." World!"

int age = 25
text message = "I am {{age}} years old"
print message
print "Simple math: {{2+3}} = 5"
print "Multiplication: {{4*5}} = 20"
print "Division: {{10/2}} = 5"
```

### Methods with Return Values
```chiquitita
int add(int x, int y)
    chiquitita x + y

float multiply(float a, float b)
    chiquitita a * b

int result = add(5, 3)
float product = multiply(2.5, 4.0)
print "Result: {{result}}"
print "Product: {{product}}"
```

### Void Methods
```chiquitita
void greet(text name)
    print "Hello, {{name}}!"

void printSeparator()
    print "-------------------"
    chiquitita  // Optional early return

greet("World")
printSeparator()
```

### Comparison Operators
```chiquitita
int a = 5
int b = 3

bool greater = a > b
bool less = a < b
bool greaterOrEqual = a >= b
bool lessOrEqual = a <= b
bool equal = a equals b

print "5 > 3 = {{greater}}"
print "5 < 3 = {{less}}"
print "5 >= 5 = {{5 >= 5}}"
print "5 <= 5 = {{5 <= 5}}"
print "5 equals 3 = {{equal}}"
print "5 equals 5 = {{5 equals 5}}"

// Works with mixed types
float x = 4.5
bool result = a > x
print "5 > 4.5 = {{result}}"

// Equals works with all types
text hello = "hello"
text world = "world"
bool textEqual = hello equals world
print "hello equals world = {{textEqual}}"
```

### Logical Operators
```chiquitita
bool t = true
bool f = false

bool andResult = t and f
bool orResult = t or f
bool xorResult = t xor f

print "true and false = {{andResult}}"
print "true or false = {{orResult}}"
print "true xor false = {{xorResult}}"

// Use with comparisons
int value = 5
bool isWithin = value >= 1 and value <= 10
print "5 is within 1-10: {{isWithin}}"
```

### While Loops
```chiquitita
int counter = 0

while counter < 5
    print "Counter: {{counter}}"
    counter = counter + 1

print "Done! Final counter: {{counter}}"

// Loop with boolean condition
bool running = true
int count = 0

while running
    count = count + 1
    if count >= 3
        running = false
    print "Count: {{count}}"
```

### Foreach Loops
```chiquitita
int[] values = [4, 5, 6]
foreach int val in values
    print "Value: {{val}}"

int[] nums = [10, 20, 30, 40]
int sum = 0
foreach int num in nums
    sum = sum + num
print "Sum: {{sum}}"
```

### Arrays
```chiquitita
// Array declarations
int[] numbers = [1, 2, 3, 4, 5]
float[] decimals = [3.14, 2.71, 1.41]
text[] words = ["hello", "world"]
bool[] flags = [true, false, true]

// Empty arrays
int[] empty = []

// Arrays with variables
int x = 10
int y = 20
int[] coords = [x, y, 30]

text greeting = "hello"
text[] messages = [greeting, "goodbye"]

print "Arrays created!"

// Array length
int size = numbers.length
print "Array has {{size}} elements"
print "Empty array length: {{empty.length}}"

// Array indexing (1-based)
int first = numbers[1]
int second = numbers[2]
print "First element: {{first}}"
print "Second element: {{numbers[2]}}"

// Loop through array
int i = 1
int len = numbers.length
while i <= len
    print "Element {{i}}: {{numbers[i]}}"
    i = i + 1
```

## Language Features

### Type System
- **Strong typing**: All variables must be declared with a type
- **Type promotion**: Mixed int/float operations automatically promote to float
- **Supported types**: `int`, `text`, `float`, `bool`, `void` (methods only)
- **Array types**: `int[]`, `text[]`, `float[]`, `bool[]`
- **Integer formats**: Decimal, hexadecimal (x prefix), binary (b prefix), octal (o prefix)

### Method System
- **Flexible placement**: Methods can be declared anywhere in the file
- **Type safety**: Return types are enforced at compile time
- **Void methods**: Can perform actions without returning values
- **Method calls**: Can be used in expressions or as standalone statements

### Reserved Keywords
The following keywords cannot be used as identifiers:
`print`, `if`, `elsif`, `else`, `while`, `foreach`, `in`, `true`, `false`, `int`, `text`, `float`, `bool`, `void`, `chiquitita`, `and`, `or`, `xor`, `mod`, `equals`

## What's Next?

Chiquitita is actively evolving! Current features include:
- ✅ Variables and type system
- ✅ Variable reassignment
- ✅ Arrays (int[], text[], float[], bool[])
- ✅ Arithmetic operations (+, -, *, /)
- ✅ Comparison operators (>, <, >=, <=)
- ✅ Logical operators (and, or, xor)
- ✅ String interpolation
- ✅ Methods with parameters and return values
- ✅ Void methods
- ✅ Conditional statements (if/elsif/else)
- ✅ While loops
- ✅ Foreach loops
- ✅ Comments

### Known Limitations
- Chained logical operations (e.g., `a and b or c`) are not currently supported
- Use intermediate variables or separate statements for complex boolean expressions

Happy coding! 🎶