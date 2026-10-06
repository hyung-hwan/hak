# HAK - Command Language

![HAK](hak.png)

## Language Syntax

A HAK program is composed of expressions. `( )` is a call, as in lisp, and that
holds everywhere except in the few declaration positions listed under
[Calls and message sends](#calls-and-message-sends).

## Reserved words

Values:

- `nil`
- `true`
- `false`
- `self`
- `super`

Special form expressions:

- `and`
- `break`
- `catch`
- `class`
- `continue`
- `do`
- `elif`
- `else`
- `fun`
- `if`
- `or`
- `return`
- `revert`
- `set`
- `set-r`
- `throw`
- `try`
- `until`
- `var`
- `while`

### do

```
do;
do 10;
do { | k | set k 20; printf "k=%d\n" k; };
```

## Literals

- integer `123`, `-45`
- radixed integer `16r1F`, `2r1011`
- floating point `1.25`
- character `'c'`, `#\a`
- named character `#\space`, `#\tab`, `#\newline`, `#\linefeed`, `#\return`,
  `#\backspace`, `#\page`, `#\rubout`, `#\vtab`, `#\nul`
- byte character `b'c'`
- string `"string"`
- byte string `b"string"`
- symbol `#"symbol"`, `#symbol`
- small pointer `0p1234`
- error `0e1`

## Basic Expressions

- function call `(f arg1 ...)` or `f(arg1 ...)`
- message send `(rcv:msg arg1 ...)` or `rcv:msg(arg1 ...)`
- dictionary `#{ "a": 1, "b": 2 }` - the pairs are comma-separated
- array `#[ ]`
- byte array `#b[ ]`
- character array `#c[ ]` - the elements are characters, as in `#c['a' 'b']`
- list `#( )`
- attribute list `[ ]`, as in `class[#b]` and `fun[#ci]`
- destructuring assignment `[a b c] := (f 5)`, which takes the return variables
  of a call - see [Variadic arguments](#variadic-arguments)
- variable declaration `| |` at the start of a block, or `var a b c`
- assignment `varname := value` or `set varname value`
- return variables `::` in a parameter list, collected with `set-r`

The elements of `#[ ]`, `#b[ ]`, `#c[ ]` and `#( )` are compiled, so an
expression may stand in any of them:

```
printf "%O\n" #[(+ 1 2) 4]        ## #[3 4]
```

## Calls and message sends

`( )` is a call everywhere but the declaration positions - the parameter list
of `fun x(a b)`, the attribute list of `class[#b]` and `fun[#ci]`, and the
instance/class variable list of a class header. Nothing else is taken
literally.

A `(` written against what precedes it, with no space between, is a call of it,
which is what gives the conventional shape:

```
printf("%d\n" 10)        ## the same as (printf "%d\n" 10)
f()                      ## the same as (f)
```

At statement level the outermost parentheses may be dropped:

```
printf "%d\n" 10
```

### Sending a message

`rcv:msg` binds a message to a receiver and is read as a *single item*. It is
sent once its arguments arrive, which happens in one of two ways - a glued `(`
carries them, or the list it heads does:

```
b:twice()                ## no arguments
b:plus(7)                ## one argument
(b:plus 7)               ## the same, with the enclosing list as the argument list
(b:twice)                ## no arguments again
```

Because the binding is one item, a send needs no parentheses of its own and
goes wherever a value goes:

```
r := b:twice()                   ## on the right of an assignment
printf "%d\n" (+ b:plus(1) 2)    ## in an argument
b:itself():tag()                 ## as the receiver of the next send
```

A receiver may be computed, and so may the selector. `rcv:(expr)` evaluates the
parenthesised part and sends whatever symbol it answers:

```
fun pick() { return #twice }
(b:(pick))               ## sends twice
b:(pick)()               ## the same
b:(pick())               ## calls pick, then calls its ANSWER as well - not the form you want
```

The parentheses in `(b:(pick))` are the send's own argument list, exactly as in
`(b:twice)`. Without them the binding heads nothing when it sits in an argument
position, and is refused.

The following forms are refused:

```
a:b:c                    ## a message that was never sent cannot receive another.
                         ## write (a:b):c, or a:b():c
r := a:b                 ## a binding that heads nothing is never sent.
                         ## write (a:b) or a:b()
```

Parentheses around a send that already carries its own glued argument list are
an extra call, for the same reason `(f(1))` calls what `f(1)` answers:

```
K:make(4)(9)             ## send, then call what it answered
(K:make 4)(9)            ## the same
(K:make(4))(9)           ## NOT the same - calls the answer with no arguments
```

### Binary operators

A binary operator between two operands is a message send, so it works for a
receiver whose class defines that selector:

```
class Money: Object (_amt) {
    fun[#ci] new(a) { self._amt := a ; return self }
    fun +(other) { return (Money:new (core.+ self._amt other:amt())) }
    fun amt() { return self._amt }
}

a := (Money:new 10)
b := (Money:new 5)
printf "%O\n" ((a + b):amt)      ## 15, the same as ((a:#+ b):amt)
```

## Builtin functions

Arithmetic and comparison:

```
+  -  *  mlt  /  div  rem  mdiv  mod  sqrt  abs
<  <=  >  >=  =  ==  !=
bit-and  bit-or  bit-xor  bit-not  bit-shift  bit-left-shift  bit-right-shift
```

Logic and equality - `and` and `or` are short-circuit special forms, while
`_and` and `_or` are ordinary functions that evaluate every argument:

```
not  _and  _or
eqv?  eql?  eqk?  nqv?  nql?  nqk?
```

Type predicates:

```
nil?  boolean?  character?  error?  smptr?  integer?  numeric?  string?
array?  bytearray?  dictionary?  fun?  class?  object?
```

Input, output and the rest:

```
printf  sprintf  scanf  sscanf
getb  getc  gets  putb  putc  puts
va-context  va-count  va-get
```

`sscanf` parses a string against a format and answers an `Array` of the values
it read; `scanf` is the same scanner over one line from the input handler. The
array holds only what matched, so its size reports how far the scan got:

```
printf "%O\n" (sscanf "%d %d" "12 34")    ## #[12 34]
printf "%O\n" (sscanf "%d %d" "12 xy")    ## #[12]
```

The conversions are `%d` for an integer (a bigint if it does not fit), `%x`,
`%o` and `%b` for other radices, `%f` for a fixed-point decimal, `%s` for a run
of non-whitespace, `%c` for exactly one character and `%%` for a literal
percent. A width limits how much is read (`%3d`) and `*` suppresses a
conversion (`%*d`), which consumes input without contributing a value.
Whitespace in the format matches any run of it, including none.

The `get`/`put` pairs read and write the input and output handlers:
`getc`/`putc` one character, `getb`/`putb` one byte, and `gets`/`puts` a
string - a line when reading. Reading answers `nil` at end of input.

The writing side takes any number of arguments and answers how many characters
or bytes went out:

	(putc 'a' 'b' 'c')           ## 3
	(puts "ab" 'c' 12)           ## 5 - writes abc12

It answers `nil` only when the stream ended before anything went out at all. A
stream that ends part way answers the short count rather than `nil`, because
that count is what says where to resume - a write, unlike a read, cannot be
retried from the start without repeating whatever already landed. Writing stops
at the first argument the stream refuses.

`puts` appends nothing, writes a byte array as bytes rather than characters, and
accepts a character or a small integer as well as a string - an integer in its
decimal spelling, so `(puts 12)` writes `12` and answers 2.

Writing to the log channel rather than to the output handler is `core.log` and
`core.logf`. These have no plain names - `log` is left free for a program to
use as it likes.

Further functions live in modules and are reached through a prefix: `core.` for
the object primitives (`core.basicNew`, `core.classOf`, `core.+`, `core.gc`, the
process and semaphore primitives), `dic.` for dictionaries (`dic.get`,
`dic.put`, `dic.size`, `dic.has?`) and `sys.` for the operating system.

## Class library

The classes written in HAK itself live in `src/` and are pulled in with
`$include-once`:

```
$include-once "object.hak"
```

- `object.hak` - `Object` and the root of the hierarchy
- `collection.hak` - `String`, `Array`, `ByteArray`, `Dictionary`
- `magnitude.hak` - `Magnitude`, `Character`, `Number`
- `stream.hak` - `Stream`, `HandleStream`, `ByteArrayStream`, `FileStream`
- `text-stream.hak` - `TextStream`, a text layer over any byte stream
- `process.hak` - `Process`, the green process
- `semaphore.hak`, `mutex.hak` - `Semaphore`, `Mutex`
- `external-process.hak` - `ExternalProcess`, `ExternalProcessGroup`
- `kernel.hak` - everything above in one include

## Defining a function

```
fun function-name(arguments) {
	| local variables |
	function body
}
```

```
set function-name (fun(arguments) {
	| local variables |
	function body
})
```

## Class

```
class[attributes] Name: Superclass (ivars (cvars)) {
    fun[attributes] name(arguments) {
        | local variables |
        function body
    }
}
```

An instance variable is reached through `self.`, a class variable by its bare
name:

```
class A (x y (cv1)) {
    fun[#ci] new() { self.x := 1 ; self.y := 2 ; return self }
    fun[#c] setcv() { cv1 := 99 ; return cv1 }
    fun show() { printf "x=%O y=%O\n" self.x self.y }
}
```

`#ci` marks a class method that instantiates, `#c` a plain class method; a
method with no attribute is an instance method.

### Class variable initialization

A class body is not only a list of methods. Statements written in it run once,
when the class is defined, which is where a class variable gets its value - next
to the declaration rather than in a separate method somebody has to remember to
call:

```
class Counter ((count limit)) {
    count := 0
    set limit 100

    fun[#ci] new() { count := (+ count 1) ; return self }
    fun[#c] howMany() { return count }
    fun[#c] getLimit() { return limit }
}

a := (Counter:new)
b := (Counter:new)
printf "%O of %O\n" (Counter:howMany) (Counter:getLimit)    ## 2 of 100
```

The initializer is an ordinary expression, so it may compute and may use
classes defined before this one:

```
class P ((base)) {
    base := 100
    fun[#c] getBase() { return base }
}

class Q ((derived)) {
    derived := (+ (P:getBase) 1)
    fun[#c] get() { return derived }
}

printf "%O\n" (Q:get)    ## 101
```

Three things the body cannot do, all following from when it runs:

- It cannot touch an instance variable. The body runs once and there is no
  instance yet, so `x := 5` against an ivar is refused with *prohibited access
  to instance variable*. Instance variables are set in a `#ci` method.
- It cannot send to the class being defined. The name is not bound until the
  definition completes, so calling one of its own class methods from the body
  reaches `nil`.
- A class variable and a method may not share a name - the second is reported
  as a *duplicate method name*.

```
class[#b] B (a b) {
    fun[#ci] new() {
        self.a := 88
        self.b := 99
    }

    fun print() {
        printf "A: %d B: %d\n" self.a self.b
    }
}

class[#b] C: B (c) {
    fun[#ci] new() {
        super:new
        self.c := 77
    }

    fun print() {
        super:print
        printf "C: %d\n" self.c
    }
}

x := (C:new)
x:print
```


## Redefining a primitive function

```
fun + (a b) {
	core.+ a b 9999
}
printf "%d\n" (+ 10 20)
```

## Variadic arguments

```
fun fn-y (t1 t2 va-ctx) {
        | i |
        set i 0
        while (< i (va-count va-ctx)) {
                printf "fn-y=>Y-VA[%d]=>[%d]\n" i (va-get i va-ctx)
                set i (+ i 1)
        }
}

fun x(a b ... :: x y z) {
    |i|

##  printf "VA_COUNT(x) = %d\n" (va-count)
    set x "xxx"
    set y "yyy"
    set z "zzz"
    set z (+ a b)

    set i 0
    while (< i (va-count)) {
        printf "VA[%d]=>[%d]\n" i (va-get i)
        set i (+ i 1)
    }
    fn-y "hello" "world" (va-context)

    return
}

printf "--------------------------\n"
printf "[%O]\n" (x 10 20 30)
printf "--------------------------\n"
set q (set-r a b c (x 10 20 30 40 50))
printf "--------------------------\n"
```

The `...` takes the extra arguments and `::` names the return variables. At the
call site those are collected either with `set-r` or by destructuring:

```
set-r p q (f 10)          ## p and q take the first two return variables
t := ([a b c] := (f 10))  ## the same, and t takes the first one
```

### Relaying the extra arguments

Writing `...` as the **last argument of a call** passes on everything this
function itself received beyond its fixed parameters:

```
fun three(a b c) { printf "%O %O %O\n" a b c }

fun relay(x ...) {
    three(x ...)          ## three gets x, then everything relay got past x
}

relay 1 2 3               ## 1 2 3
```

The two uses of `...` are inverses and never share a position - one is a
parameter list, the other an argument list - so there is no ambiguity between
collecting and relaying.

A relayed argument is an ordinary argument, so the callee needs to know nothing
about how it was called. If the callee is itself variadic, what it receives
becomes *its* pack, and `va-count` inside it sees the full number - which is
what lets relays chain:

```
fun counter(...) { return (va-count) }
fun once(...) { return counter(...) }
fun twice(...) { return once(...) }

printf "%O\n" (twice 1 2 3 4 5)    ## 5
```

It works the same in a message send, including one to `super`, and alongside
return variables. An empty pack relays nothing, so relaying into a fixed-arity
function is fine when there is nothing extra to pass and raises the ordinary
arity error when there is.

Two forms are refused. `...` anywhere but last is an error, which keeps the
argument order unambiguous; and `...` inside a function that declared no `...`
parameter is an error rather than a silent relay of nothing.

#### The pack belongs to the function that declared it

`...` reaches the pack of the function **directly** containing it. A nested
function does not inherit the pack of the one around it, even when that outer
function is variadic:

```
fun x(a b ...) {
    return (fun() {
        printf "%d %d %d\n" a b ...    ## refused
    })
}
```

```
syntax error - '...' not usable in a function that has no '...' parameter
```

The inner function declares no `...` of its own, so it has no pack to relay.
This is not a rule of its own - `va-count` and `va-get` already answer for the
function directly containing them, so an inner function sees a count of zero
where the outer one sees three:

```
fun x(a b ...) {
    | g |
    g := (fun() { return (va-count) })
    printf "outer va-count = %d\n" (va-count)    ## 3
    printf "inner va-count = %d\n" g()           ## 0
}

x 1 2 3 4 5
```

Relaying is refused at compile time rather than quietly passing nothing, which
is what the same code would do if it were allowed. Where an inner function does
need the outer arguments, name them or pass them in explicitly.

The older way - passing `(va-context)` explicitly and reading it with
`(va-count ctx)` and `(va-get i ctx)` - still works. It is no longer the only
way, and unlike a relay it requires the callee to be written for it.

## HAK Exchange Protocol

The HAK library contains a simple server/client libraries that can exchange
HAK scripts and results over network. The following describes the protocol
briefly.

### Request message
TODO: fill here

.BEGIN
.SCRIPT
.END
.EXIT
.KILL-WORKER
.SHOW-WORKERS


You can send a single-line script with a .SCRIPT command.

 .SCRIPT (printf "hello, world\n")

If the script is long and contains line-breaks, enclose multiple .SCRIPT commands 
with the .BEGIN and .END command.

  .BEGIN
  .SCRIPT (printf "hello ")
  .SCRIPT (printf "world\n")
  .END

### Reponse message

There are two types of response messages.
 - Short-form response
 - Long-form response

A short-form response is useful when you reply with a single unit of data.
A long-form response is useful when the actual data to return is more complex.

#### Short-form response
A short-form response is composed of a status line. The status line may span 
across multiple line if the single response data item can span across multiple
lines without ambiguity. A short-form response begins with a status word. 
The status word gets followed by an single data item.

There are 2 status word defined.
 - .OK
 - .ERROR

The data must begin on the same line as the status word and there should be 
as least 1 whitespace characters between the status word and the beginning of
the data. The optional data must be processible as a single unit of data. The
followings are accepted:

 * unquoted text line
   ** The end of the data is denoted by a newline character. The newline
      character also terminates the status line.
   ** Leading and trailing spaces must be trimmed off
 * quoted text
   ** If the first meaningful character of the option data is a double quote,
      the option data ends when another ordinary double quote is encounted.
   ** If a double quote is preceded by a backslash(\"), the double quote becomes
      part of the data and doesn't end the data.
   ** Not only the double quote, any character character escaped by a preceding
      backslash is treated literally. (e.g. \\ -> a single back slash, \n -> n)
   ** Trailing spaces after the ending quote must be ignored until a newline
      character is encounted. The newline character terminates the status line.
    
Take note of the followings when parsing a short-form response message
 * Whitespace characters before the status word shall get ignored.

See the following samples.

  .OK authentication success

  .ERROR double login attempt

  .OK "authentication\twas\tsuccessful"

  .OK "this is a multi-line
    string message"


#### Long-form response

A long-form response begins with the status word line. The status line 
should be composed of the status word and a new line. The status line must
get followed by data format line and the actual response data. Optional 
attribute lines may get inserted between the status line and the data format
line.

The data format line begins with .DATA and it can get followed by a data length
or the word 'chunked'.

 * .DATA <NNN>
 * .DATA chunked
 
Use .DATA <NNN> where <NNN> indicates the length of data in bytes if the
response data is length-bounded. For instance, .DATA 1234 indicates that
the following data is 1234 bytes long.

Use .DATA chunked if you don't know the length of response data in advance.

The actual data begins at the next line to .DATA.

The length-bounded response message looks like this. The response message 
handler must consume exactly the number of bytes specifed on the .LENGTH line
starting from the beginning of the next line to .DATA without ignoring any 
characters.

```
 .OK
 .DATA 10
 aaaaaaaaaa
```

The chunked data looks like this. Each chunk begins with the length and a colon.
The 0 sized chunk indicates the end of data. A chunk size is in decimal and is
followed by a colon. The actual data chunk starts immediately after the colon.
The response processor must consume exactly the number of bytes specified in
the chunk size part and attempt to read the size of the next chunk. 

The whitespaces between the end of the previous chunk data and the next chunk
size, if any, should get ignored.

```
 .OK
 .DATA chunked
 4:xxxx10:abcdef
 ghi
 0:
```

With the chunked data transfer format, you can revoke the ongoing response data
and start a new response.

```
 .OK
 .DATA chunked
 4:xxxx-1:.ERROR "error has occurred"
```

An optional attribute line is composed of the attribute name and the attribute
value delimited by a whitespace. There are no defined attributes but the attribute
name must not be one of .OK, ERROR, .DATA. The attribute value follows the same
format as the status data of the short-form response.

```
 .OK
 .TYPE json/utf8
 .DATA chunked
 ....
```
