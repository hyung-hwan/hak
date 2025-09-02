# HAK - Command Language

![HAK](hak.png)

## Language Syntax

A HAK program is composed of expressions.

## Keywords
- nil
- true
- false

## Special Form Expression
- and
- break
- class
- fun
- do
- elif
- else
- fun
- if
- lambda
- or
- return
- set
- until
- while

### do

```
do;
do 10;
do { | k | set k 20; printf "k=%d\n" k; };
```

## Literals
- integer
- character `'c'`
- small pointer
- error
- string `"string"`
- byte-string `b"string"`
- symbol `#"symbol"`

## Basic Expressions
- dictionary `#{ }`
- array `#[ ]`
- byte array `#b[ ]`
- character array `#c[ ]`
- list `#( )`
- function calls `( )`
- message sends `(rcv:msg arg1 ...)`
- variable declaration `| |`
- class variable delcarations `:: [ v1 [cv1 cv2 ...] v2 ... ] `
- assignment `var :=  value`

## Builtin functions

* not
* _and
* _or
* eqv?
* eql?
* eqk?
* nqv?
* nql?
* nqk?
* sprintf
* printf
* _+_
* _-_
* _*_
* mlt
* /
* quo
* mod
* sqrt
* bit-and
* bit-or
* bit-xor
* bit-not
* bit-shift

## Defining a function

```
(fun function-name (arguments)
	| local variables |
	function body
)
```

```
(set function-name (lambda (arguments)
	| local variables |
	function body
)
```

## Class

```
(class T
        :: | A B C | ## class variables

        (printf "initializing....\n")

        (fun :: dump()
                (printf "%d %d %d\n" A B C)
        )

        (set A 10)
        (set B 20)
        (set C 30)

        (printf "initialization done....\n")
)

(:T dump)
```

## Redefining a primitive function

```
(set prim-plus +)
(fun + (a b ...)
	(prim-plus a b 9999)
)

(printf "%d\n" (+ 10 20))
```

## Variadic arguments

```
(fun fn-y (t1 t2 va-ctx)
        | i |
        (set i 0)
        (while (< i (va-count va-ctx))
                (printf "fn-y=>Y-VA[%d]=>[%d]\n" i (va-get i va-ctx))
                (set i (+ i 1))
        )
)

(fun x(a b ... :: x y z)
        |i|

##       (printf "VA_COUNT(x) = %d\n" (va-count))
        (set x "xxx")
        (set y "yyy")
        (set z "zzz")
        (set z (+ a b))

        (set i 0)
        (while (< i (va-count))
                (printf "VA[%d]=>[%d]\n" i (va-get i))
                (set i (+ i 1))
        )
        (fn-y "hello" "world" (va-context))

        (return)
)

(printf "--------------------------\n")
(printf "[%O]\n" (x 10 20 30))
(printf "--------------------------\n")
(set q (set-r a b c (x 10 20 30 40 50)))
(printf "--------------------------\n")
```

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
