# Mini Lisp

A simple Lisp interpreter written in Rust. It supports basic arithmetic
operations, conditional expressions, and lambda functions.

## Installation

You need to have Rust installed. If you don't have Rust, you can download it
from the official Rust website: https://www.rust-lang.org/tools/install

Once you have it, you can clone the Mini Lisp repository from GitHub:

```bash
git clone https://github.com/joaofreires/mini-lisp.git
```

## Usage

You can use the interactive shell:

```shell
$ cargo run
mini-lisp> (+ 1 2 3)
Number(6)
```

Or you can provide it with a Lisp file to evaluate. You can create a file called
`example.lisp` with the following code:

```lisp
(+ 1 2 3)
```

Then, you can run Mini Lisp by executing the following command in the terminal:

```shell
cargo run example.lisp
```

It will evaluate the code in `example.lisp` and print the result:

## Syntax

It supports the following syntax:

### Arithmetic operations

```lisp
(+ 1 2 3)      ; 6
(- 5 2)        ; 3
(* 2 3 4)      ; 24
(/ 10 2)       ; 5.0
```

### Conditional expressions

```lisp
(if (= 2 2) "yes" "no") ; "yes"
(cond ((= 1 2) "no") ((= 2 2) "yes") (else "no")) ; "yes"
(and (> 2 1) (< 3 4)) ; true
```

### Lambda functions

```lisp
((lambda (x y) (+ x y)) 3 4) ; 7
```

### Cons cell operations

```lisp
(cons 1 2) ; (1 2)
(car (cons 1 2)) ; 1
(cdr (cons 1 2)) ; 2
```
