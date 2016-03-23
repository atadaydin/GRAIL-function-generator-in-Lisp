# GRAIL function generator
Automatically generates functions for abstract data types given their GRAIL representation.



## Motivation

Defining helper functions for abstract data types in symbolic computing tasks is a repetitive and tedious ordeal. To alleviate this, the program allows the user to generate such functions automatically given the GRAIL syntax of the data types.



## Usage

GRAIL, short for GRAmmars In Lisp, is a notation format for abstract data types in Lisp, which extends Backus-Naur Form (BNF) by the addition of nonterminal argument names (Cameron and Dixon 1992). The program uses a functionally equivalent representation of GRAIL rules to automatically define functions relating to a given list of GRAIL rules.

To run the program, first move the file to the directory you wish to use the file in and run `(load "grail.lisp")` before running the program inside another program or a REPL. Then run the command `(grail <grail-list>)` where `<grail-list>` is a list of GRAIL rules defined according to the following BNF syntax:

```
<grail-list>  ::= "'(" {<grail-rule>} ")"
<grail-rule>  ::= <assignment> | <alternation>
<assignment>  ::= "(" <type> " ::= " <s-exp> ")"
<alternation> ::= "(" <type> " ::= " <type> {<type>} ")"
<s-exp>       ::= <symbol> | <nonterminal> | "(" {<s-exp>} ")"
<type>        ::= "#(" <type-name> ")"
<nonterminal> ::= "#(" {<arg-name> " "} <type-name> ")"
<type-name>   ::= <symbol>
<arg-name>    ::= <symbol>
```

Then three types of helper functions for assignment rules, namely constructors, accessors and recognizers, as well as recognizers for alternation rules, will be automatically defined by the command for each type defined in the GRAIL rules. The user can also optionally define updaters for types, in case of space considerations or for ease of use, that destructively modify expressions based on a given argument of the type. The user might not define a name for a nonterminal, in which case the program will automatically generate one based on order in the definition. The functions that the program generates are of the following BNF syntax, provided a given `<type-name>` and an `<argument>`, which is the name of a nonterminal invoked in the definition of the type, drawn from `<arguments>` which contains all the arguments of the type:
```
<constructor> ::= "(make-" <type-name> " " <arguments> ")"
<accessor>    ::= "(" <type-name> "-" <argument> " expr)"
<recognizer>  ::= "(" <type-name> "-p expr)"
<updater>     ::= "(setf " <accessor> " new-value)"
```

Moreover, there are two optional keywords that can be invoked in the execution of the program: `:mutable` and `:default`. `:mutable` refers to a Boolean value, which when true instructs the program to also generate updater functions, and `:default` refers to the name of a type which should be considered to be the default type of all expressions handled in the program. `:default` can be used in handling general types, recognizers for which would otherwise have to scan deep through a given expression would simply return true, but which are nevertheless important to include in the definitions for recursively defined types and for sake of clarity.

The program does not verify the completeness of the provided rules; the user has to handle them manually or the program will define functions that call functions that aren't defined. The program also does not take transformation rules around a canonical form, such as associativity or double negation, into account when handling expressions; such transformations have to be defined manually by the user. The program allows for a list of built-in types, e.g. number, symbol, list, atom and function, that do not need to be defined by the rules and instead refer to built-in Lisp objects. Then instead of generating new recognizers for such types, the program uses the default recognizers (which do not fit the convention used for user-defined recognizers) provided by Lisp in the bodies of other recognizers. 

### Example

The following code defines functions from a syntax describing a toy subset of arithmetical expressions:
```
(grail '((#(expr)  ::= #(number) #(negation) #(sum) #(diff))
         (#(negtn) ::= (- #(negend expr)))
         (#(sum)   ::= (#(arg1 expr) + #(arg2 expr)))
         (#(diff)  ::= (#(arg1 expr) - #(arg2 expr))))
       :default 'expr)
```
Then the helper functions explained above can be used as follows:
```
(make-diff '(1 + 2) '4)    ; => ((1 + 2) - 4)
(negtn-negend '(- 6))      ; => 6
(sum-p '((3 + 4) - (- 5))) ; => NIL
```

Using these functions, one might write a simple arithmetical calculator as follows:
```
(defun calculate (expr)
  "Evaluate an arithmetical expression and return the result."
  (cond ((numberp expr) expr)
        ((negtn-p expr)
         (- (calculate (negtn-negend expr))))
        ((sum-p expr)
         (+ (calculate (sum-arg1 expr))
            (calculate (sum-arg2 expr))))
        ((diff-p expr)
         (- (calculate (diff-arg1 expr))
            (calculate (diff-arg2 expr))))))
```
Thanks to the program, the user need only write one function, when he would otherwise have to write 9, not counting the unused constructors. The constructors could be used instead in a calculator that also handles variables or canonical forms.
