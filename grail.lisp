;;;; GRAIL Parser Generator in Lisp

;;; The GRAIL metalanguage defines abstract data types corresponding to Lisp objects.
;;; For example, one might define WFFs in the propositional calculus using the notation
;;;   <WFF> ::= <proposition> | <negation> | <conjunction> | <disjunction> |
;;;             <implication> | <equivalence>
;;;   <proposition> ::= <prop-name:symbol>
;;;   <negation>    ::= (not <negend:WFF>)
;;;   <conjunction> ::= (and <conjuncts:WFF+>)
;;;   <disjunction> ::= (or <disjuncts:WFF+>)
;;;   <implication> ::= (<antecedent:WFF> implies <consequent:WFF>)
;;;   <equivalence> ::= (<condition1:WFF> equiv <condition2:WFF>)
;;; The first rule is a so-called alternation rule, defining the different forms a WFF
;;;   can take, and the other rules are construction rules, defining the s-expression
;;;   format of each form and the types and accessor functions of each nonterminal in it.
;;; The program is to generate, from each constructed nonterminal, a recognizer that takes
;;;   a random symbolic expression and returns whether it fits the form of the nonterminal,
;;;   accessor functions that take a symbolic expression assumed to fit the form of the 
;;;   nonterminal and retrieve the given element in the expression, a constructor that
;;;   takes the elements in the nonterminal's construction in order and constructs the 
;;;   corresponding nonterminal, and updaters that replace a specific element.

;;;; Package definition

(defpackage #:GRAIL-GENERATOR
	    (:use #:COMMON-LISP)
	    (:export #:GRAIL)
	    (:documentation 
	      "Program that generates functions automatically from a collection of GRAIL rules."))

(in-package #:GRAIL-GENERATOR)

;;;; Main program

;;; Possible paths for growth:
;;; - Provide for default number and symbol types, with predicates numberp and symbolp (Done)
;;; - Automatically name arguments by order of occurrence in expression if unnamed (Done)
;;; - Shorten the result of acc-shortest-path for e.g. caddr
;;; - Individually mark the mutability of each defined type on the nonterminal
;;; - Append lists of arguments, as in (and (and <args1>) (and <args2>)) -> (and <args1> . <args2>)
;;; - Define functions used in the program inside a separate package (Done)
;;; - Generate argument names from their type, not from "ARG"

;; extend for alternation rules, only for predicates
(defmacro GRAIL (grail-list &key (mutable NIL) (default NIL))
  "Generate functions from a collection of GRAIL rules.
   A GRAIL rule is defined by the following BNF syntax:
   <GRAIL>       ::= <assignment> | <alternation>
   <assignment>  ::= \"(\"<type>\" ::= \"<s-exp>\")\"
   <alternation> ::= \"(\"<type>\" ::= \" <type> { <type>}\")\"
   <s-exp>       ::= <symbol> | <nonterminal> | \"(\"{<s-exp>}\")\"
   <type>        ::= \"#(\"<type-name>\")\"
   <nonterminal> ::= \"#(\"<prop-name> <type-name>\")\"
   <type-name>   ::= <symbol>
   <prop-name>   ::= <symbol>
   The key :MUTABLE designates whether updater functions should be generated, 
   and the key :DEFAULT designates (optionally) a default type, a type that is 
   considered to encompass all inputs and is not verified for efficiency purposes."
  (let ((assign-list (remove-if-not #'assignment-p (eval grail-list)))
        (alter-list (remove-if-not #'alternation-p (eval grail-list)))
        (default-name (eval default)))
    `(progn ,@(mapcar (lambda (x) (gen-alter-recognizer x :default default-name)) alter-list)
            ,@(mapcan (lambda (x) (gen-functions x :mutable mutable :default default-name))
                      assign-list)
            'OK)))

(defun gen-functions (assignment &key (mutable NIL) (default NIL))
  "Generate a list of function definitions from a GRAIL assignment."
  (let ((name (grail-name assignment))
        (expr (name-args (car (grail-expr assignment)))))
    `(,(gen-constructor name expr)
      ,@(gen-accessors name expr)
      ,(gen-recognizer name expr :default default)
      ,@(when mutable (gen-updaters name expr)))))
       
(defun gen-constructor (name expr)
  "Generate constructor from GRAIL assignment of name NAME and body EXPR."
  `(defun ,(make-cons-name name) ,(nonterminal-names expr)
     ,(format nil "Construct an instance of ~A." name)
     ,(make-cons-body expr)))
     
(defun gen-accessors (name expr &aux (args (nonterminals expr)))
  "Generate list of accessors from GRAIL assignment of name NAME and body EXPR."
  (mapcar (lambda (arg)
            `(defun ,(make-acc-name name arg) (,name)
               ,(format nil "Return the ~A of an instance of ~A." 
                 (nonterminal-name arg) name)
               (when (,(make-rec-name name) ,name)
                 ,(make-acc-body name arg expr))))
          args))

;; should also use recognizers of types of elements                 
(defun gen-recognizer (name expr &key (default NIL))
  "Generate recognizer from GRAIL assignment of name NAME and body EXPR."
  `(defun ,(make-rec-name name) (,name)
     ,(format nil "Return whether an S-expression is an instance of ~A." name)
     ,(if (eql name default) 
          T 
          (make-rec-body name expr))))
     
(defun gen-updaters (name expr &aux (args (nonterminals expr)))
  "Generate list of updaters from GRAIL assignment of name NAME and body EXPR."
  (mapcar (lambda (arg)
            `(defun ,(make-upd-name name arg) (new-value ,name)
               ,(format nil "Update the ~A of an instance of ~A." 
                 (nonterminal-name arg) name)
               (when (,(make-rec-name name) ,name)
                 ,(make-upd-body name arg expr))))
          args))

(defun gen-alter-recognizer (alternation &key (default NIL)
                             &aux (name (grail-name alternation)))
  "Generate recognizer for GRAIL alternation."
  `(defun ,(make-rec-name name) (,name)
     ,(format nil "Return whether an S-expression is an instance of ~A." name)
     ,(if (eql name default) 
          T
          (cons 'or (mapcar (lambda (typ)
                              (list (make-rec-name typ) name))
                            (alternation-types alternation))))))
 
;;;; Generators
 
;;; Name generators

(defun make-cons-name (name)
  "Construct a constructor name from NAME."
  (intern (format nil "MAKE-~A" name)))
  
(defun make-acc-name (name arg)
  "Construct an accessor name from NAME and ARG."
  (intern (format nil "~A-~A" name (nonterminal-name arg))))
  
(defun make-rec-name (name)
  "Construct a recognizer name from NAME."
  (if (assoc name *built-in-types*) 
      (cdr (assoc name *built-in-types*))
      (intern (format nil "~A-P" name))))
  
(defun make-upd-name (name arg)
  "Construct an updater name from NAME and ARG."
  (list 'setf
        (intern (format nil "~A-~A" name (nonterminal-name arg)))))
  
;;; Body generators

(defun make-cons-body (expr)
  "Construct a constructor body from EXPR."
  (cond ((null expr) NIL)
        ((literal-p expr) `(quote ,expr))
        ((nonterminal-p expr) (nonterminal-name expr))
        (T `(cons ,(make-cons-body (car expr))
                  ,(make-cons-body (cdr expr))))))
  
(defun make-acc-body (name arg expr)
  "Construct an accessor body from NAME, ARG and EXPR."
  (reduce #'list (reverse (acc-shortest-path arg expr)) :initial-value name :from-end T))
 
(defun acc-shortest-path (arg expr)
  "Return the shortest car-cdr path to reach an instance of ARG in EXPR."
  (labels ((cons-if (x y) (if (not (eq y T)) (cons x y) T))
           (shorter-path (x y) (cond ((eq x T) y)
                                     ((eq y T) x)
                                     ((< (length x) (length y)) x)
                                     (T y))))
    (cond ((null expr) T)
          ((equalp arg expr) NIL)
          ((atom expr) T)
          (T (shorter-path
               (cons-if 
                 'car 
                 (acc-shortest-path arg (car expr)))
               (cons-if 
                  'cdr
                  (acc-shortest-path arg (cdr expr))))))))
  
;; (defun deep-memberp (arg expr)
;;   "Return T if ARG is a deep member of EXPR."
;;   (or (equal arg expr)
;;       (and (consp expr)
;;            (or (deep-memberp arg (car expr))
;;                (deep-memberp arg (cdr expr))))))
 
;; now make it adapt to kleene star types
;; does not check for repeating variables
;; nested ands unnecessary
(defun make-rec-body (name expr)
  "Construct a recognizer body from NAME and EXPR."
  (cond ((null expr)
         `(null ,name))
	    	((literal-p expr)
	    	 `(equal ,name (quote ,expr)))
	    	((nonterminal-p expr)
	    	 (if (star-type-p expr)
             `(and (listp ,name)
                   (every (function ,(make-rec-name (star-type-type expr)))
                          ,name))
             (list (make-rec-name (nonterminal-type expr))
	    	    	      name)))
	    	(T `(and (consp ,name)
	    	         ,(make-rec-body (list 'car name) (car expr))
	    			     ,(make-rec-body (list 'cdr name) (cdr expr))))))
  
(defun make-upd-body (name arg expr)
  "Construct an updater body from NAME, ARG and EXPR."
  (list 'setf
        (make-acc-body name arg expr)
        'new-value))
        
;;;; Data types

;;; A GRAIL statement exists either as an assignment rule, which matches a type to 
;;;   a single expression formed of literals and nonterminals, or as an alternation rule
;;;   which breaks down a type to a list of different types.
;;; Example notation:
;;;   (#(expr) ::= #(number) #(variable) #(negation) #(addition))
;;;   (#(addition) ::= (#(arg1 expr) + #(arg2 expr))

;;; A nonterminal is a vector of 2 elements, the first being the identifier of the
;;;   nonterminal and the second being the type of the nonterminal.

(defconstant *built-in-types*
  '((number . numberp) (symbol . symbolp) (list . listp) 
    (atom . atom) (function . functionp))
  "List of built-in types and their recognizers.")

(defun make-grail (name expr)
  "Construct GRAIL statement with NAME and EXPR."
  `(,name ::= . ,expr))
  
(defun grail-name (stmt)
  "Return the name of a GRAIL statement."
  (and (consp stmt) 
       (car stmt)
       (type-identifier-p (car stmt))
       (type-identifier-name (car stmt))))
  
(defun grail-expr (stmt)
  "Return the expression of a GRAIL statement."
  (and (consp stmt)
       (consp (cdr stmt))
       (cddr stmt)))
       
(defun assignment-p (stmt &aux (expr (grail-expr stmt)))
  "Return T if STMT is an assignment rule."
  (and (consp expr)
       (null (cdr expr))))

(defun alternation-p (stmt &aux (expr (grail-expr stmt)))
  "Return T if STMT is an alternation rule."
  (and (consp expr)
       (consp (cdr expr))
       (every #'type-identifier-p expr)))

(defun alternation-types (stmt &aux (expr (grail-expr stmt)))
  "Return the alternation types of an alternation rule."
  (when (alternation-p stmt)
        (mapcar #'type-identifier-name expr)))

(defstruct (type-identifier (:type vector))
  "Type identifier with given name."
  name)

(defstruct (nonterminal (:type vector))
  "Nonterminal with given name and type."
  name type)

(defun make-type-identifier (name)
  "Construct type identifier of given name."
  (vector name))

(defun type-identifier-name (expr)
  "Return name of type identifier."
  (when (type-identifier-p expr)
        (svref expr 0)))

(defun type-identifier-p (expr)
  "Return T if EXPR is a type identifier."
  (and (vectorp expr)
       (= (length expr) 1)
       (symbolp (svref expr 0))))
       
(defun make-nonterminal (name type)
  "Construct nonterminal of given name and type."
  (vector name type))
  
(defun nonterminal-name (nont)
  "Return the identifier of a nonterminal."
  (and (vectorp nont)
       (= (length nont) 2)
       (svref nont 0)))
       
(defun nonterminal-type (nont)
  "Return the type of a nonterminal."
  (and (vectorp nont)
       (= (length nont) 2)
       (svref nont 1)))

(defun make-star-type (type)
  "Construct Kleene star type of TYPE."
  (intern (format nil "~A*" type)))

(defun star-type-type (type &aux (name (symbol-name (type-identifier-name type))))
  "Return the parent type of the Kleene star type."
  (when (star-type-p type)
    (intern (subseq name 0 (1- (length name))))))

(defun star-type-p (type &aux (name (symbol-name (type-identifier-name type))))
  "Return T if TYPE is a Kleene star of some other type."
  (char= (char name (1- (length name))) #\*))
    
(defun nonterminal-p (expr)
  "Return T if EXPR is a nonterminal."
  (and (vectorp expr)
       (= (length expr) 2)
       (symbolp (svref expr 0))
       (symbolp (svref expr 1))))
       
(defun nonterminal-names (expr)
  "Return the names of all the nonterminals of an expression."
  (mapcar #'nonterminal-name (deep-filter #'nonterminal-p expr)))

(defun deep-filter (pred tree)
  "Search through TREE, accumulating all and only the atoms that satisfy PRED."
  (if (atom tree)
	  (when (funcall pred tree) (list tree))
	  (mapcan #'(lambda (x) (deep-filter pred x))
	   	      tree)))
  
(defun literal-p (expr)
  "Return T if EXPR is a literal atom or a list of literals."
  (or (and (atom expr)
           (not (nonterminal-p expr))
           (not (type-identifier-p expr)))
      (and (consp expr)
           (every #'literal-p expr))))
           
(defun name-args (expr)
  "Name the arguments of an assignment body if they are unnamed."
  (let ((nonts (nonterminals expr)))
    (labels ((generate-sym (name counter)
               "Generate argument name from counter."
               (intern (format nil "~A~A" name counter)))
             (find-first (expr counter)
               "Find first argument name that can be used for the given type identifier."
               (let ((test-sym (make-nonterminal :name (generate-sym (type-identifier-name expr) 
                                                                     counter) 
                                                 :type (type-identifier-name expr))))
                 (if (member test-sym nonts :test #'equalp)
                     (find-first expr (1+ counter))
                     (values test-sym (1+ counter)))))
             (assign-val (expr counter)
               "Go through each unit of the expression."
               (cond ((literal-p expr) 
                      (values expr counter))
                     ((nonterminal-p expr) 
                      (values expr counter))
                     ((type-identifier-p expr)
                      (find-first expr counter))
                     (T (let* ((vals (multiple-value-list 
                                       (assign-val (car expr) counter)))
                               (res (first vals))
                               (contr (second vals)))
                          (cons res (assign-val (cdr expr) contr)))))))
      (assign-val expr 1))))
