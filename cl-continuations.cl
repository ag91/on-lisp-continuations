(setq *cont* #'identity)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body)) ;; *cont* is added by the macro to pass the continuation

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,' ,f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(macroexpand '(=defun add1 (x) x))
;; => (PROGN (DEFMACRO ADD1 (X) `(,'=ADD1 *CONT* ,X)) (DEFUN =ADD1 (*CONT* X) X))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(macroexpand '(=values (1+ n)))
;; => (FUNCALL *CONT* (1+ N))

(=defun bar (x) (=values (list 'a (1+ x))))
(bar 5)

(=defun message ()
  (=values 'hello 'there))

(=defun baz ()
  (=bind (m n) (message)
    (=values (list m n))))

(baz)
;; => (HELLO THERE)

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(=defun add1 (x) 
  (=values (1+ x)))

(let ((fn (=lambda (n) (add1 n))))
  (=bind (y) (=funcall fn 9)
    (format nil "9 + 1 = ~A" y)))
;; => 9 + 1 = 10

(defparameter *paths* nil)

(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
    `(progn
      ,@(mapcar #'(lambda (c)
                    `(push #'(lambda () ,c) *paths*))
                (reverse (cdr choices)))
      ,(car choices))
    '(fail)))

(defun do2 (x)
  (choose (+ x 2) (* x 2) (expt x 2)))

(do2 3)
;; => 5

(defun fail ()
  (if *paths*
    (funcall (pop *paths*))
    failsym))

(fail)
;; => 9

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
    (progn
      (if (cdr choices)
          (push #'(lambda () (cb fn (cdr choices)))
                *paths*))
      (funcall fn (car choices)))
    (fail)))

(choose-bind x '(marrakesh strasbourg vegas)
  (format nil "Let's go to ~A." x))
;; => Let's go to MARRAKESH.

(fail)
;; => Let's go to STRASBOURG.

(=defun two-numbers ()
  (choose-bind n1 '(0 1 2 3 4 5)
    (choose-bind n2 '(0 1 2 3 4 5)
      (=values n1 n2))))

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
      `(the sum of ,n1 ,n2)
      (fail)))) ;; we try until all choices are exhausted

(parlor-trick 7)
;; => (THE SUM OF 2 5)
(parlor-trick 8)
;; => (THE SUM OF 3 5)
(parlor-trick 1)
;; => (THE SUM OF 0 1)

* [[https://jyp.github.io/posts/elisp-cps.html][Not JP's Blog - Concurrency in elisp via CPS]] :website:

[2017-09-20 Wed 23:01]

** Article

<<content>>
*** Concurrency in elisp via CPS

  Last updated on November 1, 2016

  Tags: [[../tags/elisp.html][elisp]], [[../tags/continuations.html][continuations]]

  There is apparently quite a bit of excitation around a “concurrency branch” which may make it to Emacs master in a not-too-distant future. In this post I show that asynchronous programs can be conveniently written in Emacs already today.

*** Points against thread-based concurrency

  Even though there seems to be a lot of enthusiasm around the concurrency branch, there are dissident voices. Lunaryorn (of flycheck) is critical of changing Emacs core to support concurrency, and argues that instead Emacs should support “Futures”.

  Let me summarize Lunaryorn's points against the approach proposed in the concurrency branch.

  -  Cooperative threads do not fundamentally change the programming model.

  -  The programmer is essentially left with handling locks and shared resources themselves.

  -  Concurrency issues should ultimately be solved by a proper programming model rather than by adding a feature to elisp.

  I am generally agreeing with all those points (until proven otherwise).

*** Concurrency in Emacs Today

  Existing asynchronous programs in Emacs revolve around callbacks. That is, if a function depends on an external resource being ready, it will typically take a callback as argument, do an asynchronous wait on the resource and, when the resource is ready, run the callback. For example, I have recently written code such as this:

  #+BEGIN_EXAMPLE
       (blocking-call
        (lambda (ret)
           (dante-async-load-current-buffer
            nil
            (lambda
              (_load-messages)
              (dante-async-call
               (concat ":loc-at " symbol)
               (lambda (target)
               (let ((xref (dante--make-xref target "def")))
                 (funcall ret (when xref (list xref))))))))))
  #+END_EXAMPLE

  In the above, both =dante-async-load-current-buffer= and =dante-async-call= run commands in an external process. When the response is ready, they call their last argument and pass it the response. Such last argument is called a “continuation”, and code written in the above style is deemed in “continuation-passing style” or CPS for short.

  Even though CPS is syntactically heavy, it is is a good way to structure code which can potentially “yield” to other processes. There is a wealth of literature on how to structure code around CPS for asynchronous programming (including some of [[https://jyp.github.io/pdf/Organ.pdf][mine]]). CPS code used to be extremely unwieldy to write in elisp, because of dynamic scoping: one needs to explicitly save the state needed by further continuations. A contrario, with lexical scoping, one simply accesses the needed variables, and the run-time takes care of saving the needed state in a closure. Fortunately lexical binding has been available in Emacs for a long time, and package writers may assume that it is there.

*** CPS syntactic-sugar

  As the saying goes: “a bit of syntactic sugar helps swallowing the semantics medicine”. In this case, the CPS syntactical overhead can be diminished drastically thanks to macros. Indeed, given a single =cps-let= macro, the above code can be rewritten as follows:

  #+BEGIN_EXAMPLE
       (cps-let ((ret (blocking-call))
                (_load-messages (dante-async-load-current-buffer nil))
                (target (dante-async-call (concat ":loc-at " symbol))))
        (let ((xref (dante--make-xref target "def")))
          (funcall ret (when xref (list xref)))))
  #+END_EXAMPLE

  The programmer conceptually binds the result of =(blocking-call)= to =res=, that of (dante-async-load-current-buffer nil) to =_load-messages=, and that of =(dante-async-call (concat ":loc-at " symbol))= to =target=. Everything looks (nearly) as tidy as synchronous code, even though we call an asynchronous function at each line.

  The macro facilities of (Emacs) lisp allow to implement =cps-let= in a handful of lines, as follows (I also support binding several variables at once).

  #+BEGIN_EXAMPLE
       (defmacro cps-bind (vars expr &rest body)
        "Bind VARS in a continuation passed to EXPR with contents BODY.
      So (cps-bind x (fun arg) body) expands to (fun arg (λ (x) body))"
        (declare (indent 2))
        (if (listp vars)
            `(,@expr (lambda ,vars ,@body))
        `(,@expr (lambda (,vars) ,@body))))

      (defmacro cps-let (bindings &rest body)
      "Expand multiple BINDINGS and call BODY as a continuation.
      Example: (cps-let ((x (fun1 arg1)) (y z (fun2 arg2))) body)
      expands to: (fun1 arg1 (λ (x) (fun2 arg2 (λ (x y) body))))."
        (declare (indent 1))
        (pcase bindings
          (`((,vars ,expr)) `(cps-bind ,vars ,expr ,@body))
          (`((,vars ,expr) . ,rest) `(cps-bind ,vars ,expr (cps-let ,rest ,@body)))))
  #+END_EXAMPLE

  Perhaps the above macros even exist somewhere in Emacs already.

*** Conclusion

  As far as I am concerned, there is no need to change elisp in any way to support concurrency. In particular, I do not see a use for “cooperative threads”. I do not see a need for “Futures”, as Lunaryorn advocates. Instead, the CPS convention can support a suitable concurrent programming model with today's elisp. Additionally, these CPS “threads” are extremely lightweight and customizable. Indeed, the scheduler is provided by the user. (This is no recent discovery: the idea can be traced to the seventies at least.) Even further, they are syntactically pleasing, thanks to macros.

  Instead of changing the core of Emacs to support concurrency, I would thus recommend to write a standard library for CPS-based concurrency. This library would include a scheduler, standard functions to talk to external processes, and macros hiding the syntactic overhead.
