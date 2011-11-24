;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def constant +toplevel-macro-aliases+ '(iter iterate))

(def macro for-each-iterator-alias (alias-variable-name &body body)
  `(macrolet ((body-emitter (,alias-variable-name)
                ,@body))
     ,@(loop
         :for alias :in +toplevel-macro-aliases+
         :collect `(body-emitter ,alias))))

(def special-variable *loop-form-stack* '()
  "A stack of LOOP-FORM instances representing the nesting of loop-form's.")

(def (special-variable :documentation "Holds an instance of LOOP-FORM that describes the corrent loop.")
  *loop-form*)

(def (special-variable :documentation "Holds the cons cells describing the current clause.")
  *clause*)

(def (special-variable e) *preserve-source-form-identities* #t
  "Try to preserve source code cons cell identities, so that the debugger can properly locate source forms inside reiterate loops (e.g. Slime's 'v' on a stack frame). When this is enabled, and your lisp is optimizing literals properly (e.g. SBCL in compile mode), then you may experience problems with quoted forms in compiled mode, e.g. when something this is compiled (eval '(iter (repeat 2) (iter (repeat 2)))).")
