;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.documentation)

(def project :hu.dwim.reiterate :path (system-pathname :hu.dwim.reiterate)
  :description "General iteration/looping macro")

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO")
    (chapter (:title "Origina of the name")
      (paragraph ()
        "The reiterate name was contributed by _3b on #lisp, thanks!")))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))
