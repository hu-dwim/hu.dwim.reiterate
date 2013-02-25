;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2013 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.reiterate+hu.dwim.logger
  :defsystem-depends-on (:hu.dwim.asdf
                         ;; KLUDGE this is a half-assed solution to be able to load the lib with full logging when developing.
                         ;; downside: even merely loading the .asd requires hu.dwim.logger in the path.
                         :hu.dwim.logger)
  :class hu.dwim.asdf:hu.dwim.system
  :depends-on (:hu.dwim.reiterate
               :hu.dwim.logger))
