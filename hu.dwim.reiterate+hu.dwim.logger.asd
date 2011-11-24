;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.reiterate+hu.dwim.logger
  :class hu.dwim.system
  :description "Loads hu.dwim.logger first, so that proper logging is available when developing, but not making the extra dependency mandatory."
  :depends-on (:hu.dwim.logger
               :hu.dwim.reiterate))

(defmethod perform :before ((op operation) (system (eql (find-system :hu.dwim.reiterate+hu.dwim.logger))))
  (load-system :hu.dwim.logger))

(defmethod perform :before ((op operation) (system (eql (find-system :hu.dwim.reiterate))))
  (load-system :hu.dwim.logger))

(defmethod perform :before ((op operation) (system (eql (find-system :hu.dwim.reiterate.test))))
  (load-system :hu.dwim.logger))
