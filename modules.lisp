(in-package :module-manager)

;;; ===========================================================================
;;;
;;; Author: Raymond de Lacaze
;;;
;;; This file defines the GRAPH module
;;;
;;; ===========================================================================

;;;----------------------------------------------------------------------------
;;; Graph Package
;;;----------------------------------------------------------------------------

(eval-when (eval compile load)
  (unless (find-package :hypergraph)
    (make-package "HYPERGRAPH"
		  :use '(#-SBCL :user #+SBCL :cl-user
			 :common-lisp
			 #-SBCL :clos
			 #-SBCL :excl)
		  :nicknames '(:graph))))


(define-root-directory '(:graph-root) *load-truename*)

;;;----------------------------------------------------------------------------
;;; Global Graph Pathname Related Variables
;;;----------------------------------------------------------------------------

(defparameter *GRAPH-DEVICE*
    (pathname-device *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *GRAPH-DIRECTORY*
   (pathname-directory *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *GRAPH-ROOT*
    (make-pathname :name nil :type nil
                   :directory  (pathname-directory *load-truename*)
                   :defaults  *load-truename*))


;;;----------------------------------------------------------------------------
;;; Graph
;;;----------------------------------------------------------------------------

(define-module :Graph
  (:directory :graph-root)
  (:requires :utilities)
  (:files "Graph"
	  "Graph-Operations"
	  "Graph-Traversals"
	  "Graph-Test"))

;;;----------------------------------------------------------------------------
;;; Graph
;;;----------------------------------------------------------------------------

(define-module :Persistent-Graph
  (:directory :graph-root)
  (:requires :graph)
  (:files "Graph-Persistence-Postgres"))

;;;----------------------------------------------------------------------------
;;; Semantic-Net
;;;----------------------------------------------------------------------------

(define-module :Semantic-Net
  (:directory :graph-root)
  (:requires :graph)
  (:files "Semantic-Net"
	  "Semantic-Net-Relations"
	  "Semantic-Net-Tests"))

;;;----------------------------------------------------------------------------
;;; Graph-Graphics
;;;----------------------------------------------------------------------------

(define-module :Graph-Graphics
  (:directory :graph-root)
  (:requires :gom :graph)
  (:files "Graph-Graphics"))


;;;-------------------------------------------------------------------------
;;; End of File
;;;-------------------------------------------------------------------------
