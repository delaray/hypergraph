(in-package :common-lisp-user)

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                          GOM REPL Commands
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;;  Preparations:
;;;    1. Create gbbopen-modules directory in your (user-homedir-pathname) 
;;;       directory
;;;    2. Create a symbolic link to this Meperia source tree in this
;;;       gbbopen-modules directory (Windows users must create a 
;;;       "pseudo-symbolic-link" file, a text file of type .sym that contains 
;;;       the target directory path as the sole line in the file)
;;;
;;;  Then load GBBopen's initiate.lisp from whatever GBBopen installation is
;;;  desired (which will load this command file).
;;;
;;;  Recommended: Set up your CL environment according to the "Enhancing Your
;;;               Development Environment" exercise in the GBBopen Tutorial.
;;;
;;;----------------------------------------------------------------------------

;; Handle outdated versions of GBBopen, remove once everyone is running at
;; least GBBopen Version 1.0:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (macro-function 'define-repl-command)
    (warn "Running on an old version of GBBopen Tools (adding ~s)"
          'define-repl-command)
    (defmacro define-repl-command (command lambda-list &rest body)
      `(define-tll-command ,command ,lambda-list ,@body)))
  (unless (macro-function 'with-system-name)
    (warn "Running on an old version of GBBopen Tools (adding ~s)"
          'with-system-name)
    (defmacro with-system-name ((&optional system-name) &body body)
      `(progn ,@body))))

;;; ===========================================================================
;;;   Supporting Entities for Meperia Commands

;; Remember this file:

(defparameter Graph-commands-file* *load-truename*)

;;; ===========================================================================
;;;   Useful PQ Commands

(with-system-name (:graph)
  
 ;;; ---------------------------------------------------------------------------

  (define-repl-command :Graph (&rest options)
    "Load (compile if needed) :graph module with directive as specified by options."
    (format t "~&;; ***** Compiling/Loading Graph~%")
     ;; Load/compile code.
    (startup-module :graph options :graph))
  
  ;;; ---------------------------------------------------------------------------

  (define-repl-command :Persistent-Graph (&rest options)
    "Load (compile if needed) :parser module with directive as specified by options."
    (format t "~&;; ***** Compiling/Loading Persistent-Graph~%")
    ;; Load/compile code.
    (startup-module :persistent-graph options :graph))
  
  ;;; ---------------------------------------------------------------------------

  (define-repl-command :Semantic-Net (&rest options)
    "Load (compile if needed) :parser module with directive as specified by options."
    (format t "~&;; ***** Compiling/Loading Semantic-Net~%")
    ;; Load/compile code.
    (startup-module :semantic-net options :graph))
  
   ;;; ---------------------------------------------------------------------------
  
  (define-repl-command :Graph-Graphics (&rest options)
    "Load (compile if needed) :graph-graphics module with directive as specified by options."
    (format t "~&;; ***** Compiling/Loading Graph~%")
    ;; Load/compile code.
    (startup-module :graph-graphics options :gom))
  )

;;;------------------------------------------------------------------------
;;; End of File
;;;------------------------------------------------------------------------
