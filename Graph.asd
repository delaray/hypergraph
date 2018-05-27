;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(defpackage :hypergraph (:use #:asdf #:cl) (:nicknames "GRAPH"))

;;;----------------------------------------------------------------------------
;;; Graph Module System Definition
;;;----------------------------------------------------------------------------

(in-package :hypergraph)

(defsystem graph
  :author "Raymond de Lacaze <delaray@hotmail.com>"
  :version "1.0"
  :maintainer  "Raymond de Lacaze <delaray@hotmail.com>"
  :licence "MIT Style license for the packaging."
  :description "Hypergraph and Semantic Net Module"
  :long-description "Hypergraph and Semantic Net Module"
  :depends-on (:utilities)
  :components ((:module "Source"
                        :components ((:file "Graph")
				     (:file "Graph-Operations")
				     (:file "Graph-Traversals")
				     (:file "Graph-Test")
				     (:file "Semantic-Net")
				     (:file "Semantic-Net-Relations")))))
					    
#+IGNORE
(asdf:oos 'asdf:load-op :graph)

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
