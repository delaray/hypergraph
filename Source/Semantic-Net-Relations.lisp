(in-package :HYPERGRAPH)

;;;****************************************************************************
;;; SEMANTIC NET RELATIONS
;;;****************************************************************************

;;;-----------------------------------------------------------------------------
;;; FIND-TRIANGULAR-CONCEPTS
;;;-----------------------------------------------------------------------------

;;; This returns a list of vertex pairs that are b-directionally
;;; related to each other. This currently employs an n-cubed
;;; algorithm to compute these vertices. This should be improved upon.

;;; Note: This has been improved by introducing cutoffs. Still need to
;;; assess improved complexity. Once a node in the outermost iteration
;;; is processed is does need to be considered in the inner two
;;; iterations.

(defmethod FIND-TRIANGULAR-CONCEPTS ((graph GRAPH))
  (let ((bdr nil)
	(temp-hash (make-hash-table :size 100 :test #'eq))
	(count 0))
    (map-concepts
     #'(lambda (c1)
	 (incf count)
	 (format t "~%~a outer concepts processed." count)
	 ;; Mark this node as processed by placing in an HT.
	 (setf (gethash c1 temp-hash) t)
	 (map-concepts
	  #'(lambda (c2)
	      ;; Skip previously processed nodes.
	      (unless (gethash c2 temp-hash)
		(map-concepts
		 #'(lambda (c3)
		     ;; Skip previously processed nodes.
		     (unless (gethash c3 temp-hash)
		       (let* ((concept-list (list c1 c2 c3)))
			 (when (and (find-relation c1 'related-to c2 :graph graph)
				    (find-relation c2 'related-to c3 :graph graph)
				    (find-relation c3 'related-to c1 :graph graph))
			   (push concept-list bdr)))))
	       :graph graph)))
	      :graph graph))
	 :graph graph)
     bdr))

;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
