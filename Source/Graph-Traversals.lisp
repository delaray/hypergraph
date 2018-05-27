(in-package :HYPERGRAPH)

;;;-----------------------------------------------------------------------------
;;; Floyd-Warshall Algorithm
;;;-----------------------------------------------------------------------------

(defmethod COMPUTE-ALL-PAIRS-SHORTEST-PATH ((graph GRAPH) &optional (edge-type 'relation))
  (let* ((vertex-count (count-graph-vertices graph))
	(vertex-indices (make-hash-table :size vertex-count))
	(index-vertices (make-hash-table :size vertex-count))
	(cost-matrix (make-array `(,vertex-count ,vertex-count)))
	(count 0))
    ;; Initialize vertex-index mappings
    (map-concepts #'(lambda (concept)
			    (setf (gethash concept vertex-indices) count)
			    (setf (gethash count index-vertices) concept)
			    (incf count))
			:graph graph)
    ;; Initialize cost matrix
    (dotimes (i vertex-count)
      (dotimes (j vertex-count)
	(let* ((v1 (gethash i index-vertices))
	       (v2 (gethash j index-vertices))
	       (edge (find-relation v1 edge-type v2 :graph graph)))
	  (setf (aref cost-matrix i j)
	    (if edge (relation-weight edge) nil)))))
    ;; Run Floyd-Warshall algorithm
    (dotimes (k vertex-count)
      (print k)
      (dotimes (i vertex-count)
	(dotimes (j vertex-count)
	  (unless (or (null (aref cost-matrix i k))
		      (null (aref cost-matrix k j))
		      (null (aref cost-matrix i j)))
	    (when (< (+ (aref cost-matrix i k)(aref cost-matrix k j))
		     (aref cost-matrix i j))
	      (setf  (aref cost-matrix i j)
		(+ (aref cost-matrix i k)(aref cost-matrix k j))))))))
    cost-matrix))


;;;***************************************************************************
;;; GRAPH TRAVERSALS
;;;***************************************************************************

;;;---------------------------------------------------------------------------
;;; FLOYD
;;;---------------------------------------------------------------------------

;;; Returns shortest paths and costs matrices.

(defmethod FLOYD ((graph GRAPH) &key (edge-type *default-edge-type*))
  (let* ((vertex-count (hash-table-count (graph-vertices graph)))
	 (vertices (make-array `(,vertex-count)))
	 (A (make-array `(,vertex-count ,vertex-count)))
	 (paths (make-array `(,vertex-count ,vertex-count)))
	 (count 0))
    ;; Initialize array of vertices
    (map-graph-vertices 
     graph
     #'(lambda (vertex)
	 (setf (aref vertices count) vertex)
	 (incf count)))
    
    ;; Initialize Matrix A
    (dotimes (i vertex-count)
      (dotimes (j vertex-count)
	(let ((edge (find-graph-edge (aref vertices i)
				    edge-type
				     (aref vertices j)
				     graph)))
	  (if edge
	    (setf (aref A i j)(edge-weight edge))
	    (setf (aref A i j) most-positive-fixnum))
	  (setf (aref paths i j) 0))))
    (dotimes (i vertex-count)
      (setf (aref A i i) 0))
    
    ;; Run FLOYD
    (dotimes (k vertex-count)
      (dotimes (i vertex-count)
	(dotimes (j vertex-count)
	  (when (< (+ (aref A i k)(aref A k j))
		   (aref A i j))
	    (setf (aref A i j)(+ (aref A i k)(aref A k j)))
	    (setf (aref paths i j) k)))))
    (values A paths vertices)))

;;;-----------------------------------------------------------------------------

(defun PRINT-MIN-PATH-COSTS (vertices A)
  (let ((n (first (array-dimensions vertices))))
    (format t "~%   ")
    (dotimes (i n)
      (format t "~a  " (vertex-name (aref vertices i))))
    (dotimes (i n)
      (format t "~%~a  " (vertex-name (aref vertices i)))
      (dotimes (j n)
	(let ((cost  (aref A i j)))
	  (when (= cost most-positive-fixnum)
	    (setf cost "N"))
	  (format t "~a  " cost))))))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
