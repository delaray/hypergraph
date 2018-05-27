(in-package :HYPERGRAPH)

;;;*****************************************************************************
;;; Graph Operations
;;;
;;; This file contains advanced graph operations that go beyond the
;;; typical graph AI functions
;;;
;;;*****************************************************************************
;;;
;;; File Contents
;;; -------------
;;;
;;; REDUCE-GRAPH-VETICES
;;; REDUCE-GRAPH
;;; COPY-GRAPH
;;;
;;;*****************************************************************************

;;;-----------------------------------------------------------------------------
;;; REDUCE-GRAPH-VERTICES
;;;-----------------------------------------------------------------------------

;;; Deletes all vertices (and associated edges) that satisfy the reduction predicate.

(defmethod REDUCE-GRAPH-VERTICES ((graph GRAPH) reduction-predicate)
  (let ((vertices-to-delete nil))
    (map-concepts 
     #'(lambda (concept)
	 (when (funcall reduction-predicate concept t)
	   (push concept vertices-to-delete)))
     :graph graph)
    (mapc #'(lambda (x)(delete-graph-vertex x graph)) vertices-to-delete)
    graph))

;;;-----------------------------------------------------------------------------
;;; REDUCE-GRAPH
;;;-----------------------------------------------------------------------------

;;; Replaces A --> B ---> C with A --> C if B satifies reduction predicate.

(defmethod REDUCE-GRAPH ((graph GRAPH) reduction-predicate 
			 &optional
			 (edge-type 'relation))
  (let ((vertices-to-delete nil)
	(count 0))
    (map-concepts 
     #'(lambda (concept)
	 (when (funcall reduction-predicate concept t)
	   (let* ((in-links (find-inbound-edges concept edge-type graph))
		  (out-links (find-outbound-edges concept edge-type graph)))
	     (incf count)
	     (format t "~%Count = ~a" count)
	     (push concept vertices-to-delete)
	     (mapc #'(lambda (x)(delete-graph-edge x graph)) in-links)
	     (mapc #'(lambda (x)(delete-graph-edge x graph)) out-links)
	     (dolist (in-link in-links)
	       (dolist (out-link out-links)
		 (let ((v1 (edge-source in-link))
		       (v2 (edge-target out-link)))
		   (unless (or (eq v1 v2)
			       (find-graph-edge v1 edge-type v2 graph))
		     (make-graph-edge edge-type v1 v2 graph
				      :weight (+ (edge-weight in-link)
						 (edge-weight out-link))
				      :class (class-name (class-of in-link))))))))))
     :graph graph)
    (mapc #'(lambda (x)(delete-graph-vertex x graph)) vertices-to-delete)
    graph))

;;;-----------------------------------------------------------------------------
;;; COPY-GRAPH
;;;-----------------------------------------------------------------------------

(defmethod COPY-GRAPH ((graph GRAPH)
		       &optional
		       (name (format nil "~a-copy" (object-name graph)))
		       (vertex-predicate #'identity)
		       (edge-predicate #'identity))
  (let ((new-graph (make-instance (class-name (class-of graph)) :name name)))
    (map-graph-vertices 
     graph
     #'(lambda (vertex)
	 (when (funcall vertex-predicate vertex)
	   (ensure-graph-vertex (vertex-name vertex) new-graph))))
    (map-graph-edges 
     graph
     #'(lambda (edge)
	 (print edge)
	 (print (class-of edge))
	 (break)
	 (when (and (funcall vertex-predicate (edge-source edge))
		    (funcall vertex-predicate (edge-target edge))
		    (funcall edge-predicate edge))
	   (ensure-graph-edge (edge-type edge)
			      (edge-source edge)
			      (edge-target edge)
			      new-graph))))
    new-graph))
 
;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
