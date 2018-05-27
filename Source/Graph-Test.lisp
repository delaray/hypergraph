(in-package :HYPERGRAPH)

;;;************************************************************************
;;;
;;; HYPER GRAPH API TESTING
;;;
;;;************************************************************************

;;;-----------------------------------------------------------------------
;;; MAKE-SAMPLE-GRAPH
;;;-----------------------------------------------------------------------

(defun MAKE-SAMPLE-GRAPH ()
  (let* ((g (make-graph))
	 (n1 (make-graph-vertex "v1" g :class 'graph-vertex))
	 (n2 (make-graph-vertex "v2" g :class 'graph-vertex))
	 (n3 (make-graph-vertex "v3" g :class 'graph-vertex))
	 (n4 (make-graph-vertex "v4" g :class 'graph-vertex))
	 (n5 (make-graph-vertex "v5" g :class 'graph-vertex))
	 (n6 (make-graph-vertex "v6" g :class 'graph-vertex))
	 (e1 (make-graph-edge 'connects n1 n2 g :class 'graph-edge :weight 2))
	 (e2 (make-graph-edge 'connects n1 n3 g :class 'graph-edge :weight 5))
	 (e3 (make-graph-edge 'connects n2 n4 g :class 'graph-edge :weight 1))
	 (e4 (make-graph-edge 'connects n2 n5 g :class 'graph-edge :weight 3))
	 (e5 (make-graph-edge 'connects n3 n6 g :class 'graph-edge :weight 10))
	 (e6 (make-graph-edge 'connects n4 n1 g :class 'graph-edge :weight 20))
	 (e7 (make-graph-edge 'connects n5 n1 g :class 'graph-edge :weight 20)))
    (values g
	    (list n1 n2 n3 n4 n5 n6)
	    (list e1 e2 e3 e4 e5 e6 e7))))

;;;-----------------------------------------------------------------------
;;; TEST-GRAPH
;;;-----------------------------------------------------------------------

 (defun TEST-GRAPH ()
  (declare (special *g*))
  (let ((g (make-graph)))
    (setf *g* g)
    (let* ((n1 (make-graph-vertex "vertex-1" g :class 'graph-vertex))
           (n2 (make-graph-vertex "vertex-2" g :class 'graph-vertex))
           (n3 (make-graph-vertex "vertex-3" g :class 'graph-vertex))
           (n4 (make-graph-vertex "vertex-4" g :class 'graph-vertex))
           (n5 (make-graph-vertex "vertex-5" g :class 'graph-vertex))
           (n6 (make-graph-vertex "vertex-6" g :class 'graph-vertex)) 
           (e1 (make-graph-edge 'link1 n1 n2 g :class 'graph-edge))
           (e2 (make-graph-edge 'link2 n2 n1 g :class 'graph-edge)))
      
      (setf (vertex-weight n1) 10.0)
      (setf (vertex-weight n2) 10.0)
      
      (util::test-function 'vertex-name '(vertex-name n1) (vertex-name n1) t)
      (util::test-function 'vertex-name '(vertex-name e1) (vertex-name e1))
      
      (util::test-function 'vertex-weight '(vertex-weight n1) (vertex-weight n1))

      (util::test-function 'find-graph-vertex
		     '(find-graph-vertex "vertex-1" g)
		     (find-graph-vertex "vertex-1"  g)
		     t)
      
      (util::test-function 'edge-name '(edge-name e1) (edge-name e1) t)
      (util::test-function 'edge-name '(edge-name e1) (edge-name e1))
      
      (util::test-function 'edge-weight '(edge-weight e1) (edge-weight e1) t)
      (setf (edge-weight e1) 120)  
      (util::test-function 'edge-weight '(edge-weight e1) (edge-weight e1))

        
      (util::print-testing-intro 'add-grah-edge)
      (format t "~%Result = ~a" (make-graph-edge 'next n1 n2 g :class 'graph-edge))
      (format t "~%Result = ~a" (make-graph-edge 'next n2 n3 g :class 'graph-edge))
      (format t "~%Result = ~a" (make-graph-edge 'next n3 n4 g :class 'graph-edge))
      (format t "~%Result = ~a" (make-graph-edge 'next n4 n5 g :class 'graph-edge))
      (format t "~%Result = ~a" (make-graph-edge 'next n5 n6 g :class 'graph-edge))
         
      (util::print-testing-intro 'find-edge-types)
      (print (find-edges-of-type 'link1 g))
      (print (find-edges-of-type 'link2 g))
      (print (find-edges-of-type 'next  g))

      (util::print-testing-intro 'edge-object)
      (print (edge-source e1))
      (print (edge-target e2))
			
      (util::print-testing-intro 'find-graph-edge)
      (print (find-graph-edge n1 'link1 n2 g))
      (print (find-graph-edge n2 'link2 n1 g))
      (print (find-graph-edge n1 'link1 n3 g))

      (util::print-testing-intro 'find-edge-types)
      (print (find-outbound-edges n1 'link1 g))
      (print (find-outbound-edges n1 'next g))
      (print (find-outbound-edges n1 'before g))
      (print (find-outbound-edges n1 'wrong g))
      (print (find-outbound-edges n1 'link2 g))

      (util::print-testing-intro 'find-outbound-vertices)
      (print (find-outbound-vertices n1 'link1 g))
      (print (find-outbound-vertices n1 'link2 g))
      (format t "~%~%")
      )
    'done))

;;;-----------------------------------------------------------------------------
;;; Performance Tests
;;;-----------------------------------------------------------------------------

;;; Generate a complete graph with n vertices

(defun MAKE-COMPLETE-GRAPH (&optional (n 100))
   (declare (special *g*))
   (let ((g (make-graph))
	 (count 0))
     (setf *g* g)
     ;; Generate n vertices
     (dotimes (i n)
       (make-graph-vertex (format nil "vertex-~a" i) g :class 'graph-vertex))
     ;; Add an edge between every distinct pair of vertices
     (map-graph-vertices
      g
      #'(lambda (v1)
	  ;;(format t "~%~a Vertices processed...." count)
	  (map-graph-vertices
	   g
	   #'(lambda (v2)
	       (unless (eq v1 v2)
		 (make-graph-edge 'connects v1 v2 g :class 'graph-edge))))
	  (incf count)))
     ;; Return the graph
     g))

;;;-----------------------------------------------------------------------------

(defun TEST-VERTEX-RETRIEVAL (graph)
  (map-graph-vertices graph
		      #'(lambda (vertex)
			  (find-graph-vertex (vertex-name vertex) graph)))
  graph)
 
;;;-----------------------------------------------------------------------------

(defun TEST-EDGE-RETRIEVAL (graph)
  (map-graph-edges graph
		      #'(lambda (edge)
			  (find-graph-edge (edge-source edge)
					   (edge-type edge)
					   (edge-target edge)
					    graph)))
  graph)
 
;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
