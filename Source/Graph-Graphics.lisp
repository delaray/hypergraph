(in-package :GOM)

(use-package :hypergraph)

;;;**************************************************************************
;;; GRAPH GRAPHICS
;;;**************************************************************************
;;;
;;; Classes
;;; -------
;;;
;;; GRAPH-VERTEX
;;; GRAPH-WINDOW
;;;
;;;
;;; Functions
;;; ---------
;;;
;;; MAKE-GRAPH-WINDOW
;;; INITIALIZE-GRAPH-WINDOW
;;;
;;; HANDLE-RESHAPE-WIDGE
;;; HANDLE-REFRESH-WIDGET
;;;
;;; DRAW-GRAPH
;;; DRAW-GRAPH-VERTICES
;;; DRAW-GRAPH-EDGES

;;;--------------------------------------------------------------------------
;;; GRAPH-VERTEX
;;;--------------------------------------------------------------------------

(defclass GRAPHICAL-VERTEX ()
  ((vertex :initarg :vertex
	  :initform nil
	  :accessor object-vertex)
   (position :initarg :position
	     :initform nil
	     :accessor object-position)))

;;;--------------------------------------------------------------------------
;;; GRAPH-WINDOW
;;;--------------------------------------------------------------------------

(defclass GRAPH-WINDOW (GOM-WINDOW)
  ((graph :initarg :graph
	  :initform nil
	  :accessor object-graph)
   (vertices :initarg :vertics
	     :initform nil
	     :accessor object-vertices)))

;;;--------------------------------------------------------------------------
;;; MAKE-GRAPH-WINDOW
;;;--------------------------------------------------------------------------

(defmethod MAKE-GRAPH-WINDOW ((graph GRAPH))
  (let* ((window (make-window 'Graph :class 'graph-window)))
    (setf (object-graph window) graph)
    (initialize-graph-window window)
    window))
      
;;;--------------------------------------------------------------------------
;;; INITIALIZE-GRAPH-WINDOW
;;;--------------------------------------------------------------------------

;;; TODO: Need to modify initialize-graph-window to update graphical
;;; vertex positioms rather than regenerate the graphical vertices for
;;; efficiency.

(defmethod INITIALIZE-GRAPH-WINDOW ((window GRAPH-WINDOW))
  (when (object-graph window)
    (let* ((graph (object-graph window))
	   (vertex-count (hash-table-count (graph::graph-vertices graph)))
	   (x0 (widget-x-center window))
	   (y0 (widget-y-center window))
	   (radius (round (* 3 (floor (min x0 y0) 4))))
	   (points (util::compute-points-on-circle vertex-count x0 y0 radius)))
      (setf (object-vertices window) nil)
      (graph::map-graph-vertices
       graph
       #'(lambda (vertex)
	   (push (make-instance 'graphical-vertex 
		   :vertex vertex
		   :position (pop points))
		 (object-vertices window))))
      (refresh-widget window))))
  
;;;--------------------------------------------------------------------------
;;; HANDLE-RESHAPE-WIDGET
;;;--------------------------------------------------------------------------

;;; TODO: Need to modify initialize-graph-window to update graphical
;;; vertex positioms rather than regenerate the graphical vertices for
;;; efficiency.

(defmethod HANDLE-RESHAPE-WIDGET :after ((window GRAPH-WINDOW))
  (initialize-graph-window window))

;;;--------------------------------------------------------------------------
;;; HANDLE-REFRESH-WIDGET
;;;--------------------------------------------------------------------------

(defmethod HANDLE-REFRESH-WIDGET :after ((window GRAPH-WINDOW))
   (clear-widget window)
   (draw-graph window))
 
;;;--------------------------------------------------------------------------
;;; DRAW-GRAPH
;;;--------------------------------------------------------------------------

(defmethod DRAW-GRAPH ((window GRAPH-WINDOW))
  ;; Draw Outbound Edges of each vertex
  (draw-graph-edges window)
  ;; Draw Vertices
  (draw-graph-vertices window))

;;;--------------------------------------------------------------------------
;;; DRAW-GRAPH-VERTICES
;;;--------------------------------------------------------------------------

(defmethod DRAW-GRAPH-VERTICES ((window GRAPH-WINDOW))
  (let* ((vertices (object-vertices window)))
    (dolist (g-vertex vertices)
      (let* ((point (object-position g-vertex))
	     (vertex (object-vertex g-vertex))
	     (name (vertex-name vertex)))
	;; Clear the space occupied by the edge tips
	(draw-gom-circle window (first point) (second point) 15
			 :color (widget-background-color window)
			 :fill t)
	;; Draw a container circle for the vertex
	(draw-gom-circle window (first point) (second point) 15
			 :color cg::blue)
	;; Draw the vertex name inside the circle. Note: We my wish to
	;; draw a rectangle for long names
	(draw-gom-string  window
			  (vertex-name vertex)
			  ;; String x position
			  (round (- (first point)
				    (/ (string-width window name)
				       2)))
			  ;; String y positrion
			  (round (- (second point)
				    (/ (string-height window name)
				       2))))))))

;;;--------------------------------------------------------------------------
;;; DRAW-GRAPH-EDGES
;;;--------------------------------------------------------------------------

(defmethod DRAW-GRAPH-EDGES ((window GRAPH-WINDOW))
  (let* ((vertices (object-vertices window))
	 (graph (object-graph window))
	 (incr 10))
    (dolist (g-vertex vertices)
      (let* ((p1 (object-position g-vertex))
	     (x1 (first p1))
	     (y1 (second p1))
	     (edges (utilities::find-outbound-edges (object-vertex g-vertex)
						    'util::connects
						    graph)))
	;; (format t "~%Edges: ~a" edges)
	(dolist (edge edges)
	  (let* ((t-vertex (edge-target edge))
		 (p2 (object-position
		      (find t-vertex vertices :key #'object-vertex :test #'eq)))
		 (x2 (first p2))
		 (y2 (second p2)))
	    (draw-gom-line window x1 y1 x2 y2)
	    ;; Now draw the arrow at the midpoint of the edge
	    (let* ((midpoint (util::compute-midpoint x1 y1 x2 y2))
		   (x (first midpoint))
		   (y (second midpoint)))
	      (multiple-value-bind (x3 y3 x4 y4 a1 b1)
		  (util::compute-orthogonal-segment x1 y1 x2 y2 x y 4)
		(let* ((x5  (if (>= a1 most-positive-fixnum) x (+ x incr)))
		       (y5 (if (>= a1 most-positive-fixnum) (+ y incr)
			     (+ (* a1 x5) b1))))
		  ;;(format t "~%X3=~a , Y3=~a, X4=~a, Y4=~a" x3 y3 x4 y4)
		  (draw-edge-arrow window x3 y3 x4 y4 x5 y5 cg::red))))))))))

;;;--------------------------------------------------------------------------
;;; DRAW-EDGE-ARROW
;;;--------------------------------------------------------------------------

(defun DRAW-EDGE-ARROW (window x3 y3 x4 y4 x5 y5 &optional (color cg::red))
  (draw-gom-line window x3 y3 x4 y4 :color color)
  (draw-gom-line window x3 y3 x5 y5 :color color)
  (draw-gom-line window x4 y4 x5 y5 :color color))
  
;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------