(in-package :HYPERGRAPH)

;;;************************************************************************
;;;
;;; DIRECTED, LABELED & WEIGHTED HYPER GRAPH API
;;;
;;;************************************************************************
;;;
;;; Variables
;;; ---------
;;;
;;; *DEFAULT-VERTEX-WEIGHT*
;;; *DEFAULT-EDGE-WEIGHT*
;;; *DEFAULT-EDGE-TYPE*
;;;
;;;
;;; Classes
;;; -------
;;;
;;; GRAPH
;;; GRAPH-VERTEX
;;; GRAPH-EDGE
;;;
;;; GRAPH-PATH
;;;
;;; EDGE-TABLES
;;;
;;;
;;; Graph Functions
;;; ---------------
;;;
;;; MAKE-GRAPH
;;; FIND-GRAPH
;;; ENSURE-GRAPH
;;; DELETE-GRAPH
;;; CLEAR-GRAPH
;;; PRINT-GRAPH
;;;
;;; MAP-GRAPH-VERTICES
;;; MAP-GRAPH-EDGES
;;;
;;; COUNT-GRAPH-VERTICES
;;; COUNT-GRAPH-EDGES
;;;
;;;
;;; Vertex Functions
;;; ----------------
;;;
;;; MAKE-GRAPH-VERTEX
;;; FIND-GRAPH-VERTEX
;;; ENSURE-GRAPH-VERTEX
;;; DELETE-GRAPH-VERTEX
;;;
;;; FIND-OUTBOUND-VERTICES
;;; FIND-INBOUND-VERTICES
;;;
;;;
;;; Edge Functions
;;; --------------
;;;
;;; MAKE-GRAPH-EDGE
;;; MAKE-DEFAULT-EDGE-NAME
;;; FIND-GRAPH-EDGE
;;; ENSURE-GRAPH-EDGE
;;; DELETE-GRAPH-EDGE
;;;
;;; FIND-OUTBOUND-EDGES
;;; FIND-INBOUND-EDGES
;;;
;;; MAP-OUTBOUND-EDGES
;;; MAP-INBOUND-EDGES
;;;
;;; FIND-EDGES-OF-TYPE
;;;
;;; EDGE-NAME
;;; EDGE-WEIGHT
;;;
;;;
;;; Edge Type Functions
;;; -------------------
;;;
;;; GRAPH-EDGE-TYPES
;;;
;;;
;;; Edge Tables
;;; -----------
;;;
;;; MAKE-EDGE-TABLES
;;; FIND-EDGE-TABLES
;;; DELETE-EDGE-TABLES
;;; EMPTY-EDGE-TABLES-P
;;;
;;;
;;; SAVE & RESTORE
;;; --------------
;;;
;;; SAVE-GRAPH
;;; RESTORE-GRAPH
;;;
;;;
;;; MISCELLANEOUS
;;; -------------
;;;
;;; MAKE-GRAPH-PATH
;;; COMPUTE-PATH-COST
;;; COUNT-NODES-IN-SUBGRAPH
;;; MAP-NODES-IN-SUBGRAPH
;;; PRETTY-PRINT-SUBGRAPH
;;;
;;;
;;;************************************************************************

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(;; Graph Classes
	    GRAPH GRAPH-VERTEX GRAPH-EDGE
	    ;; Graph Functions
	    MAKE-GRAPH FIND-GRAPH DELETE-GRAPH PRINT-GRAPH
	    MAP-GRAPH-VERTICES MAP-GRAPH-EDGES
	    ;; Vertex Functions
	    VERTEX-NAME VERTEX-WEIGHT
	    MAKE-GRAPH-VERTEX ENSURE-GRAPH-VERTEX DELETE-GRAPH-VERTEX
	    FIND-GRAPH-VERTEX FIND-INBOUND-VERTICES FIND-OUTBOUND-VERTICES
	    ;; Edge Functions
	    EDGE-NAME EDGE-SOURCE EDGE-TARGET EDGE-WEIGHT
	    MAKE-GRAPH-EDGE ENSURE-GRAPH-EDGE DELETE-GRAPH-EDGE
	    FIND-GRAPH-EDGE FIND-OUTBOUND-EDGES FIND-INBOUND-EDGES
	    FIND-EDGES-OF-TYPE)))

;;;-------------------------------------------------------------------------
;;; *ALL-GRAPHS*
;;;-------------------------------------------------------------------------

(defvar *ALL-GRAPHS* (make-hash-table :test #'equalp :size 10))

;;;-------------------------------------------------------------------------
;;; Default Values
;;;-------------------------------------------------------------------------

(defvar *DEFAULT-VERTEX-WEIGHT* 0)

;;;-------------------------------------------------------------------------

(defvar *DEFAULT-EDGE-WEIGHT* 1)

;;;-------------------------------------------------------------------------

(defvar *DEFAULT-EDGE-TYPE* 'connects)

;;;-------------------------------------------------------------------------
;;; GRAPH
;;;-------------------------------------------------------------------------

(defclass GRAPH ()
  ((name :initarg :name
	 :initform nil
	 :accessor graph-name
	 :accessor object-name)
   (vertices :initarg :vertex 
          :initform (make-hash-table :test #'equalp :size 100) 
          :accessor graph-vertices)
   (edges :initarg :edges
                 :initform (make-hash-table :test #'eq :size 20) 
                 :accessor graph-edges)
   (edge-types :initarg :edge-types
	       :initform nil
	       :accessor graph-edge-types)))

;;;-------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((graph GRAPH) stream)
  (cond (*print-readably* (call-next-method))
        (t 
	 (format stream "#<Graph ~a: ~d vertices, ~d edges, ~d edge type(s)>"
		 (or (graph-name graph) "Unnamed")
		 (count-graph-vertices graph)
		 (count-graph-edges graph)
		 (length (graph-edge-types graph))))))

;;;---------------------------------------------------------------------------
;;; GRAPH-VERTEX-MIXIN
;;;---------------------------------------------------------------------------

(defclass GRAPH-VERTEX-MIXIN ()
  ((name :reader vertex-name
	 :initarg :name
	 :initform nil)
   (weight :initarg :weight
	   :type fixnum 
           :accessor vertex-weight)
   #+IGNORE
   (graph :initarg :graph
	  :accessor vertex-graph
	  :accessor object-graph)))

;;;---------------------------------------------------------------------------
;;; GRAPH-VERTEX
;;;---------------------------------------------------------------------------

(defclass GRAPH-VERTEX (GRAPH-VERTEX-MIXIN)
  ())

;;;------------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((vertex GRAPH-VERTEX) stream)
  (cond (*print-readably* (call-next-method))
        (t
	 (format stream  "#<Vertex(~a): ~a>"
		 (vertex-weight vertex)
		 (vertex-name vertex)))))

;;;---------------------------------------------------------------------------
;;; GRAPH-EDGE
;;;---------------------------------------------------------------------------

(defclass GRAPH-EDGE (GRAPH-VERTEX)
  (;; Add additional readers/accessors to inherited slots
   (name :reader edge-name)
   (weight :accessor edge-weight)
   #+IGNORE
   (graph  :accessor edge-graph)
   ;; New slots
   (type :initarg :type
	 :initform 'CONNECTS
	 :accessor edge-type)
   (source :initarg :source
           :accessor edge-source)
   (target :initarg :target
           :accessor edge-target)))

;;;---------------------------------------------------------------------------

(defmethod EDGE-SOURCE-NAME ((edge GRAPH-EDGE))
  (vertex-name (edge-source edge)))

;;;---------------------------------------------------------------------------

(defmethod EDGE-TARGET-NAME ((edge GRAPH-EDGE))
  (vertex-name (edge-target edge)))

;;;---------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((edge GRAPH-EDGE) stream)
  (cond (*print-readably* (call-next-method))
        (t
	 (format stream "#<EDGE(~d): Type: ~a From: ~a To: ~a>"
		 (edge-weight edge)
		 (edge-type edge)
		 (edge-source-name edge)
		 (edge-target-name edge)))))

;;;---------------------------------------------------------------------------
;;; EDGE-TABLES
;;;---------------------------------------------------------------------------

;;; Each EDGE TYPE has an associated EDGE-TABLES object. This in turn
;;; comprise 3 different hash tables. The outbound-edges are the outbound
;;; edges of each vertex. The inbound-edges are the inbound edges of
;;; each vertex. The links table contains an entry for each pair of
;;; vertices connected by one or more edge types.

(defclass EDGE-TABLES ()
  ((graph :initarg :graph 
	  :accessor graph)
   ;; NOTE: Need to test whether dtpr-eq actually buys us anything.
   ;; If so, do other Lisps allow hashtable tests other than eq, eql,
   ;; equal, and equalp??
   (links :initarg :links
          :initform (make-hash-table :test #'eq)
	  :accessor links
          :documentation 
          "Hash table keys are <source-vertex> the value of which are hash keyed by
           <target-vertex> with the edge as value.")
   (sources :initarg :sources 
            :initform (make-hash-table :test 'eq) 
            :accessor outbound-edges-table
            :documentation
            "Keys are vertices, values are links for which vertex is source.")
   (targets :initarg :targets 
            :initform (make-hash-table :test 'eq) 
            :accessor inbound-edges-table
            :documentation
            "Keys are vertices, values are links for which vertex is target.")))

;;;---------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((edge-tables EDGE-TABLES) stream)
  (cond (*print-readably* (call-next-method))
        (t
	 (format stream "#<Edge-Tables: Edges: ~a, Outbound: ~a, Inbound: ~a>"
		 (count-graph-edges (graph edge-tables))
		 (hash-table-count (outbound-edges-table edge-tables)) 
		 (hash-table-count (inbound-edges-table edge-tableS))))))
			      
;;;-------------------------------------------------------------------------
;;; GRAPH-PATH
;;;-------------------------------------------------------------------------

;;; This represents a path through the graph. Both vertex paths ands
;;; edge paths are stored for convenience even though one is derivable
;;; from the other.

(defclass GRAPH-PATH ()
  ((vertex-path :initarg :vertex-path
		:initform nil
		:accessor graph-vertex-path)
   (edge-path :initarg :edges
                 :initform nil
                 :accessor graph-edge-path)
   (path-cost :initform 0
	      :initarg :path-cost
	      :accessor graph-path-cost)
   (graph :initform nil
	  :initarg :graph
	  :accessor graph-path-graph
	  :accessor object-graph)))

;;;-------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((graph-path GRAPH-PATH) stream)
  (cond (*print-readably* (call-next-method))
        (t 
	 (format stream "#<GRAPH-PATH: Length=~a, Cost=~a>"
		   (length (graph-edge-path graph-path))
		   (graph-path-cost graph-path)))))

;;;************************************************************************
;;; Generic Method Definitions
;;;************************************************************************

(defgeneric VERTEX-NAME (graph-vertex))

(defgeneric VERTEX-WEIGHT (graph-vertex))

(defgeneric EDGE-TYPE (edge))

(defgeneric EDGE-WEIGHT (edge))


;;;***********************************************************************
;;; GRAPH METHODS
;;;***********************************************************************

;;;-------------------------------------------------------------------------
;;; MAKE-GRAPH 
;;;-------------------------------------------------------------------------

(defun MAKE-GRAPH (&key (name nil)(class 'graph)(cache nil))
  (let ((graph (make-instance class :name name)))
    (when (and name cache)
      (setf (gethash name *all-graphs*) graph))
    graph))

;;;-------------------------------------------------------------------------

(defvar *DEFAULT-GRAPH* (make-graph :name "Default"))

;;;-------------------------------------------------------------------------
;;; FIND-GRAPH
;;;-------------------------------------------------------------------------

(defmethod FIND-GRAPH ((graph-name STRING)(location (eql :MEMORY)))
  (gethash graph-name *all-graphs*))

;;;-------------------------------------------------------------------------
;;; ENSURE-GRAPH
;;;-------------------------------------------------------------------------

(defmethod ENSURE-GRAPH ((graph-name STRING))
  (or (find-graph graph-name :memory)
      (restore-graph :lisp graph-name)
      (make-graph :name graph-name)))

;;;-------------------------------------------------------------------------
;;; DELETE-GRAPH
;;;-------------------------------------------------------------------------

(defmethod DELETE-GRAPH ((graph-name STRING))
  (remhash graph-name *all-graphs*))

;;;-------------------------------------------------------------------------

(defmethod DELETE-GRAPH ((graph GRAPH))
  (when (graph-name graph)
    (delete-graph (graph-name graph))))

;;;-------------------------------------------------------------------------
;;; CLEAR-GRAPH
;;;-------------------------------------------------------------------------

(defmethod CLEAR-GRAPH ((graph GRAPH))
  (clrhash (graph-vertices graph))
  (clrhash (graph-edges graph))
  graph)

;;;-------------------------------------------------------------------------
;;; PRINT-GRAPH
;;;-------------------------------------------------------------------------

;;; This prints out 3 things:
;;;
;;; 1. A list of all vertex names
;;; 2. A list of all edge types
;;; 3. A list of all edges, i.e. ordered vertex pairs

(defmethod PRINT-GRAPH ((graph GRAPH) &optional (stream t))
  (print graph stream)
  ;; Print all vertices
  (format stream "~%~%Vertices:")
  (map-graph-vertices graph
		      #'(lambda (vertex)
			  (format stream "~%~a" (vertex-name vertex))))
  ;; Print all edge types
  (format stream "~%~%Edge Types:")
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       (format stream "~%~a" key))
	   (graph-edges graph))
  ;; Print all edges
  (format stream "~%~%Edges:")
  (map-graph-edges graph
		   #'(lambda (edge)
 		       (format stream "~%~a --- ~a(~a) ---> ~a"
			       (vertex-name (edge-source edge))
			       (edge-type edge)
			       (edge-weight edge)
			       (vertex-name (edge-target edge)))))
  t)


;;;-----------------------------------------------------------------------------
;;; MAP-GRAPH-VERTICES
;;;-----------------------------------------------------------------------------

(defun MAP-GRAPH-VERTICES (graph function)
  (maphash #'(lambda (key value)
               (declare (ignore key))
               (funcall function value))
           (graph-vertices graph))
  t)

;;;-----------------------------------------------------------------------------
;;; MAP-GRAPH-EDGES
;;;-----------------------------------------------------------------------------

(defmethod MAP-GRAPH-EDGES ((graph GRAPH) function
			    &key
			    (edge-types nil))
  (maphash #'(lambda (edge-type edge-tables)
	       (when (or (not edge-types)
			 (member edge-type edge-types))
		 (maphash #'(lambda (source-vertex target-hash)
			      (declare (ignore source-vertex))
			      (maphash #'(lambda (target-vertex edge)
					   (declare (ignore target-vertex))
					   ;;(print edge)
					   (funcall function edge))
				       target-hash))
			  (links edge-tables))))
	   (graph-edges graph)))

;;;-----------------------------------------------------------------------------

(defmethod MAP-VERTEX-EDGES ((vertex GRAPH-VERTEX) function 
			     &key
			     (edge-types nil)
			     (graph *default-graph*))
  (maphash #'(lambda (edge-type edge-tables)
	       (when (or (not edge-types)
			 (member edge-type edge-types))
		 (let ((target-hash (gethash vertex (links edge-tables))))
		   (when target-hash
		     (maphash #'(lambda (target-vertex edge)
				  (declare (ignore target-vertex))
				  (funcall function edge))
			      target-hash)))))
	   (graph-edges graph)))

;;;-----------------------------------------------------------------------------
;;; COUNT-GRAPH-VERTICES
;;;-----------------------------------------------------------------------------

(defmethod COUNT-GRAPH-VERTICES ((graph GRAPH))
  (hash-table-count (graph-vertices graph)))
  
;;;-------------------------------------------------------------------------

(defmethod COUNT-GRAPH-EDGES ((graph GRAPH))
  (let* ((edge-tables (graph-edges graph))
	 (count 0))
    (maphash #'(lambda (edge-type edge-table)
		 (declare (ignore edge-type))
		 (maphash #'(lambda (source-vertex target-hash)
			      (declare (ignore source-vertex))
			      (incf count (hash-table-count target-hash)))
			  (links edge-table)))
	     edge-tables)
    count))

;;;***********************************************************************
;;; GRAPH-VERTEX METHODS
;;;***********************************************************************


;;;---------------------------------------------------------------------------
;;; MAKE-GRAPH-VERTEX
;;;---------------------------------------------------------------------------

(defgeneric MAKE-GRAPH-VERTEX (vertex-name graph &key class weight))

;;;-----------------------------------------------------------------------------

;;; We insist on VERTEX-NAMES for the graph level API to be
;;; STRINGS. The Concept Space API allows strings, symbols and
;;; numbers.

(defmethod MAKE-GRAPH-VERTEX ((vertex-name STRING)(graph GRAPH)
			      &key
			      (class 'graph-vertex)
			      (weight *default-vertex-weight*))
  ;; Create the vertex object. 
  (let ((vertex (make-instance class
		  :name vertex-name
		  :weight weight)))
    ;; Now add the vertex to the graph.
    (%add-graph-vertex vertex graph)
    ;; Return the new vertex
    vertex))

;;;-----------------------------------------------------------------------------
;;; %ADD-GRAPH-VERTEX
;;;-----------------------------------------------------------------------------

;;; This adds vertex to graph and sets the backpointer.

(defun %ADD-GRAPH-VERTEX (vertex graph)
  (setf (gethash (vertex-name vertex) (graph-vertices graph)) vertex)
  #+IGNORE
  (setf (vertex-graph vertex) graph)
  vertex)

;;;-----------------------------------------------------------------------------
;;; FIND-GRAPH-VERTEX
;;;-----------------------------------------------------------------------------

(defgeneric FIND-GRAPH-VERTEX (vertex-name graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-GRAPH-VERTEX ((vertex-name STRING)(graph GRAPH))
 (gethash vertex-name (graph-vertices graph)))

;;;-----------------------------------------------------------------------------

;;; Defined for convenience, probably deprecate!

(defmethod FIND-GRAPH-VERTEX ((vertex-name SYMBOL)(graph GRAPH))
   (find-graph-vertex (symbol-name vertex-name) graph))

;;;-----------------------------------------------------------------------------

;;; Defined for convenience, probably deprecate!

(defmethod FIND-GRAPH-VERTEX ((vertex-name NUMBER)(graph GRAPH))
   (find-graph-vertex (util::number-to-string vertex-name) graph))

;;;-----------------------------------------------------------------------------
;;; ENSURE-GRAPH-VERTEX
;;;-----------------------------------------------------------------------------

(defmethod ENSURE-GRAPH-VERTEX ((vertex-name STRING)(graph GRAPH)
			      &key
			      (class 'graph-vertex)
			      (weight *default-vertex-weight*))
  (or (find-graph-vertex vertex-name graph)
      (make-graph-vertex vertex-name graph
			 :weight weight
			 :class class)))


;;;-----------------------------------------------------------------------------
;;; DELETE-GRAPH-VERTEX
;;;-----------------------------------------------------------------------------

(defgeneric DELETE-GRAPH-VERTEX (graph-vertex graph))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-GRAPH-VERTEX ((vertex GRAPH-VERTEX)(graph GRAPH))
  (remhash (vertex-name vertex) (graph-vertices graph)))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-GRAPH-VERTEX :after ((vertex graph-vertex)(graph GRAPH))
  (mapc #'(lambda (edge)(delete-graph-edge edge graph))
	(find-outbound-edges vertex :all graph))
  (mapc #'(lambda (edge)(delete-graph-edge edge graph))
	(find-inbound-edges vertex :all graph))
  t)

;;;-----------------------------------------------------------------------------
;;; FIND-OUTBOUND-VERTICES
;;;-----------------------------------------------------------------------------

(defgeneric FIND-OUTBOUND-VERTICES (graph-vertex edge-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-OUTBOUND-VERTICES ((vertex GRAPH-VERTEX)
				   (edge-type SYMBOL)
				   (graph GRAPH))
  (mapcar #'edge-target (find-outbound-edges vertex edge-type graph)))

;;;-----------------------------------------------------------------------------
;;; FIND-INBOUND-VERTICES
;;;-----------------------------------------------------------------------------

(defgeneric FIND-INBOUND-VERTICES (graph-vertex edge-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-INBOUND-VERTICES ((vertex GRAPH-VERTEX)
				  (edge-type SYMBOL)
				  (graph GRAPH))
  (mapcar #'edge-source (find-inbound-edges vertex edge-type graph)))

;;;*****************************************************************************
;;; EDGE METHODS
;;;*****************************************************************************

;;;-----------------------------------------------------------------------------
;;; MAKE-GRAPH-EDGE
;;;----------------------------------------------------------------------------

(defgeneric MAKE-GRAPH-EDGE (edge-type vertex-1 vertex-2 graph
			     &key weight class))

;;;-----------------------------------------------------------------------------

;;; This creates a single directed ege from <source> to <target> of
;;; type <edge-type> and adds it to graph. If <hyper-edge-p> is true
;;; then the newly created edge is uniquely named and added to the
;;; vertex set.

;;; Non-hyper-edges have no name unless one was explicitly supplied.

(defmethod MAKE-GRAPH-EDGE ((edge-type SYMBOL)
                            (source GRAPH-VERTEX)
			    (target GRAPH-VERTEX)
			    (graph GRAPH)
			    &key 
			    (name nil)
			    (weight *default-edge-weight*)
			    (class 'graph-edge))
  (let ((edge (make-instance class 
		:name name
		:type edge-type
		:source source
                :target target
                :weight weight)))
    ;; Add the edge to the graph
    (%add-graph-edge edge (find-edge-tables graph edge-type))
    ;; Ensure graph edge types
    (pushnew edge-type (graph-edge-types graph) :test #'eq)
    ;; Return the edge object
    edge))

;;;-----------------------------------------------------------------------------
  
;;; Promote target edge to hyper vertex.
  
(defmethod MAKE-GRAPH-EDGE :after ((edge-type SYMBOL)
				   (source GRAPH-VERTEX)
				   (target GRAPH-EDGE)
				   (graph GRAPH)
				   &rest rest)
  (declare (ignore rest))
  (%add-hyper-vertex target graph)) 

;;;-----------------------------------------------------------------------------
  
;;; Promote source edge to hyper vertex.
  
(defmethod MAKE-GRAPH-EDGE :after ((edge-type SYMBOL)
				   (source GRAPH-EDGE)
				   (target GRAPH-VERTEX)
				   (graph GRAPH)
				   &rest rest)
  (declare (ignore rest))
  (%add-hyper-vertex source graph))

;;;-----------------------------------------------------------------------------
  
;;; Promote target edge to hyper vertex.
  
(defmethod MAKE-GRAPH-EDGE :after ((edge-type SYMBOL)
				   (source GRAPH-EDGE)
				   (target GRAPH-EDGE)
				   (graph GRAPH)
				   &rest rest)
  (declare (ignore rest))
  (%add-hyper-vertex source graph)
  (%add-hyper-vertex target graph))

;;;-----------------------------------------------------------------------------

(defun %ADD-HYPER-VERTEX (edge graph)
   ;; Ensure a name for the hyper-edge (edge/vertex)
  (unless (find-graph-vertex (object-name edge) graph)
    (setf (slot-value edge 'name) (make-default-edge-name edge))
    ;; Add the hyper edge to the vertex table as well.
    (%add-graph-vertex edge graph)))

;;;-----------------------------------------------------------------------------
;;; MAKE-DEFAULT-EDGE-NAME
;;;-----------------------------------------------------------------------------

(defmethod MAKE-DEFAULT-EDGE-NAME ((edge GRAPH-EDGE))
  (make-default-edge-name `(,(graph::edge-source-name edge)
			    ,(graph::edge-type edge)
			    ,(graph::edge-target-name edge))))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-DEFAULT-EDGE-NAME ((edge LIST))
  (format nil "~a-~a-~a" (first edge)(second edge)(third edge)))

;;;-----------------------------------------------------------------------------
;;; FIND-GRAPH-EDGE
;;;-----------------------------------------------------------------------------

(defgeneric FIND-GRAPH-EDGE (source-vertex edge-type target-vertex graph))

;;;-----------------------------------------------------------------------------
  
(defmethod FIND-GRAPH-EDGE ((source GRAPH-VERTEX) 
			    (edge-type STRING) 
			    (target GRAPH-VERTEX)
			    (graph GRAPH))
  "Returns link object from <source> vertex to <target-vertex> with
   name <edge-type>, else nil."
  (find-graph-edge source (util::safe-word-to-symbol edge-type) target graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-GRAPH-EDGE ((source GRAPH-VERTEX) 
			    (edge-type SYMBOL)
			    (target GRAPH-VERTEX)
			    (graph GRAPH))
  "Returns link object from <source> vertex to <target-vertex> with
   name <edge-type>, else nil."
  (let ((edge-tables (find-edge-tables graph edge-type)))
    (when edge-tables
      (%find-graph-edge source target edge-tables))))

;;;---------------------------------------------------------------------------
;;; %FIND-GRAPH-EDGE
;;;---------------------------------------------------------------------------

;;; This is an internal function that retrieves an edge object from a
;;; specific edges-tables object

(defmethod %FIND-GRAPH-EDGE ((source GRAPH-VERTEX)
			     (target GRAPH-VERTEX)
			     (edge-table EDGE-TABLES))
  "Given a EDGE-tableS object, return a edge object or nil,
   indexed by source and target vertex objects in the links hashtable."
  (let ((target-hash  (gethash source (links edge-table))))
    (when target-hash
      (gethash target target-hash))))

;;;-----------------------------------------------------------------------------
;;; ENSURE-GRAPH-EDGE
;;;-----------------------------------------------------------------------------

(defmethod ENSURE-GRAPH-EDGE ((edge-type SYMBOL)
			      (source GRAPH-VERTEX)
			      (target GRAPH-VERTEX)
			      (graph GRAPH)
			      &key 
			      (name nil)
			      (weight *default-edge-weight*)
			      (class 'graph-edge))
  (or (find-graph-edge source edge-type target graph)
      (make-graph-edge edge-type source target graph
		       :name name
		       :weight weight
		       :class class)))

;;;-----------------------------------------------------------------------------
;;; %ADD-GRAPH-EDGE
;;;-----------------------------------------------------------------------------

;; Internal function, don't call directly.

;;; TODO: This implementation uses a hash-table of dotted pairs. Need
;;; to compare this with the use of cascading hash tables.

(defmethod %ADD-GRAPH-EDGE ((edge GRAPH-EDGE) (edge-table EDGE-TABLES))
  (let* ((source (edge-source edge))
	 (target (edge-target edge))
	 (source-links-table (gethash source (links edge-table))))
    (unless source-links-table
      (setf source-links-table
	(setf (gethash source (links edge-table))
	  (make-hash-table :test #'eq :size 10))))
    ;; TODO: This implementation uses a hash-table of dotted pairs. Need
    ;; to compare this with the use of cascading hash tables.
    (setf (gethash target source-links-table) edge)
    ;; The next two are vertex-keyed lists of outbound and inbound edges.
    (push edge (gethash source (outbound-edges-table edge-table)))
    (push edge (gethash source (inbound-edges-table edge-table)))
    edge))

;;;-----------------------------------------------------------------------------
;;; DELETE-GRAPH-EDGE
;;;-----------------------------------------------------------------------------

(defgeneric DELETE-GRAPH-EDGE (edge graph))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-GRAPH-EDGE ((edge GRAPH-EDGE)(graph GRAPH))
  (%delete-graph-edge (find-edge-tables graph (edge-type edge))
		      edge))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-GRAPH-EDGE :after ((edge GRAPH-EDGE)(graph GRAPH))
  ;; Edge may be a hyper vertex, and may need to be deleted as a vertex.
  (when (find-graph-vertex (object-name edge) graph)
    (delete-graph-vertex edge graph)))

;;;-----------------------------------------------------------------------------
;;; %DELETE-GRAPH-EDGE
;;;-----------------------------------------------------------------------------

;; Internal function, don't call directly.

(defmethod %DELETE-GRAPH-EDGE ((edge-table EDGE-TABLES) (edge GRAPH-EDGE))
  (let ((outbound-edges (outbound-edges-table edge-table))
	(inbound-edges (inbound-edges-table edge-table))
	(source (edge-source edge))
	(target (edge-target edge)))
    (let ((target-hash (gethash source (links edge-table)))) 
      (when target-hash
	(remhash target target-hash)))
    (setf (gethash source outbound-edges) 
      (delete edge (gethash source outbound-edges)))
    ;; No more edge from source, remove it so it can be garbage collected.
    (unless (gethash source outbound-edges)
      (remhash source outbound-edges))
    (setf (gethash target inbound-edges) 
      (delete edge (gethash target inbound-edges)))
    ;; No more edge to target, remove it so it can be garbage collected.
    (unless (gethash target inbound-edges)
      (remhash target inbound-edges))))

;;;-----------------------------------------------------------------------------

(defmethod %DELETE-GRAPH-EDGE :after ((edge-table EDGE-TABLES) (edge GRAPH-EDGE))
  (when (empty-edge-tables-p edge-table)
    (delete-edge-tables edge-table (edge-type edge))))

;;;-----------------------------------------------------------------------------
;;; FIND-OUTBOUND-EDGES
;;;-----------------------------------------------------------------------------

(defgeneric FIND-OUTBOUND-EDGES (graph-vertex edge-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-OUTBOUND-EDGES ((vertex GRAPH-VERTEX) 
				(edge-type STRING)
				(graph GRAPH))
  "Return link objects of type <edge-type> with <vertex> as source, else nil."
  (find-outbound-edges vertex (util::safe-word-to-symbol edge-type) graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-OUTBOUND-EDGES ((vertex GRAPH-VERTEX)
				(edge-type SYMBOL)
				(graph GRAPH))
  "Return link objects named <edge-type> with <vertex> as source, else nil."
  (let ((edge-tables (find-edge-tables graph edge-type)))
    (gethash vertex (outbound-edges-table edge-tables))))

;;;-----------------------------------------------------------------------------

(defmethod FIND-OUTBOUND-EDGES ((vertex GRAPH-VERTEX)
			       (edge-type (eql :all))
			       (graph GRAPH))
  (%all-outbound-edges vertex graph))

;;;-----------------------------------------------------------------------------

;;; Note: This conses up a list of edges across edge types. Need a MAP fn.

(defmethod %ALL-OUTBOUND-EDGES ((obj GRAPH-VERTEX)(graph GRAPH))
  (let ((all-edges nil)
	(edges nil))
    (dolist (edge-type (graph-edge-types graph))
      (setf edges (find-outbound-edges obj edge-type graph))
      (when edges
	(setf all-edges (nconc all-edges edges))))
    all-edges))


;;;-----------------------------------------------------------------------------
;;; FIND-INTBOUND-EDGES
;;;-----------------------------------------------------------------------------

;;;This queries the INBOUND-EDGES hash table of the edge tables 

(defgeneric FIND-INBOUND-EDGES (graph-vertex edge-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-INBOUND-EDGES ((vertex GRAPH-VERTEX)
			       (edge-type STRING)
			       (graph GRAPH))
   (find-inbound-edges vertex (util::safe-word-to-symbol edge-type) graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-INBOUND-EDGES ((vertex GRAPH-VERTEX)
			       (edge-type SYMBOL)
			       (graph GRAPH))
   (let ((edge-tables (find-edge-tables graph edge-type)))
    (gethash vertex (inbound-edges-table edge-tables))))

;;;-----------------------------------------------------------------------------

(defmethod FIND-INBOUND-EDGES ((vertex GRAPH-VERTEX)
			       (edge-type (eql :all))
			       (graph GRAPH))
  (%all-inbound-edges vertex graph))

;;;-----------------------------------------------------------------------------
;;;  %ALL-INBOUND-EDGES
;;;-----------------------------------------------------------------------------

;;; Note: This conses up a list of edges across edge types. Need a MAP fn.

(defmethod %ALL-INBOUND-EDGES ((obj GRAPH-VERTEX)(graph GRAPH))
  (let ((all-edges nil)
	(edges nil))
    (dolist (edge-type (graph-edge-types graph))
      (setf edges (find-inbound-edges obj edge-type graph))
      (when edges
	(setf all-edges (nconc all-edges edges))))
    all-edges))


;;;-----------------------------------------------------------------------------
;;; FIND-EDGES-OF-TYPE
;;;-----------------------------------------------------------------------------

(defgeneric FIND-EDGES-OF-TYPE (edge-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-EDGES-OF-TYPE ((edge-type SYMBOL)(graph GRAPH))
  "Returns links objects named <edge-type> from <graph>, else nil."
  (let ((result-list '())
        (source-hash (links (find-edge-tables graph edge-type))))
    (when source-hash
      (maphash #'(lambda (source-vertex target-hash)
                   (declare (ignore source-vertex))
		   (maphash #'(lambda (target-vertex edge)
				(declare (ignore target-vertex))
				(push edge result-list))
			    target-hash))
               source-hash))
    result-list))

;;;-----------------------------------------------------------------------------
;;; MAP-OUTBOUND-EDGES
;;;-----------------------------------------------------------------------------

(defmethod MAP-OUTBOUND-EDGES ((vertex GRAPH-VERTEX)(edge-type (eql :all)) function
			       &key
			       (graph *default-graph*))
  (dolist (edge-type (graph-edge-types graph))
    (map-outbound-edges vertex edge-type function))
  vertex)
   
;;;-----------------------------------------------------------------------------

(defmethod MAP-OUTBOUND-EDGES ((vertex GRAPH-VERTEX)(edge-type SYMBOL) function
			       &key
			       (graph *default-graph*))
  (let ((edge-tables (find-edge-tables graph edge-type)))
    (dolist (edge (gethash vertex (outbound-edges-table edge-tables)))
      (funcall function edge))
    vertex))

;;;-----------------------------------------------------------------------------
;;; MAP-INBOUND-EDGES
;;;-----------------------------------------------------------------------------

(defmethod MAP-INBOUND-EDGES ((vertex GRAPH-VERTEX)(edge-type (eql :all)) function
			       &key
			       (graph *default-graph*))
  (dolist (edge-type (graph-edge-types graph))
    (map-inbound-edges vertex edge-type function))
  vertex)
   
;;;-----------------------------------------------------------------------------

(defmethod MAP-INBOUND-EDGES ((vertex GRAPH-VERTEX)(edge-type SYMBOL) function
			       &key
			       (graph *default-graph*))
   (let ((edge-tables (find-edge-tables graph edge-type)))
    (dolist (edge (gethash vertex (inbound-edges-table edge-tables)))
      (funcall function edge))
    vertex))
	
;;;-----------------------------------------------------------------------------
;;; EDGE-NAME
;;;-----------------------------------------------------------------------------

(defmethod EDGE-NAME ((link GRAPH-EDGE))
  (vertex-name link))
	
;;;-----------------------------------------------------------------------------
;;; EDGE-WEIGHT
;;;-----------------------------------------------------------------------------

(defmethod EDGE-WEIGHT ((link GRAPH-EDGE))
  (vertex-weight link))

;;;-----------------------------------------------------------------------------

(defmethod (setf EDGE-WEIGHT) (weight (link GRAPH-edge))
  (setf (vertex-weight link) weight))

;;;************************************************************************
;;; EDGE-TABLES FUNCTIONS
;;;************************************************************************

;;;---------------------------------------------------------------------------
;;; MAKE-EDGE-TABLES
;;;---------------------------------------------------------------------------

(defun MAKE-EDGE-TABLES (graph)
  (make-instance 'EDGE-TABLES :graph graph))

;;;-------------------------------------------------------------------------
;;; FIND-EDGE-TABLES
;;;-------------------------------------------------------------------------

;;; This finds or creates an edges-table object.

;;; TODO: Provide an ENSURE-EDGE-TABLES function so that the find find
;;; fiunction doesn't side effect the creation of edge-tables.

(defmethod FIND-EDGE-TABLES ((graph GRAPH) (edge-type SYMBOL))
  "Return a EDGE-tableS indexed by edge-type in the graph-edges hashtable."
  (or (gethash edge-type (graph-edges graph))
      (setf (gethash edge-type (graph-edges graph))
	(make-edge-tables graph))))

;;;---------------------------------------------------------------------------
;;; DELETE-EDGE-TABLES
;;;---------------------------------------------------------------------------

(defmethod DELETE-EDGE-TABLES ((table EDGE-tableS) (edge-type SYMBOL))
  (remhash edge-type (graph-edges (graph table))))

;;;---------------------------------------------------------------------------

(defmethod DELETE-EDGE-TABLES ((table EDGE-TABLES) (link GRAPH-EDGE))
  (delete-edge-tables table (edge-type link)))

;;;---------------------------------------------------------------------------
;;; EMPTY-EDGE-TABLES-P
;;;---------------------------------------------------------------------------

(defmethod EMPTY-EDGE-TABLES-P ((table EDGE-TABLES))
  (and (util::empty-hash-table-p (links table)) 
       (util::empty-hash-table-p (outbound-edges-table table)) 
       (util::empty-hash-table-p (inbound-edges-table table))))


;;;************************************************************************
;;; SAVE & RESTORE
;;;************************************************************************

;;;-----------------------------------------------------------------------------
;;; SAVE-GRAPH
;;;-----------------------------------------------------------------------------

(defgeneric SAVE-GRAPH (graph format destination &key metadata))


;;;-----------------------------------------------------------------------------

;;; This writes out line by line SEXPR rather than 2 large SEXPRs.

(defmethod SAVE-GRAPH ((graph GRAPH)(format (eql :lisp2))(file-name STRING)
		       &key
		       (subdirectory nil)
		       (metadata nil))
  (setf file-name (util::make-graphs-pathname file-name "lisp2" subdirectory))
  (format t "~%Saving graph to ~a..." file-name)
  (with-open-file (file file-name :direction :output :if-exists :supersede)
    ;; First Write the vertices
    (format file ";;; List of vertices.")
    (format file "~%:vertices")
    (map-graph-vertices 
     graph
     #'(lambda (vertex)
	 (let ((class (class-of vertex)))
	   (format file "~%(~s ~d ~a)"
		   (vertex-name vertex)
		   (vertex-weight vertex)
		   (format nil "~a::~a"
			   (package-name (symbol-package (class-name class)))
			   (class-name class))))))
     ;; Now write the edges
    (format file "~%~%;;; List of edges")
    (format file "~%:edges")
    (map-graph-edges 
     graph
     #'(lambda (edge)
	 (let ((class (class-of edge)))
	   (format file "~%(~a ~s ~s ~d ~a)"
		   (edge-type edge)
		   (vertex-name (edge-source edge))
		   (vertex-name (edge-target edge))
		   (edge-weight edge)
		    (format nil "~a::~a"
			   (package-name (symbol-package (class-name class)))
			   (class-name class))))))
    (format file "~%:end-of-graph~%")
    ;; Finally write the metadata
    (format file "~%~%;;; Metadata~%")
    (format file "~a" metadata)
  )
  t)

;;;-----------------------------------------------------------------------------

(defmethod SAVE-GRAPH ((graph GRAPH)(format (eql :lisp))(file-name STRING)
		       &key
		       (subdirectory nil)
		       (metadata nil))
  (declare (ignore subdirectory))
  (setf file-name (util::make-graphs-pathname file-name "lisp"))
  (format t "~%Saving graph to ~a..." file-name)
  (with-open-file (file file-name :direction :output :if-exists :supersede)
    ;; First Write the vertices
    (format file ";;; List of vertices.")
    (format file "~%(")
    (map-graph-vertices 
     graph
     #'(lambda (vertex)
	 (let ((class (class-of vertex)))
	   (format file "~%(~s ~d ~a)"
		   (vertex-name vertex)
		   (vertex-weight vertex)
		   (format nil "~a::~a"
			   (package-name (symbol-package (class-name class)))
			   (class-name class))))))
    (format file "~%)")
     ;; Now write the edges
    (format file "~%~%;;; List of edges")
    (format file "~%(")
    (map-graph-edges 
     graph
     #'(lambda (edge)
	 (let ((class (class-of edge)))
	   (format file "~%(~a ~s ~s ~d ~a)"
		   (edge-type edge)
		   (vertex-name (edge-source edge))
		   (vertex-name (edge-target edge))
		   (edge-weight edge)
		    (format nil "~a::~a"
			   (package-name (symbol-package (class-name class)))
			   (class-name class))))))
    ;; Finally write the metadata
    (format file "~%~%;;; Metadata~%")
    (format file "~a" metadata)
  (format file "~%)"))
  t)




;;;-----------------------------------------------------------------------------

;;; This approach did not work and actually took significantly longer
;;; to load the file.

(defmethod SAVE-GRAPH ((graph GRAPH)(format (eql :fasl))(file-name STRING)
		       &key 
		       (subdirectory nil)
		       (metadata nil))
  (declare (ignore subdirectory metadata))
  (setf file-name (util::make-graphs-pathname file-name "fastlisp"))
  (format t "~%Saving graph to ~a..." file-name)
  (with-open-file (file file-name :direction :output :if-exists :supersede)
    ;; Create the graph
    (let ((graph-class (class-of graph)))
      (format file
	      "~%(setf %%tempgraph%% (util::make-graph :name ~s :class '~a))"
	      (graph-name graph)
	      (format nil "~a::~a"
		      (package-name (symbol-package (class-name graph-class)))
		      (class-name graph-class))))
    ;; Create the vertices
    (map-graph-vertices 
     graph
     #'(lambda (vertex)
	 (let ((vertex-class (class-of vertex)))
	   (format file
		   "~%(ensure-graph-vertex ~s %%tempgraph%% :weight ~a :class '~a)"
		   (vertex-name vertex)
		   (vertex-weight vertex)
		   (format nil "~a::~a"
			   (package-name (symbol-package (class-name vertex-class)))
			   (class-name vertex-class))))))
    ;; Create the edges
    (map-graph-edges 
     graph
     #'(lambda (edge)
	 (let ((edge-class (class-of edge))
	       (v1 (edge-source edge))
	       (v2 (edge-target edge)))
	   (format file
		   "~%(ensure-graph-edge '~a (find-graph-vertex ~s  %%tempgraph%%)(find-graph-vertex ~s  %%tempgraph%%) %%tempgraph%% :weight ~a :class '~a)"
		   (edge-type edge)
		   (vertex-name v1)
		   (vertex-name v2)
		   (edge-weight edge)
		   (format nil "~a::~a"
			   (package-name (symbol-package (class-name edge-class)))
			   (class-name edge-class)))))))
  ;; Compile the graph
  (compile-file file-name))

		
;;;-----------------------------------------------------------------------------
;;; RESTORE-GRAPH
;;;-----------------------------------------------------------------------------

(defvar *GRAPH* nil)

(defgeneric RESTORE-GRAPH (format source
			   &key
			   graph graph-class vertex-class edge-class clear-graph))

;;;-----------------------------------------------------------------------------

(defmethod RESTORE-GRAPH ((format (eql :lisp2))(file-name STRING)
			  &key
			  (graph nil)
			  (graph-class 'graph)
			  (vertex-class 'graph-vertex)
			  (edge-class 'graph-edge)
			  (clear-graph nil)
			  (subdirectory nil)
			  (cache nil))
  (setf file-name (util::make-graphs-pathname file-name "lisp2" subdirectory))
  (cond ((probe-file file-name)
	 (format t "~%Restoring graph from ~a..." file-name)
	 (with-open-file (file file-name :direction :input)
	   (let ((graph (or graph
			    (find-graph (pathname-name file-name) :memory)
			    (make-graph :name (pathname-name file-name)
					:class graph-class
					:cache cache)))
		 (metadata nil))
	     (setf *graph* graph)
	     ;; Clear the graph
	     (when clear-graph
	       (clear-graph graph))
	     ;; Restore the vertices
	     (%restore-graph-vertices graph file vertex-class)
	     ;; Restore the edges
	     (%restore-graph-edges graph file edge-class)
	     ;; Read the metadata
	     (setf metadata (read file nil -1))
	     ;; Return the graph
	     (values graph metadata))))
	(t
	 (warn "The file ~a does not exist." file-name)
	 nil)))

;;;-----------------------------------------------------------------------------

;;; Helper function for RESTORE-GRAPH (mode :lisp2)

(defun %restore-graph-vertices (graph file vertex-class)
  (let ((vertex nil))
    (loop 
      (when (eq vertex :vertices)
	(return t))
      (setf vertex (read file)))
    (loop 
      (setf vertex (read file))
      ;;(format t "~%Vertex=~a" vertex)
      (when (not (listp vertex))
	(return t))
      (let ((class (third vertex)))
	(ensure-graph-vertex (first vertex) graph 
			     :weight (second vertex)
			     :class (or vertex-class class))))))

;;;-----------------------------------------------------------------------------

;;; Helper function for RESTORE-GRAPH (mode :lisp2)

(defun %restore-graph-edges (graph file edge-class)
  (let ((edge nil)
	(eof -1))
    (loop
      (setf edge (read file))
      (unless (eq edge :edges)
	(when (or (equal edge eof)
		  (equalp edge "end-of-graph")
		  (equalp edge 'end-of-graph)
		  (equalp edge :end-of-graph))
	  (return t))
	;;(format t "~%Edge=~a" edge)
	(let ((edge-type (first edge))
	      (v1 (find-graph-vertex (second edge) graph))
	      (v2 (find-graph-vertex (third edge) graph))
	      (weight (fourth edge))
	      (class  (fifth edge)))
	(when (and v1 v2)
	  ;; Avoid creation of duplicate edges.
	  (setf edge (ensure-graph-edge edge-type v1 v2 graph
					:weight weight
					:class (or edge-class class)))))))))

;;;-----------------------------------------------------------------------------
;;; RESTORE-GRAPH (:lisp)
;;;-----------------------------------------------------------------------------

(defmethod RESTORE-GRAPH ((format (eql :lisp))(file-name STRING)
			  &key
			  (graph nil)
			  (graph-class 'graph)
			  (vertex-class 'graph-vertex)
			  (edge-class 'graph-edge)
			  (clear-graph nil)
			  (cache nil))
   (setf file-name (util::make-graphs-pathname file-name "lisp"))
  (cond ((probe-file file-name)
	 (format t "~%Restoring graph from ~a..." file-name)
	 (with-open-file (file file-name :direction :input)
	   (let ((vertices (read file))
		 (vertex-list nil)
		 (edges (read file))
		 (edge-list nil)
		 ;; (metadata (read file nil -1))
		 (graph (or graph
			    (find-graph (pathname-name file-name) :memory)
			    (make-graph :name (pathname-name file-name)
					:class graph-class
					:cache cache))))
	     (setf *graph* graph)
	     ;; Clear the graph
	     (when clear-graph
	       (clear-graph graph))
	     ;; Create the vertices
	     (dolist (vertex vertices)
	       (let ((class (third vertex)))
		 (push (ensure-graph-vertex (first vertex) graph 
					    :weight (second vertex)
					    :class (or vertex-class class))
		       vertex-list)))
	     ;; Create the edges
	     (dolist (edge edges)
	       (let ((edge-type (first edge))
		     (v1 (find-graph-vertex (second edge) graph))
		     (v2 (find-graph-vertex (third edge) graph))
		     (weight (fourth edge))
		     (class  (fifth edge)))
		 (when (and v1 v2)
		   ;; Avoid creation of duplicate edges.
		   (let ((edge (find-graph-edge v1 edge-type v2 graph)))
		     (if edge (incf (edge-weight edge))
		       (setf edge (make-graph-edge edge-type v1 v2 graph
						   :weight weight
						   :class (or edge-class class))))
		     (push edge edge-list)))))
	     ;; Return the graph
	     (values graph (nreverse vertex-list)(nreverse edge-list)))))
	(t
	 (warn "The file ~a does not exist." file-name)
	 nil)))

;;;-----------------------------------------------------------------------------

;;; This approach did not work and actually took significantly longer
;;; to load the file.

(defvar %%tempgraph%% nil)

(defmethod RESTORE-GRAPH ((format (eql :fasl))(file-name STRING) &rest rest)
  (declare (ignore rest))
  (setf file-name (util::make-graphs-pathname file-name "fasl"))
  (format t "~%Restoring graph from ~a..." file-name)
  (load file-name)
  %%tempgraph%%)

;;;-----------------------------------------------------------------------------
;;; After Math
;;;-----------------------------------------------------------------------------

;;; Anton K's comments.  Are any still valid?

;;; TODO:
;;;
;;; 1. Reflect Tony's comments
;;; 2. Implement data load 
;;; 3. Implement functional tests  
;;; 4. Do bencmarking of design option A (implemented now)
;;; 5. Implement design option B 
;;; 6. Do bencmarking of design option B
;;; 7. Consider which design option - A or B - is better (expect difference no greater than 100%)
;;; 8. Do cons-level optimization of add-link and rem-link and probably other low-level
;;;    optimization (expect difference no greater than 50%)
;;;
;;; Notes:
;;;
;;; Implement and bencmark two design options:
;;;
;;; option A (implemented now)
;;; vertex/link instances identified by poiner 
;;; PRO: more compact, lack cross-machine operations support
;;; CON: unefficient check for source/target (source/vertex) identity
;;;
;;; option B (todo)
;;; vertex/link instances identified by integer id
;;; PRO: efficient check for source/target (source/vertex) identity, cross-machine operations enabled
;;; CON: less compact


;;;************************************************************************
;;; GRAPH PATHS
;;;************************************************************************

;;;-----------------------------------------------------------------------------
;;; MAKE-GRAPH-PATH
;;;-----------------------------------------------------------------------------

(defmethod MAKE-GRAPH-PATH((graph GRAPH)(edges LIST))
  (let ((path (make-instance 'graph-path :edge-path edges :graph graph))
	(vertices nil))
    (dolist (edge edges)
      (push (edge-source edge) vertices))
    (push (edge-target (first (last edges))) vertices)
    (setf (graph-vertex-path path) (nreverse vertices))
    (setf (graph-path-cost path)(compute-path-cost path))
    path))

;;;-----------------------------------------------------------------------------
;;; COMPUTE-PATH-COST
;;;-----------------------------------------------------------------------------

(defmethod COMPUTE-PATH-COST ((path GRAPH-PATH))
  (let ((cost 0))
    (dolist (edge (graph-edge-path path))
      (incf cost (edge-weight edge)))
    cost))


;;;---------------------------------------------------------------------------
;;; MAP-NODES-IN-SUBGRAPH
;;;---------------------------------------------------------------------------

;;; Effectively traverses the transitive closure of the node rooted at
;;; <node> applying <function> to each node visited. This function is
;;; cycle-friendly.

(defmethod MAP-NODES-IN-SUBGRAPH ((node GRAPH-VERTEX) function
				  &key 
				  (depth 0)
				  (children-accessor #'default-graph-child-accessor)
				  (processed-nodes (make-hash-table :size 20 :test #'eq)))
  (cond ((gethash node processed-nodes)
	 0)
	(t
	 (setf (gethash node processed-nodes) t)
	 (funcall function node)
	 (mapc #'(lambda (child)
		   (map-nodes-in-subgraph child function 
					  :depth (1+ depth)
					  :children-accessor children-accessor
					  :processed-nodes processed-nodes))
	        (funcall children-accessor node)))))

;;;---------------------------------------------------------------------------
;;; COUNT-NODES-IN-SUBGRAPH
;;;---------------------------------------------------------------------------

(defmethod COUNT-NODES-IN-SUBGRAPH ((node GRAPH-VERTEX)
				    &key 
				    (children-accessor #'default-graph-child-accessor))
  (let ((count 0))
    (map-nodes-in-subgraph node #'(lambda (n)(declare (ignore n))(incf count))
			   :children-accessor children-accessor)
    count))


;;;---------------------------------------------------------------------------
;;; PRETTY-PRINT-SUBGRAPH
;;;---------------------------------------------------------------------------

;;; Need to use map-nodes-in-subgraph instead!

(defmethod PRETTY-PRINT-SUBGRAPH ((node GRAPH-VERTEX)
				  &key 
				  (depth 0)
				  (children-accessor  #'default-graph-child-accessor)
				  (processed-nodes (make-hash-table :size 20 :test #'eq)))
  (unless (gethash node processed-nodes)
    (setf (gethash node processed-nodes) t)
    (format t "~%")
    (dotimes (i  depth)
      (format t "   "))
    (format t " -> ~a" (vertex-name node))
    (mapc #'(lambda (x)
	      (pretty-print-subgraph x
				     :depth (1+ depth)
				     :children-accessor children-accessor
				     :processed-nodes processed-nodes))
	(funcall children-accessor node))))

;;;---------------------------------------------------------------------------
;;; DEFAULT-GRAPH-CHILD-ACCESSOR
;;;---------------------------------------------------------------------------

(defun DEFAULT-GRAPH-CHILD-ACCESSOR (n &key (graph *default-graph*))
  (let ((*default-graph* graph))
    (mapcar #'edge-target (find-outbound-edges n :all graph))))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
	       
