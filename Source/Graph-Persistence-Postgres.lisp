(in-package :HYPERGRAPH)

;;;************************************************************************
;;;
;;; Graph Persistence with Postgres via POMO.
;;;
;;;************************************************************************
;;;*****************************************************************************
;;; PostGres Graphs DB
;;;
;;; The code in this file provides a persistence mechanism for in-memory graphs
;;; by placing them into a Postgresql Database.
;;;
;;;*****************************************************************************
;;;
;;; File Contents
;;; -------------
;;;
;;; INITIALIZE-SQL-CONNECTION
;;; CLOSE-SQL-CONNECTION
;;;
;;; Part 1: Basic Insertion and Retrieval
;;; -------------------------------------
;;;
;;; ADD-GRAPH
;;; FIND-GRAPH
;;;
;;; ADD-VERTEX
;;; FIND-VERTEX
;;; FIND-VERTEX-ID
;;;
;;; ADD-GRAPH-VERTICES
;;;
;;; ADD-EDGE
;;; FIND-EDGE
;;
;;; Part 2: Saving and Restoring Entire Graphs
;;; ------------------------------------------
;;;
;;; PUT-GRAPH-IN-DATABASE
;;; GET-GRAPH-FROM-DATABASE
;;;
;;;
;;; Part 3: Resetting the Database
;;; ------------------------------
;;;
;;; DELETE-ALL-GRAPH-DATABASES
;;; DELETE-ALL-GRAPHS
;;; DELETE-ALL-VERTICES
;;; DELETE-ALL-EDGES
;;;
;;; ALL-VERTICES
;;; ALL-EDGES
;;; ALL-GRAPHS
;;;
;;;*****************************************************************************

;;;----------------------------------------------------------------------------
;;; PostModern
;;;----------------------------------------------------------------------------


(eval-when (eval compile load)
  (asdf:oos 'asdf:load-op :postmodern))

#+IGNORE
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :postmodern))

(use-package :postmodern)

(defvar *SQL-DB-CONNECTION* nil)

;;;----------------------------------------------------------------------------
;;; INITIALIZE-SQL-CONNECTION
;;;----------------------------------------------------------------------------

(defmethod INITIALIZE-SQL-CONNECTION ((db-type (eql :postgres))
				      &key 
				      (host "localhost")
				      (db-name "postgres")
				      (user "postgres")
				      (password "postgres"))
  (unless *sql-db-connection*
    (pomo::connect-toplevel db-name user password host)
    (setf *sql-db-connection* t)))

;;;---------------------------------------------------------------------------
;;; CLOSE-SQL-CONNECTION
;;;----------------------------------------------------------------------------

(defmethod CLOSE-SQL-CONNECTION ((db-type (eql :postgres)))
  (when *sql-db-connection*
    (pomo::disconnect-toplevel)
    (setf *sql-db-connection* nil)))

;;;----------------------------------------------------------------------------
;;; Basic API
;;;----------------------------------------------------------------------------

(defmethod VERTEX-NAME ((vertex LIST))
  (first vertex))
;;;----------------------------------------------------------------------------

(defmethod VERTEX-WEIGHT ((vertex LIST))
  (second vertex))

;;;----------------------------------------------------------------------------

(defmethod EDGE-SOURCE ((edge LIST))
  (first edge))

;;;----------------------------------------------------------------------------

(defmethod EDGE-TYPE ((edge LIST))
  (second edge))

;;;----------------------------------------------------------------------------

(defmethod EDGE-TARGET ((edge LIST))
  (third edge))

;;;;----------------------------------------------------------------------------

(defmethod EDGE-WEIGHT ((edge LIST))
  (fourth edge))

;;;;----------------------------------------------------------------------------

(defmethod EDGE-NAME ((edge LIST))
  (fifth edge))
 
;;;----------------------------------------------------------------------------
;;; PERSIST-GRAPH 
;;;----------------------------------------------------------------------------

(defmethod PERSIST-GRAPH ((graph GRAPH::GRAPH)(db-type (eql :postgres))
			  &key 
			  (graph-name (graph-base-name graph)))
  ;; Add an entry in the graphs table
  (unless (find-graph graph-name :postgres)
    (add-graph graph db-type))
  ;; Create vertex and edges table for graph
  (unless (find-vertices-table graph-name)
    (make-vertices-table graph-name))
  ;; Add graph vertices to graph vertex-table
  (add-graph-vertices graph db-type)
  ;; Create edges table for graph if need
  (unless (find-edges-table graph-name)
    (make-edges-table graph-name))
  ;; Add graph edges to graph edge-table
  (add-graph-edges graph db-type)
  ;; Return the two table names
  (list (find-vertices-table graph-name)
	(find-edges-table graph-name)))

;;;----------------------------------------------------------------------------
;;; GRAPHS TABLE
;;;----------------------------------------------------------------------------

;;; This stores the various graph names in a single table. Each
;;; individual graph is stored in a separate table with the same name
;;; as the graph.

(defun MAKE-GRAPHS-TABLE ()
  (query (:create-table 
	  "Graphs" 
	  ((name :type string  :primary-key t)
	   (vertex-count :type integer)
	   (edge-count :type integer)))))

;;;----------------------------------------------------------------------------

(defun FIND-GRAPHS-TABLE ()
  (find (intern "GRAPHS" :keyword) (list-tables)))

;;;----------------------------------------------------------------------------

(defun ENSURE-GRAPHS-TABLE ()
  (or (find-graphs-table)(make-graphs-table)))

;;;----------------------------------------------------------------------------

;;; This is the DAO class for Graphs.

(defclass DB-GRAPH ()
  ((name :col-type string
	 :initarg :name
         :reader db-graph-name)
   (vertex-count :col-type integer :initarg :vertex-count
                :accessor vertex-count)
   (edge-count :col-type  integer :initarg :edge-count
              :accessor edge-count))
  (:metaclass dao-class)
  (:table-name "GRAPHS")
  (:keys name))
 
;;;----------------------------------------------------------------------------
;;; ADD-GRAPH
;;;----------------------------------------------------------------------------

;;; Adds Graph entry in graphs table of the PG DB.

(defmethod ADD-GRAPH ((graph GRAPH)(db-type (eql :postgres)))
  (let ((graph-dao (make-instance 'db-graph
				  :name (graph-base-name graph)
				  :vertex-count (graph::count-graph-vertices graph)
				  :edge-count (graph::count-graph-edges graph))))
    (save-dao graph-dao)
    graph-dao))

;;;----------------------------------------------------------------------------
;;; GRAPH-BASE-NAME
;;;----------------------------------------------------------------------------

(defmethod GRAPH-BASE-NAME ((graph GRAPH))
  (graph-base-name (graph-name graph)))

;;;----------------------------------------------------------------------------

(defmethod GRAPH-BASE-NAME ((graph-name STRING))
  (cond ((search "-processed" graph-name :test #'string-equal)
	 (setf graph-name (subseq graph-name 0 (- (length graph-name)(length "-processed")))))
	((search "-raw" graph-name :test #'string-equal)
	 (setf graph-name (subseq graph-name 0 (- (length graph-name)(length "-raw")))))
	(t nil))
  graph-name)

;;;----------------------------------------------------------------------------
;;; FIND-GRAPH
;;;----------------------------------------------------------------------------

(defmethod FIND-GRAPH ((graph GRAPH)(db-type (eql :postgres)))
  (find-graph (graph-base-name graph) db-type))

;;;----------------------------------------------------------------------------

(defmethod FIND-GRAPH ((graph-name STRING)(db-type (eql :postgres)))
  (query (:select 'name :from (:as 'GRAPHS 'x) :where (:= 'x.name graph-name))))

;;;----------------------------------------------------------------------------
;;; DELETE-GRAPH
;;;----------------------------------------------------------------------------

#+IGNORE
(defmethod DELETE-GRAPH ((graph-name STRING)(db-type (eql :postgres)))
  (query (:select 'name :from (:as 'GRAPHS 'x) :where (:= 'x.name graph-name))))

;;;----------------------------------------------------------------------------
;;; VERTICES TABLE
;;;----------------------------------------------------------------------------

(defun MAKE-VERTICES-TABLE (graph-name)
  (query (sql-compile `(:create-table 
		       ,(vertex-table-name graph-name)
		       ((name :type string  :primary-key t)
			(weight :type integer)
			(relevance :type real)))))
   (query (sql-compile `(:create-index 
			,(read-from-string (vertex-table-index graph-name))
			:on ,(vertex-table-name graph-name)
			:fields name weight relevance))))


;;;----------------------------------------------------------------------------

(defun FIND-VERTICES-TABLE (graph-name)
  (find (vertex-table-name graph-name)(list-tables)))

;;;----------------------------------------------------------------------------

(defun ENSURE-VERTICES-TABLE (graph-name)
  (or (find-vertices-table graph-name)
      (make-vertices-table graph-name)))

;;;----------------------------------------------------------------------------

(defun DELETE-VERTICES-TABLE (graph-name)
   (query (:drop-table  (vertex-table-name graph-name))))

;;;----------------------------------------------------------------------------

(defun VERTEX-TABLE-NAME (graph-name)
  (intern (string-upcase (format nil "~a-Vertices" graph-name)) :keyword))

;;;----------------------------------------------------------------------------

(defun VERTEX-TABLE-INDEX (graph-name)
  (format nil "~a-Vertex-Index" graph-name))

;;;----------------------------------------------------------------------------

;;; This is the DAO class for Vertices

(defclass DB-VERTEX ()
  ((name :col-type string
	 :initarg :name
         :accessor db-vertex-name)
   (weight :col-type integer
	   :initarg :weight
	   :accessor db-vertex-weight))
   (:metaclass dao-class)
   (:table-name "PG-VERTICES")
   (:keys name))

;;;----------------------------------------------------------------------------
;;; EDGES TABLE
;;;----------------------------------------------------------------------------

(defun MAKE-EDGES-TABLE (graph-name)
  (query (sql-compile `(:create-table 
			,(edge-table-name graph-name)
			((name :type string :primary-key t)
			 (source :type string)
			 (type :type string)
			 (target :type string)
			 (weight :type integer)
			 (relevance :type real)))))
  (query (sql-compile `(:create-index 
			,(read-from-string (edge-table-index graph-name))
			:on ,(edge-table-name graph-name)
			:fields source type target relevance)))
  (find-edges-table graph-name))

;;;----------------------------------------------------------------------------

(defun FIND-EDGES-TABLE (graph-name)
  (find (edge-table-name graph-name)(list-tables)))

;;;----------------------------------------------------------------------------

(defun DELETE-EDGES-TABLE (graph-name)
   (query (:drop-table  (edge-table-name graph-name))))

;;;----------------------------------------------------------------------------

(defun ENSURE-EDGES-TABLE (graph-name)
  (or (find-edges-table graph-name)
      (make-edges-table graph-name)))

;;;----------------------------------------------------------------------------

(defun EDGE-TABLE-NAME (graph-name)
  (intern (string-upcase (format nil "~a-Edges" graph-name)) :keyword))

;;;----------------------------------------------------------------------------

(defun EDGE-TABLE-INDEX (graph-name)
  (format nil "~a-Index" graph-name))

;;;----------------------------------------------------------------------------

;;; This is the DAO class for Edges

(defclass DB-EDGE ()
  ((name :col-type string
	 :initarg :name
         :accessor db-edge-name)
   (source :col-type string
	   :initarg :source
	   :accessor db-edge-source)
   (target :col-type string
	 :initarg :target
         :accessor db-edge-target)
   (type :col-type string
	 :initarg :edge-type
	 :accessor db-edge-type)
   (weight :col-type integer
	   :initarg :weight
	   :accessor db-edge-weight))
  (:metaclass dao-class)
  (:table-name "PG-EDGES")
  (:keys source target type))

;;;----------------------------------------------------------------------------
;;; ADD-VERTEX
;;;----------------------------------------------------------------------------

(defvar *vertex-template-dao* 
    (make-instance 'db-vertex))

(defmethod ADD-VERTEX ((vertex GRAPH-VERTEX)(graph GRAPH)(db-type (eql :postgres))
		       &key
		       (table 'PG-Vertices)
		       (relevance 0))
  (add-vertex `(,(graph::vertex-name vertex)
		,(graph::vertex-weight vertex))
	      graph
	      db-type
	      :table table
	      :relevance relevance))

;;;----------------------------------------------------------------------------

(defmethod ADD-VERTEX ((vertex LIST)(graph GRAPH)(db-type (eql :postgres))
		       &key
		       (table 'PG-Vertices)
		       (relevance 0))
  (let ((vertex-name (first vertex)))
    (unless (find-vertex vertex-name db-type :table table)
      (query (:insert-into table :set
			   'name vertex-name
			   'weight (second vertex)
			   'relevance relevance)))))

;;;----------------------------------------------------------------------------
;;; FIND-VERTEX
;;;----------------------------------------------------------------------------

(defmethod FIND-VERTEX ((vertex GRAPH::GRAPH-VERTEX)(db-type (eql :postgres))
			&key
			(table 'PG-Verices))
  (find-vertex (graph::vertex-name vertex) db-type :table table))

;;;----------------------------------------------------------------------------

(defmethod FIND-VERTEX ((vertex-name STRING)(db-type (eql :postgres))
			&key
			(table 'PG-Verices))
  (query (sql-compile `(:select 'name 'weight
			:from (:as ,table 'x)
			:where (:= 'x.name ,vertex-name)))))
  
;;;----------------------------------------------------------------------------
;;; LIST-VERTICES
;;;----------------------------------------------------------------------------

(defmethod LIST-VERTICES ((db-type (eql :postgres))
			  &key
			  (table 'PG-Verices))
  (mapcar #'first (query (sql-compile `(:select 'name :from (:as ,table 'x))))))
  
;;;----------------------------------------------------------------------------
;;; ADD-GRAPH-VERTICES
;;;----------------------------------------------------------------------------

;;; Adds all edges of graph to the mysql db.Turn off auto-commit
;;; during thios process for efficiency.

(defmethod ADD-GRAPH-VERTICES ((graph GRAPH)(db-type (eql :postgres)))
  (let ((table (vertex-table-name (graph-base-name graph))))
    (graph::map-graph-vertices 
     graph
     #'(lambda (vertex)
	 (add-vertex vertex graph db-type :table table)))
    t))
 
;;;----------------------------------------------------------------------------
;;; ADD-EDGE
;;;----------------------------------------------------------------------------

(defvar *wiki-edge-count* 0)

(defvar *edge-template-dao* 
    (make-instance 'db-edge))

(defmethod ADD-EDGE ((edge GRAPH::GRAPH-EDGE)(db-type (eql :postgres))
		       &key
		       (table 'PG-Edges)
		       (relevance 0))
  (add-edge `(,(graph::edge-source-name edge)
	      ,(graph::edge-type edge)
	      ,(graph::edge-target-name edge)
	      ,(graph::edge-weight edge)
	      ,(graph::edge-name edge))
	    db-type
	    :table table
	    :relevance relevance))

;;;----------------------------------------------------------------------------

(defmethod ADD-EDGE ((edge LIST)(db-type (eql :postgres))
		     &key
		     (table 'PG-Edges)
		     (relevance 0))
  (let ((edge-name (or (fifth edge)(graph::make-default-edge-name edge))))
    (unless (find-edge-by-name edge-name db-type :table table)
      (query (:insert-into table :set
			   'name edge-name
			   'source (first edge)
			   'type (symbol-name (second edge))
			   'target (third edge)
			   'weight (fourth edge)
			   'relevance relevance)))))

;;;----------------------------------------------------------------------------
;;; FIND-EDGE
;;;----------------------------------------------------------------------------

(defmethod FIND-EDGE ((edge GRAPH::GRAPH-EDGE)(db-type (eql :postgres))
		      &key
		      (table 'PG-EDGES))
  (find-edge `(,(graph::edge-source-name edge)
	       ,(graph::edge-type edge)
	       ,(graph::edge-target-name edge))
	       db-type
	       :table table))

;;;----------------------------------------------------------------------------

(defmethod FIND-EDGE ((edge LIST)(db-type (eql :postgres))
		      &key
		      (table 'PG-EDGES))
  (query (:select 'source 'type 'target 'weight
	  :from (:as table 'x)
	  :where (:and (:= 'x.source (first edge))
		       (:= 'x.type (symbol-name (second edge)))
		       (:= 'x.target (third edge))))))
  
;;;----------------------------------------------------------------------------
;;; FIND-EDGES
;;;----------------------------------------------------------------------------

(defmethod FIND-EDGES ((vertex GRAPH::GRAPH-VERTEX)
		       (edge-type SYMBOL)
		       (db-type (eql :postgres))
		      &key
		      (table 'PG-EDGES))
  (find-edges (graph::vertex-name vertex) edge-type db-type :table table))

;;;----------------------------------------------------------------------------

(defmethod FIND-EDGES ((vertex-name STRING)(edge-type SYMBOL)(db-type (eql :postgres))
		       &key
		       (table 'PG-EDGES))
  (query (:select 'source 'type 'target 'weight
	  :from (:as table 'x)
	  :where (:and (:= 'x.source vertex-name)
		       (:= 'x.type (symbol-name edge-type))))))
   
;;;----------------------------------------------------------------------------
;;; FIND-EDGE-BY-NAME
;;;----------------------------------------------------------------------------

(defmethod FIND-EDGE-BY-NAME ((edge GRAPH::GRAPH-EDGE)(db-type (eql :postgres))
			      &key
			      (table 'PG-EDGES))
  (find-edge-by-name (or (graph::edge-name edge)
			 (graph::make-default-edge-name edge))
		     db-type
		     :table table))

;;;----------------------------------------------------------------------------

(defmethod FIND-EDGE-BY-NAME ((edge-name STRING)(db-type (eql :postgres))
			      &key
			      (table 'PG-EDGES))
  (query (:select 'source 'type 'target 'weight
	  :from (:as table 'x)
	  :where  (:= 'x.name edge-name))))
       
;;;----------------------------------------------------------------------------
;;; ADD-GRAPH-EDGES
;;;----------------------------------------------------------------------------

;;; Adds all edges of graph to the mysql db. Turn off auto-commit
;;; during thios process for efficiency.

(defmethod ADD-GRAPH-EDGES ((graph GRAPH)(db-type (eql :postgres))
			    &key 
			    (edge-types :all))
  (let ((table-name (edge-table-name (graph-base-name graph)))
	(count 0))
    (graph::map-graph-edges
     graph
     #'(lambda (edge)
	 (incf count)
	 (when (zerop (mod count 50000))
	   (format t "~%~a edges inserted." count))
	 (when (or (eq edge-types :all)
		   (member (edge-type edge) edge-types))
	   (add-edge edge db-type :table table-name))))
    t))

;;;----------------------------------------------------------------------------
;;; TEST-EDGE-RETRIEVAL
;;;----------------------------------------------------------------------------

(defmethod TEST-PERSISTENT-EDGE-RETRIEVAL ((graph GRAPH))
  (let ((count 0))
    (time (graph::map-graph-edges 
	   graph
	   #'(lambda (edge)
	       (when (find-edge edge :postgres)
		 (incf count)
		 (when (zerop (mod count 5000))
		   (format t "~%~a edges retrieved." count))))))))
  
;;;----------------------------------------------------------------------------
;;; ALL-GRAPHS
;;;----------------------------------------------------------------------------


(defmethod ALL-GRAPHS ((db-type (eql :postgres)))
  (query (:select 'name :from 'Graphs)))

;;;****************************************************************************
;;; Part 2: Complete Graph Save & Restore
;;;****************************************************************************

;;;----------------------------------------------------------------------------
;;; PUT-GRAPH-IN-DATABASE 
;;;----------------------------------------------------------------------------

(defmethod PUT-GRAPH-IN-DATABASE  ((graph GRAPH)(db-type (eql :postgres)))
  (persist-graph graph db-type))
  
;;;----------------------------------------------------------------------------
;;; GET-GRAPH-FROM-DATABASE 
;;;----------------------------------------------------------------------------

(defmethod GET-GRAPH-FROM-DATABASE  ((graph-name STRING)(db-type (eql :postgres))
				     &key
				     (new-name graph-name))
  (let* ((vertex-table-name (vertex-table-name graph-name))
	 (edge-table-name (edge-table-name graph-name))
	 ;; Create the graph object
	 (graph (make-graph :name new-name)))
    ;; Create the vertices
    (eval (list 'doquery 
		`(:select 'name 'weight :from ,vertex-table-name)
		'(name weight)
		`(graph::make-graph-vertex name ,graph :weight weight)))
    ;; Create the edges
    (eval (list 'doquery 
		`(:select 'source 'type 'target 'weight 'name
		  :from ,edge-table-name)
		'(source type target weight name)
		`(let* ((v1 (graph::find-graph-vertex source ,graph))
			(v2 (graph::find-graph-vertex target ,graph)))
		   (graph::make-graph-edge (read-from-string type) v1 v2 ,graph
					   :weight weight
					   :name name))))
    graph))
 
;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
