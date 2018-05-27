(in-package :HYPERGRAPH)

;;;****************************************************************************
;;; BASIC SEMANTIC NET API
;;;****************************************************************************
;;;
;;; Classes
;;; -------
;;;
;;; SEMANTIC-NET
;;;
;;; CONCEPT-MIXIN
;;; CONCEPT
;;; ATOMIC-CONCEPT
;;; COMPOSITE-CONCEPT
;;;
;;; RELATION
;;;
;;;
;;; Concepts
;;; --------
;;;
;;; MAKE-CONCEPT
;;; ADD-CONCEPT
;;; FIND-CONCEPT
;;; ENSURE-CONCEPT
;;; DELETE-CONCEPT
;;;
;;; MAP-CONCEPTS
;;; DO-CONCEPTS
;;;
;;; CONCEPT-NORM
;;; COMPOSING-CONCEPTS
;;; ATOMIC-CONCEPT-P
;;;
;;; DELETE-ALL-CONCEPTS
;;;
;;; SORT-CONCEPTS-BY-NAME
;;; SORT-CONCEPTS-BY-WEIGHT
;;; SORT-CONCEPT-PAIRS-BY-NAME
;;;
;;; PRINT-ALL-CONCEPTS
;;;
;;;
;;; Relations
;;; ---------
;;;
;;; RELATION-NAME
;;; RELATION-WEIGHT
;;; RELATION-SOURCE
;;; RELATION-TARGET
;;; RELATION-TYPE
;;;
;;; MAKE-RELATION
;;; MAKE-RELATION-NAME
;;; ENSURE-RELATION
;;;
;;; ADD-RELATION
;;; DELETE-RELATION
;;;
;;; FIND-RELATION
;;; FIND-RELATIONS
;;; FIND-RELATION-BY-NAME
;;; FIND-RELATIONS-BY-TYPE
;;; FIND-LINKED-CONCEPTS
;;; FIND-BIDIRECTIONAL-RELATIONS
;;;
;;; SORT-RELATIONS-BY-SOURCE
;;; SORT-RELATIONS-BY-TARGET
;;; SORT-RELATIONS-BY-WEIGHT
;;;
;;; MAP-RELATIONS
;;; MAP-CONCEPT-RELATIONS
;;;
;;; PRINT-ALL-RELATIONS
;;;
;;;
;;; Reverse RelationS
;;; ----------------------
;;;
;;; *DEFAULT-REVERSE-RELATION-TYPES*
;;; ADD-RELATION-TYPE-ENTRY
;;; FIND-RELATION-TYPE-ENTRY
;;; DELETE-RELATION-TYPE-ENTRY
;;;
;;; ADD-REVERSE-RELATION
;;; FIND-REVERSE-RELATION
;;; DELETE-REVERSE-RELATION
;;;
;;;
;;; Save & Restore
;;; --------------
;;;
;;; SAVE-SEMANTIC-NET
;;; RESTORE-SEMANTIC-NET
;;;
;;;
;;; ISA Relations
;;; -------------
;
;;; ADD-ISA-LINK
;;; CONCEPT-ISAS
;;; CONCEPT-ISA-P
;;;
;;;
;;;
;;; Miscelaneous
;;;
;;; SAME-VALUE-P

;;;******************************************************************************


;;;-----------------------------------------------------------------------------
;;; *DEFAULT-REVERSE-RELATION-TYPES*
;;;-----------------------------------------------------------------------------

(defparameter *DEFAULT-REVERSE-RELATION-TYPES*
    '((RELATED-TO RELATED-TO)
      (ISA ISAR)
      (REVERSES REVERSES)
      (ABBREVIATES IS-ABBREVIATED-BY)
      (COMPOSES COMPOSED-OF)
      (SUBSUMES IS-SUBSUMED-BY)
      (IS-SYNOMYN-OF IS-SYNOMYN-OF)
      (HAS-VALUE IS-VALUE-OF)
      (PRECEDES FOLLOWS)))

;;;******************************************************************************
;;; Part 1: Classes
;;;******************************************************************************

;;;-----------------------------------------------------------------------------
;;; SEMANTIC-NET
;;;-----------------------------------------------------------------------------

(defclass SEMANTIC-NET (GRAPH)
  ((reverse-relation-types
	  :initarg :reverse-relation-types
	  :accessor reverse-relation-types
	  :initform '((related-to related-to)))))

;;;-----------------------------------------------------------------------------

(defun MAKE-SEMANTIC-NET  (&key (name nil)
				(class 'semantic-net)
				(cache nil)
				(reverse-relation-types *default-reverse-relation-types*))
  (let ((graph (make-graph :name name :class class :cache cache)))
    (setf (reverse-relation-types graph) reverse-relation-types)
    graph))

;;;-----------------------------------------------------------------------------

(defvar *DEFAULT-NET*
    (make-semantic-net :name "Semantic Net"
		       :class 'semantic-net
		       :reverse-relation-types *default-reverse-relation-types*)
  "Default Semantic Net")

;;;-----------------------------------------------------------------------------
;;; CONCEPT-MIXIN
;;;-----------------------------------------------------------------------------

;;; This is common to both concepts and relations.

(defclass CONCEPT-MIXIN ()
  ((value :initform nil
	  :initarg :value
	  :accessor concept-value
	  :accessor object-value)))

;;;-----------------------------------------------------------------------------
;;; CONCEPT
;;;-----------------------------------------------------------------------------

;;; Add acessors to slots inherited from graph-vertex

(defclass CONCEPT (CONCEPT-MIXIN GRAPH-VERTEX)
  ((name :accessor concept-name)
   (weight :accessor concept-weight)))

;;;-----------------------------------------------------------------------------

;;; For backward compatibility: Class names were written to graph files!!!

(defclass UTIL::CONCEPT (CONCEPT)
  ())

;;;-----------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((object CONCEPT) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (format stream "#<Concept(~d): ~a>"
		 (concept-weight object)
		 (concept-name object)))))

;;;-----------------------------------------------------------------------------
;;; ATOMIC-CONCEPT
;;;-----------------------------------------------------------------------------

(defclass ATOMIC-CONCEPT (CONCEPT)
  ())

;;;-----------------------------------------------------------------------------
;;; COMPOSITE-CONCEPT
;;;-----------------------------------------------------------------------------

(defclass COMPOSITE-CONCEPT (CONCEPT)
  ())

;;;-----------------------------------------------------------------------------
;;; RELATION
;;;-----------------------------------------------------------------------------

;;; Added reverse link slot as GBBO link slot for efficient deletion
;;; of concepts and their links.

(defclass RELATION (CONCEPT GRAPH-EDGE)
  ((name :accessor relation-name)
   (value :accessor relation-value)
   (type :accessor relation-type
	 :initform 'related-to)
   (weight :accessor relation-weight)
   (source  :accessor relation-source)
   (target  :accessor relation-target)
   (reverse :initarg :reverse
	    :initform nil
	    :accessor reverse-relation)))

;;;-----------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((object RELATION) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (format stream "#<relation(~a): ~a ~a ~a>"
		 (relation-weight object)
		 (concept-name (relation-source object))
		 (relation-type object)
		 (concept-name (relation-target object))))))

;;;-----------------------------------------------------------------------------

;;; For backward compatibility: Clas names were written to graph files.

(defclass UTIL::RELATION (RELATION)
  ())

;;;******************************************************************************
;;; Part 2: Concepts
;;;******************************************************************************

;;;-----------------------------------------------------------------------------
;;; MAKE-CONCEPT
;;;-----------------------------------------------------------------------------

(defmethod MAKE-CONCEPT ((concept-name STRING)
			 &key 
			 (value concept-name)
			 (weight 0)
			 (class 'CONCEPT)
			 (graph *default-net*))
  (let ((concept (make-graph-vertex concept-name graph :class class :weight weight)))
    (setf (concept-value concept) value)
    concept))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-CONCEPT ((concept-value SYMBOL)
			 &key 
			 (value concept-value)
			 (weight 0)
			 (class 'CONCEPT)
			 (graph *default-net*))
  (let ((concept (make-graph-vertex (symbol-name concept-value) graph
				   :class class :weight weight)))
    (setf (concept-value concept) value)
    concept))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-CONCEPT ((concept-value NUMBER)
			 &key 
			 (value concept-value)
			 (weight 0)
			 (class 'CONCEPT)
			 (graph *default-net*))
   (let ((concept (make-graph-vertex (util::number-to-string concept-value) graph
				     :class class :weight weight)))
     (setf (concept-value concept) value)
     concept))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-CONCEPT ((concept-value CHARACTER)
			 &key 
			 (value concept-value)
			 (weight 0)
			 (class 'CONCEPT)
			 (graph *default-net*))
   (let ((concept (make-graph-vertex (format nil "~a" concept-value) graph
				     :class class :weight weight)))
     (setf (concept-value concept) value)
     concept))

;;;-----------------------------------------------------------------------------

(defmethod OBJECT-NAME ((obj CONCEPT-MIXIN))
  (concept-name obj))

;;;-----------------------------------------------------------------------------
;;; ADD-CONCEPT
;;;-----------------------------------------------------------------------------

(defmethod ADD-CONCEPT ((concept CONCEPT-MIXIN))
  concept)

;;;-----------------------------------------------------------------------------

;; Until we fix the NULL concept problem...

(defmethod ADD-CONCEPT ((null NULL))
  nil)

;;;-----------------------------------------------------------------------------
;;; FIND-CONCEPT
;;;-----------------------------------------------------------------------------

(defmethod FIND-CONCEPT ((concept-name STRING) &key (graph *default-NET*))
  (find-graph-vertex concept-name graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-CONCEPT ((concept-value SYMBOL) &key (graph *default-NET*))
  (find-concept (symbol-name concept-value) :graph graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-CONCEPT ((concept-value NUMBER) &key (graph *default-NET*))
  (find-concept (util::number-to-string concept-value) :graph graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-CONCEPT ((concept-value CHARACTER) &key (graph *default-NET*))
  (find-concept (format nil "~a" concept-value) :graph graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-CONCEPT ((concept CONCEPT) &key (graph *default-NET*))
  (declare (ignore graph))
  concept)

;;;-----------------------------------------------------------------------------
;;; ENSURE-CONCEPT
;;;-----------------------------------------------------------------------------

;;; This function should replace FIND-OR-MAKE-CONCEPT which should be deprecated.
;;; For now just adding a definition for ENSURE-CONCEPT.

(defmethod ENSURE-CONCEPT ((concept-name STRING)
			   &rest rest
			   &key 
			   (graph *default-net*)
			   (class 'concept))
  (or (find-concept concept-name :graph graph)
      (apply #'make-concept concept-name :class class rest)))

;;;-----------------------------------------------------------------------------

(defmethod ENSURE-CONCEPT ((concept-name SYMBOL)
			   &rest rest
			   &key (graph *default-net*)
				(class 'concept))
   (or (find-concept concept-name :graph graph)
      (apply #'make-concept concept-name :class class rest)))

;;;-----------------------------------------------------------------------------

(defmethod ENSURE-CONCEPT ((concept-name NUMBER)
			   &rest rest
			   &key (graph *default-net*)
				(class 'concept))
  (or (find-concept concept-name :graph graph)
      (apply #'make-concept concept-name :class class Rest)))

;;;-----------------------------------------------------------------------------

(defmethod ENSURE-CONCEPT ((concept-name CHARACTER)
			   &rest rest
			   &key (graph *default-net*)
				(class 'concept))
  (or (find-concept concept-name :graph graph)
      (apply #'make-concept concept-name :class class Rest)))

;;;-----------------------------------------------------------------------------

(defmethod ENSURE-CONCEPT ((concept CONCEPT)
			   &rest rest
			   &key (graph *default-net*))		
  (declare (ignore graph rest))
  (values concept nil))

;;;-----------------------------------------------------------------------------

;;; Note: If make-concept is called directly weight does not change.

(defmethod ENSURE-CONCEPT :around ((concept t)
				   &rest rest
				   &key (increment-weight t))
  (declare (ignore rest))
  (let ((concept (call-next-method)))
    (when increment-weight
      (incf (concept-weight concept)))
    concept))

;;;-----------------------------------------------------------------------------
;;; DELETE-CONCEPT
;;;-----------------------------------------------------------------------------

;;; Note this deletes all outgoing and incoming relations assciated to
;;; concept. An :after methods on delete-graph-vertex takes care of this.

(defmethod DELETE-CONCEPT ((concept CONCEPT) &key (graph *default-net*))
  (delete-graph-vertex concept graph))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-CONCEPT ((concept-name STRING) &key (graph *default-net*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (delete-concept concept))))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-CONCEPT ((concept-name SYMBOL) &key (graph *default-net*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (delete-concept concept))))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-CONCEPT ((concept-name NUMBER) &key (graph *default-net*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (delete-concept concept))))

;;;-----------------------------------------------------------------------------
;;; DELETE-ALL-CONCEPTS
;;;-----------------------------------------------------------------------------

(defun DELETE-ALL-CONCEPTS (&key (graph *default-net*))
  (map-concepts #'(lambda (concept)(delete-concept concept :graph graph))
		:graph graph))

;;;-----------------------------------------------------------------------------
;;; SORT-CONCEPTS-BY-NAME
;;;-----------------------------------------------------------------------------

(defmethod SORT-CONCEPTS-BY-NAME ((concepts LIST))
  (sort (copy-list concepts)
	#'(lambda (c1 c2)
	    (string<= (concept-name c1)(concept-name c2)))))

;;;-----------------------------------------------------------------------------
;;; SORT-CONCEPTS-BY-WEIGHT
;;;-----------------------------------------------------------------------------

(defmethod SORT-CONCEPTS-BY-WEIGHT ((concepts LIST))
  (sort (copy-list concepts)
	#'(lambda (c1 c2)
	    (>= (concept-weight c1)(concept-weight c2)))))

;;;-----------------------------------------------------------------------------
;;; SORT-CONCEPT-PAIRS-BY-NAME
;;;-----------------------------------------------------------------------------

(defmethod SORT-CONCEPT-PAIRS-BY-NAME ((concepts LIST))
  (sort (copy-list concepts)
	#'(lambda (p1 p2)
	    (string<= (concept-name (first p1))(concept-name (first p2))))))

;;;-----------------------------------------------------------------------------
;;; PRINT-ALL-CONCEPTS
;;;-----------------------------------------------------------------------------

(defun PRINT-ALL-CONCEPTS (&key (graph *default-net*))
  (map-concepts #'print	:graph graph))

;;;-----------------------------------------------------------------------------
;;; CONCEPT-LIST-TO-STRING
;;;-----------------------------------------------------------------------------

;;; Should go with CS stuff. Putting here for now.

(defun CONCEPT-LIST-TO-STRING (concept-list)
  (util::make-description-string (mapcar #'concept-name concept-list)))

;;;*****************************************************************************
;;; MAP Functions
;;;*****************************************************************************

;;;-----------------------------------------------------------------------------
;;; MAP-CONCEPTS
;;;-----------------------------------------------------------------------------

(defun MAP-CONCEPTS (function &key (graph *default-NET*)(filter #'identity))
  (map-graph-vertices graph
		      #'(lambda (concept)
			  (when (funcall filter concept)
			    (funcall function concept)))))

;;;-----------------------------------------------------------------------------
;;; DO-CONCEPTS
;;;-----------------------------------------------------------------------------

(defmacro DO-CONCEPTS ((concept &key (graph *default-net*)) &body body)
  "Iterate over all concepts."
  `(block nil
     (map-concepts #'(lambda (,concept) ,@body) :graph ,graph)))

;;;-----------------------------------------------------------------------------
;;; ALL-CONCEPTS
;;;-----------------------------------------------------------------------------

;;; This is silly as it conses up a list of all concepts. Strictly for
;;; dev purposes, do not include in API.

(defun ALL-CONCEPTS (&key (graph *default-NET*))
  (let ((concepts nil))
    (do-concepts (concept :graph graph)(push concept concepts))
    concepts))

;;;-----------------------------------------------------------------------------
;;; ATOMIC-CONCEPT-P
;;;-----------------------------------------------------------------------------

(defmethod ATOMIC-CONCEPT-P ((concept CONCEPT) 
			     &key
			     (graph *default-net*))
  (declare (ignore graph))
  (= (concept-norm concept) 1))


;;;-----------------------------------------------------------------------------

(defmethod ATOMIC-CONCEPT-P ((concept-name STRING)
			     &key
			     (graph *default-net*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (atomic-concept-p concept :graph graph))))

;;;-----------------------------------------------------------------------------
;;; CONCEPT-NORM
;;;-----------------------------------------------------------------------------

(defmethod CONCEPT-NORM ((concept CONCEPT) &key (graph *default-NET*))
  (declare (ignore graph))
  (let ((composing-concepts (composing-concepts concept)))
    (length composing-concepts)))

;;;-----------------------------------------------------------------------------

(defmethod CONCEPT-NORM ((concept-name STRING) &key (graph *default-NET*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (concept-norm concept :graph graph))))

;;;-----------------------------------------------------------------------------
;;; COMPOSING-CONCEPTS
;;;-----------------------------------------------------------------------------

(defmethod COMPOSING-CONCEPTS ((concept CONCEPT))
  (mapcar #'relation-target (find-relations concept 'composed-of)))

;;;******************************************************************************
;;; Part 3: Relations
;;;******************************************************************************

;;; Class defined accessors:
;;;
;;; RELATION-NAME
;;; RELATION-WEIGHT
;;; RELATION-SOURCE
;;; RELATION-TARGET
;;; RELATION-TYPE

;;;-----------------------------------------------------------------------------
;;; MAKE-RELATION
;;;-----------------------------------------------------------------------------

(defmethod MAKE-RELATION ((relation-type SYMBOL)
			  (source CONCEPT-MIXIN)
			  (target CONCEPT-MIXIN)
			  &key
			  (class 'relation)
			  (graph *default-net*))
  (make-graph-edge relation-type source target graph :class class))

;;;-----------------------------------------------------------------------------

;;; Don't allow relations with null types, signal an error if such an
;;; attemp is made.

(defmethod MAKE-RELATION ((relation-type NULL)
			  (source T)
			  (target T)
			  &key
			  (class 'relation)
			  (graph *default-net*))
  (declare (ignore class graph))
  (error "Attempt to create an edge of type NULL with source ~a and target ~a" 
	 source
	 target))

;;;-----------------------------------------------------------------------------
;;; ENSURE-RELATION
;;;-----------------------------------------------------------------------------

(defmethod ENSURE-RELATION ((relation-type SYMBOL)
			    (source CONCEPT-MIXIN)
			    (target CONCEPT-MIXIN)
			    &key
			    (class 'relation)
			    (graph *default-net*)
			    (no-reverse-relation nil)
			    (increment-weight t))
  (let ((relation (find-relation source relation-type target :graph graph)))
    (cond ((and relation increment-weight)
	   (incf (relation-weight relation)))
	  (t
	   (setf relation
	     (add-relation relation-type source target
			   :graph graph
			   :class class
			   :no-reverse-relation no-reverse-relation))))
    relation))

;;;-----------------------------------------------------------------------------
;;; MAKE-RELATION-NAME
;;;-----------------------------------------------------------------------------

;;; Creates a "unique" name based source and target names and relation type.

(defmethod MAKE-RELATION-NAME ((object1 CONCEPT-MIXIN)
			       (relation-type SYMBOL)
			       (object2 CONCEPT-MIXIN))
  (let ((name1 (concept-name object1))
        (name2 (concept-name object2)))
    (format nil "~a-~a-~a" name1 relation-type name2)))


;;;-----------------------------------------------------------------------------
;;; ADD-RELATION
;;;-----------------------------------------------------------------------------

;;; Adds a labeled and weighted link from <c1> to <c2> of type <relation-type>.

;;; Note: This should probably be deprecated as make-relation does an
;;; implicit add. Either that or make-relation should simply create
;;; the object and not add to the graph.

;;; Unlike make-relation this function also deals with reverse
;;; relations. See the :after method.

(defmethod ADD-RELATION ((relation-type SYMBOL) (c1 CONCEPT) (c2 CONCEPT)
			 &key
			 (class 'relation) 
			 (graph *default-net*)
			 (no-reverse-relation nil)
			 (increment-weight t))
  (declare (ignorable no-reverse-relation increment-weight))
  (make-relation relation-type c1 c2 :graph graph :class class))

;;;-----------------------------------------------------------------------------

;;; This adds reverse relations.

(defmethod ADD-RELATION :after ((relation-type SYMBOL)
				(concept1 CONCEPT)
				(concept2 CONCEPT)
				&key
				(class 'relation)
				(graph *default-net*)
				(no-reverse-relation nil)
				(increment-weight t))
  ;; Now set up the reverse link
  (unless no-reverse-relation
    (add-reverse-relation relation-type concept1 concept2
			  :graph graph
			  :class class
			  :increment-weight increment-weight)))

;;;-----------------------------------------------------------------------------
;;; DELETE-RELATION
;;;-----------------------------------------------------------------------------

(defmethod DELETE-RELATION ((edge RELATION) &key (graph *default-net*))
  (delete-graph-edge edge graph))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-RELATION :after ((edge RELATION) &key (graph *default-net*))
  (delete-reverse-relation edge :graph graph))

;;;-----------------------------------------------------------------------------
;;; FIND-RELATION
;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATION ((c1 CONCEPT-MIXIN)
			  (relation-type SYMBOL) 
			  (c2 CONCEPT-MIXIN)
			  &key
			  (graph *default-net*))
  (find-graph-edge c1 relation-type c2 graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATION ((concept1-name T)
			  (relation-type T)
			  (concept2-name t)
			  &key
			  (graph *default-net*))
  (let ((concept1 (find-concept concept1-name :graph graph))
        (concept2 (find-concept concept2-name :graph graph)))
    (when (and concept1 relation-type concept2)
      (find-relation concept1 relation-type concept2 :graph graph))))

;;;-----------------------------------------------------------------------------
;;; FIND-RELATIONS
;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATIONS ((c1 CONCEPT-MIXIN) (relation-type STRING)
			   &key
			   (graph *default-net*))
  (find-outbound-edges c1 relation-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATIONS ((c1 CONCEPT-MIXIN) (relation-type SYMBOL)
			   &key
			   (graph *default-net*))
  (find-outbound-edges c1 relation-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATIONS ((concept-name T) (relation-type SYMBOL)
			   &key
			   (graph *default-net*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (find-relations concept relation-type :graph graph))))

;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATIONS ((concept-name T) (relation-type STRING)
			   &key
			   (graph *default-net*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (find-relations concept relation-type :graph graph))))

;;;-----------------------------------------------------------------------------
;;; FIND-RELATION-BY-NAME
;;;-----------------------------------------------------------------------------

;;; This retieves a link object by it's full symbol name which consists of the 
;;; the source concept name, the relation-type and the target concept name.

(defmethod FIND-RELATION-BY-NAME ((link-name SYMBOL) &key (graph *default-NET*))
  (find-concept link-name :graph graph))

;;;-----------------------------------------------------------------------------

;;; This retieves a link object by it's full string name which consists of the 
;;; the source concept name, the RELATION-type and the target concept name.

(defmethod FIND-RELATION-BY-NAME ((link-name STRING) &key (graph *default-NET*))
  (find-concept link-name :graph graph))

;;;-----------------------------------------------------------------------------
;;;  FIND-RELATIONS-BY-TYPE
;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATIONS-BY-TYPE ((name SYMBOL) &key (graph *default-NET*))
  (find-edges-of-type name graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-RELATIONS-BY-TYPE ((name STRING) &key (graph *default-NET*))
  (unless (equal name util::*null-string*)
    (find-edges-of-type name graph)))

;;;-----------------------------------------------------------------------------
;;; FIND-LINKED-CONCEPTS
;;;-----------------------------------------------------------------------------

(defmethod FIND-LINKED-CONCEPTS ((c1 CONCEPT-MIXIN) (RELATION-type STRING)
				 &key
				 (graph *default-NET*))
  (find-outbound-vertices c1 RELATION-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-LINKED-CONCEPTS ((c1 CONCEPT-MIXIN) (RELATION-type SYMBOL)
				 &key
				 (graph *default-NET*))
  (find-outbound-vertices c1 RELATION-type graph))

;;;-----------------------------------------------------------------------------

(defmethod FIND-LINKED-CONCEPTS ((concept-name t) link-name
 				 &key
				 (graph *default-NET*))
  (let ((concept (find-concept concept-name :graph graph)))
    (when concept
      (find-linked-concepts concept link-name :graph graph))))


;;;-----------------------------------------------------------------------------
;;; FIND-BIDIRECTIONAL-RELATIONS
;;;-----------------------------------------------------------------------------

(defmethod FIND-BIDIRECTIONAL-RELATIONS ((graph GRAPH))
  (let* ((bidirectional-relations nil))
    (map-concepts 
     #'(lambda (concept)
	 (map-graph-edges
	  concept
	  #'(lambda (relation)
	      (let ((c1 (edge-source relation))
		    (c2 (edge-target relation))
		    (type (edge-type relation)))
		(unless (eq c1 c2)
		  (when (find-relation c2 type c1 :graph graph)
		    (unless (or (find (list c1 c2) bidirectional-relations 
				      :test #'equal)
				(find (list c2 c1) bidirectional-relations 
				      :test #'equal))
		      (push (list c1 c2) bidirectional-relations))))))))
     :graph graph)
    bidirectional-relations))
			      
;;;-----------------------------------------------------------------------------
;;; SORT-RELATIONS-BY-SOURCE-NAME
;;;-----------------------------------------------------------------------------

(defmethod SORT-RELATIONS-BY-SOURCE-NAME ((relations LIST))
  (sort relations #'(lambda (r1 r2)
		     (string<= (concept-name (relation-source r1))
			       (concept-name (relation-source r2))))))

;;;-----------------------------------------------------------------------------
;;; SORT-RELATIONS-BY-TARGET-NAME
;;;-----------------------------------------------------------------------------

(defmethod SORT-RELATIONS-BY-TARGET-NAME ((relations LIST))
  (sort relations #'(lambda (r1 r2)
		     (string<= (concept-name (relation-target r1))
			       (concept-name (relation-target r2))))))

;;;-----------------------------------------------------------------------------
;;; SORT-RELATIONS-BY-WEIGHT
;;;-----------------------------------------------------------------------------

(defmethod SORT-RELATIONS-BY-WEIGHT ((relations LIST))
  (sort relations #'(lambda (r1 r2)
		     (>= (relation-weight r1) (relation-weight r2)))))

;;;-----------------------------------------------------------------------------
;;; MAP-RELATIONS
;;;-----------------------------------------------------------------------------

(defmethod MAP-RELATIONS ((graph GRAPH)(map-fn FUNCTION)
			  &key 
			  (relation-types nil))
  (map-graph-edges graph map-fn :edge-types relation-types))
  
;;;-----------------------------------------------------------------------------
;;; MAP-CONCEPT-RELATIONS
;;;-----------------------------------------------------------------------------

(defmethod MAP-CONCEPT-RELATIONS ((concept CONCEPT-MIXIN)(map-fn FUNCTION)
				  &key 
				  (relation-types nil))
  (map-graph-edges concept map-fn :edge-types  relation-types))

;;;-----------------------------------------------------------------------------

(defmethod MAP-CONCEPT-RELATIONS ((concept-name t)(map-fn FUNCTION)
				  &key 
				    (relation-types nil))
  (let ((concept (find-concept concept-name)))
    (when concept
      (map-concept-relations concept map-fn :relation-types relation-types))))

  
;;;-----------------------------------------------------------------------------
;;; PRINT-ALL-REATIONS
;;;-----------------------------------------------------------------------------

(defmethod PRINT-ALL-RELATIONS (&key (graph *default-net*))
  (map-relations graph #'print))

;;;****************************************************************************
;;; PART 4: REVERSE LINKS
;;;****************************************************************************

;;;-----------------------------------------------------------------------------
;;; ADD-RELATION-TYPE-ENTRY
;;;-----------------------------------------------------------------------------

;;; If either is registered, no new entry is created.

(defun ADD-RELATION-TYPE-ENTRY (relation-type reverse-relation-type
				&key 
				(graph *default-net*))
  ;; Check both since reverses need to be unique
  (unless (or (find-relation-type-entry relation-type graph)
	      (find-relation-type-entry reverse-relation-type graph))
    (push (list relation-type reverse-relation-type)
	  (reverse-relation-types graph))))

;;;-----------------------------------------------------------------------------
;;; FIND-RELATION-TYPE-ENTRY
;;;-----------------------------------------------------------------------------

(defun FIND-REVERSE-RELATION-TYPE (relation-type graph)
  (second (find-relation-type-entry relation-type graph)))

;;;-----------------------------------------------------------------------------

;;; This ensures we only need to store one pair of <name> & <reverse>.

(defun FIND-RELATION-TYPE-ENTRY (relation-type graph)
  (or (find relation-type (reverse-relation-types graph) :test #'eq :key #'first)
      (reverse (find relation-type (reverse-relation-types graph) :test #'eq :key #'second))))

;;;-----------------------------------------------------------------------------
;;; DELETE-RELATION-TYPE-ENTRY
;;;-----------------------------------------------------------------------------

;;; Delete entry.

(defun DELETE-RELATION-TYPE-ENTRY (relation-type graph)
  ;; Check both since reverses need to be unique
  (let* ((entry1 
	  (find relation-type (reverse-relation-types graph) :test #'eq :key #'first))
	 (entry2 
	  (find relation-type (reverse-relation-types graph) :test #'eq :key #'second))
	 (entry (or entry1 entry2)))
    (when entry
      (setf (reverse-relation-types graph)
	(remove entry (reverse-relation-types graph))))))

;;;-----------------------------------------------------------------------------
;;; ADD-REVERSE-RELATION
;;;-----------------------------------------------------------------------------

(defmethod ADD-REVERSE-RELATION ((relation-type SYMBOL)
				 (concept1 CONCEPT-MIXIN)
				 (concept2 CONCEPT-MIXIN)
				 &key
				 (graph *default-net*)
				 (class 'relation)
				 (increment-weight t))
  (let ((reverse-relation-type (find-reverse-relation-type relation-type graph)))
    (when reverse-relation-type
      (ensure-relation reverse-relation-type concept2 concept1 
		    :class class
		    :graph graph
		    ;; avoid cycling.
		    :no-reverse-relation t
		    :increment-weight increment-weight))))

;;;-----------------------------------------------------------------------------
;;; FIND-REVERSE-RELATION
;;;-----------------------------------------------------------------------------

(defmethod FIND-REVERSE-RELATION ((relation RELATION) &key (graph *default-net*))
  (let* ((reverse-relation-type
	  (find-reverse-relation-type (relation-type relation) graph))
	 (source (relation-source relation))
	 (target (relation-target relation)))
    (find-relation target reverse-relation-type source :graph graph)))

;;;-----------------------------------------------------------------------------
;;; DELETE-REVERSE-RELATION
;;;-----------------------------------------------------------------------------

(defmethod DELETE-REVERSE-RELATION ((relation RELATION) &key (graph *default-net*))
  (let ((reverse-relation (find-reverse-relation relation)))
    (when reverse-relation
      (delete-relation reverse-relation :graph graph))))

;;;****************************************************************************
;;; SAVE & RESTORE CONCEPTS
;;;****************************************************************************

;;;-----------------------------------------------------------------------------
;;; SAVE-SEMANTIC-NET
;;;-----------------------------------------------------------------------------

(defmethod SAVE-SEMANTIC-NET ((graph SEMANTIC-NET)(format (eql :lisp))(file-name STRING))
  (save-graph graph format file-name))

;;;-----------------------------------------------------------------------------
;;; RESTORE-SEMANTIC-NET
;;;-----------------------------------------------------------------------------

(defmethod RESTORE-SEMANTIC-NET ((format (eql :lisp))(file-name STRING))
  (restore-graph format file-name 
		 :graph-class 'semantic-net
		 :vertex-class 'concept
		 :edge-class 'relation ))


;;;****************************************************************************
;;; ISA LINKS
;;;****************************************************************************

;;;-----------------------------------------------------------------------------
;;; ADD-ISA-LINK
;;;-----------------------------------------------------------------------------

(defmethod ADD-ISA-LINK ((concept1 CONCEPT-MIXIN)(concept2 CONCEPT-MIXIN))
  (add-relation concept1 'ISA concept2))

;;;-----------------------------------------------------------------------------

(defmethod ADD-ISA-LINK ((concept1 CONCEPT-MIXIN)(concept2-name SYMBOL))
  (let ((concept2 (find-concept concept2-name)))
    (when concept2
      (add-isa-link concept1 concept2))))

;;;----------------------------------------------------------------------------
;;; CONCEPT-ISAS
;;;-----------------------------------------------------------------------------

(defmethod CONCEPT-ISAS ((concept CONCEPT-MIXIN))
  (mapcar #'relation-target 
	  (sort-relations-by-weight (find-relations (concept-name concept) 'isa))))

;;;-----------------------------------------------------------------------------

(defmethod CONCEPT-ISAS ((concept-name T))
  (let ((concept (find-concept concept-name)))
    (when concept
      (concept-isas concept))))

;;;-----------------------------------------------------------------------------
;;; CONCEPT-ISA-P
;;;-----------------------------------------------------------------------------

;;; This really needs to go up the ISA hierarchy.

(defmethod CONCEPT-ISA-P ((concept1 CONCEPT-MIXIN)(concept2 CONCEPT-MIXIN)
			  &optional (absolute-p t))
  (let ((links (copy-list (find-relations concept1 'ISA))))
    (when links
      (setf links (sort links #'(lambda (a b) (> a b)) :key #'relation-weight))
      (cond (absolute-p
	     (eq concept2 (relation-target (first links))))
	    (t
	     (member concept2 links :key #'relation-target :test #'eq))))))

;;;-----------------------------------------------------------------------------

(defmethod CONCEPT-ISA-P ((concept-name1 SYMBOL)(concept-name2 SYMBOL)
			  &optional (absolute-p t))
  (let ((concept1 (find-concept concept-name1))
        (concept2 (find-concept concept-name2)))
    (when (and concept1 concept2)
      (concept-isa-p concept1 concept2 absolute-p))))

;;;-----------------------------------------------------------------------------

(defmethod CONCEPT-ISA-P ((concept-name1 STRING)(concept-name2 STRING)
			  &optional (absolute-p t))
  (let ((concept1 (find-concept concept-name1))
        (concept2 (find-concept concept-name2)))
    (when (and concept1 concept2)
      (concept-isa-p concept1 concept2 absolute-p))))

;;;-----------------------------------------------------------------------------
;;; SAME-VALUE-P
;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((concept-1 CONCEPT) (string-2 STRING))
  (same-value-p (concept-name concept-1) string-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((string-1 STRING) (concept-2 CONCEPT))
  (same-value-p concept-2 string-1))


;;;-----------------------------------------------------------------------------
;;; VALUE-LESS-P
;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((concept-1 CONCEPT) (concept-2 CONCEPT))
  (string<= (symbol-name (concept-name concept-1)) 
           (symbol-name (concept-name concept-2))))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((concept-1 CONCEPT) (string-2 STRING))
  (string<= (symbol-name (concept-name concept-1)) string-2))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((string-1 STRING) (concept-2 CONCEPT))
  (string<= string-1 (symbol-name (concept-name concept-2))))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
