(in-package :HYPERGRAPH)


;;;****************************************************************************
;;; BASIC TESTS FOR THE SEMANTIC NET API
;;;****************************************************************************

;;; This exmaple demonstrates meta relations.

(defun MAKE-SAMPLE-SEMANTIC-NET ()
  (let ((graph (make-semantic-net :name "Sample Semantic-Net"
				  :reverse-relation-types  '((related-to related-to)))))
    ;; Add relation types
    (add-relation-type-entry 'likes 'is-liked-by :graph graph)
    (add-relation-type-entry 'loves 'is-loved-by :graph graph)
    (add-relation-type-entry 'dislikes 'is-disliked-by :graph graph)
    (add-relation-type-entry 'resents 'is-resented-by :graph graph)
    (add-relation-type-entry 'is-married-to 'is-married-to :graph graph)
    ;; Add some concepts
    (let* ((john (ensure-concept "John" :graph graph))
	   (jill (ensure-concept "Jill" :graph graph))
	   (jack (ensure-concept "Jack" :graph graph))
	   (mom  (ensure-concept "Mom" :graph graph))
	   (likes1 (add-relation john 'likes jack :graph graph))
	   (likes2 (add-relation jack 'likes john :graph graph))
	   (loves (add-relation john 'loves jill :graph graph))
	   (marriage (add-relation jill 'is-married-to jack :graph graph))
	   (resents (add-relation john 'resents marriage :graph graph))
	   (dislikes (add-relation mom 'dislikes resents :graph graph)))
      graph)))