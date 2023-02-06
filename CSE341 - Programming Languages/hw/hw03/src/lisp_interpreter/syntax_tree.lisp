(defun create-tree (data)
   (cons (cons data nil) nil))

(defun first-child (tree)
   (if (null tree) nil (cdar tree)))

(defun next-sibling (tree)
   (cdr tree))

(defun data (tree)
    (caar tree))

(defun add-child (tree child)
   (setf (car tree) (append (car tree) child)) tree)

;; sample for the g++ expression (+ v1 v2)
(setq op_op (create-tree "OP_OP"))
(setq op_cp (create-tree "OP_CP"))
(setq vf1 (create-tree "VALUEF"))
(setq vf2 (create-tree "VALUEF"))
(setq v1 (create-tree "5f1"))
(setq v2 (create-tree "3f1"))
(setq op_plus (create-tree "OP_PLUS"))
(setq exp0 (create-tree "EXP"))
(setq exp1 (create-tree "EXP"))
(setq exp2 (create-tree "EXP"))
(setq input (create-tree "INPUT"))
(setq start (create-tree "START"))

;; EXP := VALUEF
(add-child exp1 vf1)
(add-child vf1 v1)

(add-child exp2 vf2)
(add-child vf2 v2)

;; EXP := OP_OP OP_PLUS EXP EXP OP_CP
(add-child exp0 op_op)
(add-child exp0 op_plus)
(add-child exp0 exp1)
(add-child exp0 exp2)
(add-child exp0 op_cp)

;; INPUT := EXP
(add-child input exp0)

;; START := INPUT
(add-child start input)

(print start)