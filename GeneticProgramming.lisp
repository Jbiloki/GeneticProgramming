;gnu clisp 2.49

(setq x 0)
(setq y -2)
(setq z 1)
(setq output -16)


(setf *random-state* (make-random-state t))
(defun tree_nth_cell (rnth rtree)
  "Return the DFS N-th cell in the given tree: 1-based."
  (let ((size (cell_count rtree)))
    ;;(print (list :dbga rnth size (car rtree)))
    (cond
      ((not (integerp rnth)) nil)
      ((not (listp rtree)) nil) ;; Not a tree?
      ((null rtree) nil) ;; No tree elts?
      ((= 1 rnth) rtree) ;; 1st elt of list is the tree, itself.
      ((>= 0 rnth) nil) ;; Nth 0 or negative?
      ((> rnth size) nil) ;; N-th beyond tree's end?
      (t ;; Elt is in car subtree or cdr "subtree".
        (setq rnth (1- rnth)) ;;Account: Elt isn't the current (car+cdr's) node.
        (let ((size1 (cell_count (car rtree))))
          ;;(print (list :dbgb rnth size1 (car rtree)))
          (cond
          ((>= 0 size1) (tree_nth_cell ;; No car subtree.
            rnth
            (cdr rtree))) ;; Find elt in the cdr subtree.
          ((<= rnth size1) (tree_nth_cell ;; Elt is in car subtree.
            rnth
            (car rtree))) ;; Find elt in the car subtree.
          (t (tree_nth_cell ;; Elt is in cdr subtree.
            (- rnth size1) ;; Account for skipping car subtree.
            (cdr rtree))))))))) ;; Skip car subtree.



(defun tree_nth (rnth rtree)
  "Return the DFS N-th subtree/elt in the given tree."
  (let ((size (cell_count rtree)))
    ;; (print (list :dbga rnth size (car rtree)))
    (cond
      ((not (integerp rnth)) nil)
      ((not (listp rtree)) nil) ;; Not a tree?
      ((null rtree) nil) ;; No tree elts?
      ((= 1 rnth) (car rtree)) ;; 1st elt of list is its car subtree.
      ((>= 0 rnth) nil) ;; Nth 0 or negative?
      ((> rnth size) nil) ;; N-th beyond tree's end?
      ((= 1 size) (car rtree)) ;; 1st elt is Tree's car.
      (t ;; Elt is in car subtree or cdr "subtree".
        (setq rnth (1- rnth)) ;;Account: Elt isn't the current (car+cdr's) node.
        (let ((size1 (cell_count (car rtree))))
          ;; (print (list :dbgb rnth size1 (car rtree)))
          (cond
            ((>= 0 size1) (tree_nth ;; No car subtree.
                           rnth
                           (cdr rtree))) ;; Find elt in the cdr subtree.
            ((<= rnth size1) (tree_nth ;; Elt is in car subtree.
                              rnth
                              (car rtree))) ;; Find elt in the car subtree.
            (t (tree_nth ;; Elt is in cdr subtree.
                (- rnth size1) ;; Account for skipping car subtree.
                (cdr rtree))))))))) ;; Skip car subtree.

;(defun random_tree_cell (list)
;  "Returns a random element of LIST."
;  (if (not (and (list) (listp list)))
;      (return-from random_tree_cell (nth (random (1- (1+ (length list)))) list))
;    (error "Argument to get-random-element not a list or the list is empty")))

(defun random_tree_cell (rtree)
 "Return random cell in the tree, but not the whole tree."
  (setf rtree (flatten rtree))
  ;(write "TREE:")
  ;(write rtree)
  ;(terpri)
 (let* ((size (cell_count rtree))
 (rx (1+ (random (1- size)))) ;; Avoid 1st cell (the whole tree).
 (nth (1+ rx)) ;; Incr cuz our fcn is 1-based, not 0-based.
 (spot (tree_nth_cell nth rtree)))
 ;; (print (list :dbg size nth spot))
 spot))

(defun cell_count (rt)
  "Return the number of nodes/cells in the tree. Skip non-cells."
  (cond
    ((null rt) 0)
    ((not (listp rt)) 0)
    (t (let ((cc (length rt)))
      (+ cc (apply #'+ (mapcar #'cell_count rt)))))))



(defun make_kid (rmom rtgt rnew)
 "Return kid: copy of mom with tgt cell replaced by given new cell, or nil."
 (if (not (and rmom rtgt rnew
 (listp rmom)
 (listp rtgt)
 (listp rnew)))
 rmom
 (if (eq rmom rtgt)
 rnew
 (cons (make_kid (car rmom) rtgt rnew)
 (make_kid (cdr rmom) rtgt rnew)))))

(defun get_front_upto_nth ( rn rlist )
 "Return list head from 0-th thru N-th elt. Assumes elt-n is unique."
 (let ((elt-n (nth rn rlist)))
 (reverse (member elt-n (reverse rlist))))) 

;(defun get_fitness (rcritter)
; "Get score for critter. Dummy fcn: just return its length."
; (length rcritter))

(defun mutate_critter (critter)
    (setq new_critter critter)
    (setq change_pos (random (length critter)))
    ;(terpri)
    ;(write (nth change_pos new_critter))
    ;(terpri)
    (setq op (random 3))
    (setq curnum (- (random 22) 9))
    (cond 
        ((= change_pos 0)
        (if (= op 0) (setq newop '+))
        (if (= op 1) (setq newop '-))
        (if (= op 2) (setq newop '*))
        ;(if (eq (car critter) newop)
        ;(mutate_critter critter))
        (setq new_critter (append (list newop) (cdr critter))))
        ((> change_pos 0); (or (eq (nth change_pos new_critter) 'x)(eq (nth change_pos new_critter) 'y)(eq (nth change_pos new_critter) 'z)(numberp (nth change_pos new_critter))))
            ;(write "HERE")
            ;(write curnum)
            (cond
            ((= curnum -10) (setf curnum 'x ))
            ((= curnum -11) (setf curnum 'y ))
            ((= curnum -12) (setf curnum 'z ))
            ((= curnum 10) (setf curnum 'x ))
            ((= curnum 11) (setf curnum 'y ))
            ((= curnum 12) (setf curnum 'z )))
            ;(write "FINISHED")
            ;(terpri)
            ;(write curnum)
            ; (terpri)
            (setf (nth change_pos new_critter) curnum)))
            
    (return-from mutate_critter new_critter)
    )




(defun create_random_child (passed)
    (setq child passed)
    (setq op (random 3))
    (if (= op 0) (setq newop '+))
    (if (= op 1) (setq newop '-))
    (if (= op 2) (setq newop '*))
    (setf newlist (append '() (list newop)))
    (setf numele (random 3))
    (loop while(>= numele 0) do
        (setq curnum (- (random 22) 9))
        (case curnum
            (-10 (setq curnum 'x ))
            (-11 (setq curnum 'y ))
            (-12 (setq curnum 'z ))
            (10 (setq curnum 'x ))
            (11 (setq curnum 'y ))
            (12 (setq curnum 'z )))
        (nconc newlist (list curnum))
        (decf numele))
    
    (if (not passed)
       (setq child (append child  newlist))
       (setq child (append child (list newlist))))
    ;(print child)
    (setf try-again (random 5))
    (if (= try-again 0)
        (create_random_child child))
    (return-from create_random_child child))

(defun pop_fitness ( rpop ) ;; Pop is a population.
 "Create Pop-Scored pairs (Score Critter) given Pop list of critters."
 (mapcar #'(lambda (critter)
 (let ((score (get_fitness critter)))
 (list score critter)))
 rpop))

(defun safe_sort_scored_pop ( rscored-pop )
 "Return a sorted list of scored-critter elts. Don't change given list.
 NB, your Lisp's built-in sort fcn may damage the incoming list."
 (let ((sacrifice-list (copy-list rscored-pop)))
 (sort sacrifice-list
 #'(lambda (scored-critter-1 scored-critter-2)
 (< (car scored-critter-1) (car scored-critter-2))))))

(defun get_pop_from_scored (rscored-pop)
 "Return just the Pop of critters from the Scored Pop."
 ;;Alt: (mapcar #'(lambda (elt) (nth 1 elt)) rscored-pop)
 (mapcar #'cadr rscored-pop))

(defun flatten (structure)
  "Provided by https://rosettacode.org/wiki/Flatten_a_list#Common_Lisp"
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun get_fitness (rcritter)
 ; "Get score for critter. (We recommend summing the absolute deltas; without absolutes,
 ; some large negative deltas may cancel large positive deltas, and we want all deltas to 
 ; approach zero.)
 (setq delta (- (eval rcritter) output))
 (abs delta))

(defun mate_population (pop)
  (let* ((n0 (random (/ 50 2)))
         (n1 (random (/ 50 2)))
         (parent0 (nth n0 pop))
         (parent1 (nth n1 pop))
         (next-gen (append pop (select_crossover parent0 parent1))))
    (if (< (length pop) (- (length pop) 2))
      (mate_population next-gen)
      next-gen)))


(defun select_crossover (mom0 mom1)
    ;(write mom0)
    (terpri)
    (setq cell0 (random_tree_cell mom0))
    ;(write mom0)
    (terpri)
    (setq cell1 (random_tree_cell mom1))
    (setq child0 (make_kid mom0 cell0 cell1))
    (setq child1 (make_kid mom1 cell1 cell0))
    (setq mutate-or-not (random 10))
    (setq pick-child (random 2))
    (if (= mutate-or-not 0)
        (cond
            ((= pick-child 0)(setq child0 (mutate_critter child0)))
            ((= pick-child 1)(setq child1 (mutate_critter child1)))))
        (return-from select_crossover (list child0 child1))
    )



(defun main (n term)
    (progn
        (setf curgen 0)
        
        (setq pop ())
        (loop while(< (length pop) n) do
        (setf pop (append pop (list (create_random_child '())))))
        (loop while(< curgen term) do
              (setf curgen (+ curgen 1))
              (setq best (pop_fitness pop))
              (setq best-sorted (safe_sort_scored_pop best))
              (setq best-from-pop (first best-sorted))
              (write best-from-pop)
              (write pop)
              (setq best-next-gen (get_front_upto_nth (- (/ n 2) 1) best-sorted))
              (setq surviving-pop (get_pop_from_scored best-next-gen))
              (setq pop (mate_population surviving-pop))
              
              (terpri))
        ))
(main 50 20)


;;(progn
;; (do until current pool filled ;; init population xx
;; create new expr xx
;; add expr to current pool ) xx
;; (do until exceed terminal generation count xx
;;; bump generation count xx
;; (for each expr in current pool xx
;; calc fitness for expr ) xx
;; save a copy of most fit expr for this generation xx
;; (do until no more exprs in current pool
;; select 2 exprs as parents
;; remove parents from current pool
;; select crossover point/node in each parent
;;make crossed kids
;; expose each kid to mutattion
;;add kids to next pool )
;;current pool = next pool ))



                    ;(write "Population")
                    ;(write pop)
                    ;(terpri)
                    ;(write "Mom1")
                    ;(write r-mom0)
                    ;(terpri)
                    ;(write "Mom2")
                    ;(write r-mom1)
                    ;(terpri)
                    ;(write "Crossover")
                    ;(write (select_crossover r-mom0 r-mom1))
                    ;(terpri)


