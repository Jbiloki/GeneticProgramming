;gnu clisp 2.49

;Authors/Contact Info:
;   Jacob Biloki
;   Email: Bilokij@csu.fullerton.edu
;   CWID: 891882573
;   Joshua Christ

; Program:
; This program implements genetic programming to generate a random population of critters. Over generations the best
; should be selected and crossed with a chance of mutation. They are judged by the distance from the target value and should
; converge to it.


;Set your golden critter here
(setq x -4)
(setq y -5)
(setq z -3)
(setq output 58)

;Set population size global variable
(setq pop-size 50)

;Declare our random seed
(setf *random-state* (make-random-state t))

;;; PROVIDED HELPER FUNCTIONS ;;;
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

(defun pop_fitness ( rpop ) ;; Pop is a population.
 "Create Pop-Scored pairs (Score Critter) given Pop list of critters."
 (mapcar #'(lambda (critter)
 (let ((score (get_fitness critter)))
 (list score critter)))
 rpop))

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

;;; END PROVIDED FUNCTIONS ;;;


(defun mutate_critter (critter)
    "We pass in a critter to mutate, changing either than operator or an operand"
    (setq new_critter critter) ;Create a copy of our critter
    (setq change_pos (random (length critter))) ;Randomly select the mutation point
    (setq op (random 3)) ;Randomly select the operation to change if selected
    (setq curnum (- (random 22) 9)) ;Randomly select the operand to replace old if selected
    (cond 
        ((= change_pos 0) ;If selected point is 0 we change the operator with a randomly selected one
        (if (= op 0) (setq newop '+))
        (if (= op 1) (setq newop '-))
        (if (= op 2) (setq newop '*))
        (setq new_critter (append (list newop) (cdr critter)))) ;Set the new critter to be prepended by the new operation
        ((> change_pos 0);If the position is not the operator we change with a number or variable at random
            (cond
            ((= curnum -10) (setf curnum 'x ))
            ((= curnum -11) (setf curnum 'y ))
            ((= curnum -12) (setf curnum 'z ))
            ((= curnum 10) (setf curnum 'x ))
            ((= curnum 11) (setf curnum 'y ))
            ((= curnum 12) (setf curnum 'z )))
            (setf (nth change_pos new_critter) curnum))) ;Set the new operand
    (return-from mutate_critter new_critter) ;Return the resulting new critter
    )




(defun create_random_child (passed)
    "Create a child at random"
    (setq child passed) ;Set our current child to the passed child
    (setq op (random 3)) ;Randomly select our operator
    (if (= op 0) (setq newop '+))
    (if (= op 1) (setq newop '-))
    (if (= op 2) (setq newop '*))
    (setf newlist (append '() (list newop))) ;Create a new list with our operator
    (setf numele (random 3)) ;Randomly select how many elements in our list
    (loop while(>= numele 0) do ;Insert random numbers/variables for each element
        (setq curnum (- (random 22) 9)) ;Get random number/variable
        (case curnum
            (-10 (setq curnum 'x ))
            (-11 (setq curnum 'y ))
            (-12 (setq curnum 'z ))
            (10 (setq curnum 'x ))
            (11 (setq curnum 'y ))
            (12 (setq curnum 'z )))
        (nconc newlist (list curnum)) ;Create a new list with our numbers
        (decf numele)) ;Decrement
    
    (if (not passed) ;If not NIL then we want to create a normal list else create a deep list
       (setq child (append child  newlist))
       (setq child (append child (list newlist))))
    (setf try-again (random 5)) ;Randomly try again to build a deeper list
    (if (= try-again 0)
        (create_random_child child))
    (return-from create_random_child child)) ;Return our critter




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
    "Pass the whole population to decide and mate"
  (let* ((n0 (random (/ pop-size 2))) ;Randomly select elements to mate
         (n1 (random (/ pop-size 2)))
         (parent0 (nth n0 pop))
         (parent1 (nth n1 pop))
         (pop (remove parent0 pop)) ;Remove from population after selection
         (pop (remove parent1 pop))
         (next-gen (append pop (select_crossover parent0 parent1)))) ;Append to pop our crossover child
    (if (< (length pop) (- pop-size 2))
      (mate_population next-gen)
      next-gen))) ;Return our new generation


(defun select_crossover (mom0 mom1)
    "Select the crossover of the children and cross"
    (setq cell0 (random_tree_cell mom0)) ;Select random cells to cross over
    (setq cell1 (random_tree_cell mom1))
    (setq child0 (make_kid mom0 cell0 cell1)) ;Make a kid based on the mom/cells
    (setq child1 (make_kid mom1 cell1 cell0))
    (setq mutate-or-not (random 10)) ;Randomly mutate
    (setq pick-child (random 2)) ;Randomly pick a child to mutate
    (if (= mutate-or-not 3)
        (cond
            ((= pick-child 0)(setq child0 (mutate_critter child0))) ;Mutate child0
            ((= pick-child 1)(setq child1 (mutate_critter child1))))) ;Mutate child1
        (return-from select_crossover (list child0 child1))
    )

(defun add_total_score (scored-pop)
    "Given the sorted population with fitness, return the total fitness, help from Scott Kim"
    (if (not (equal (length scored-pop) 0))
        (progn
            (+ (car (car scored-pop)) (add_total_score (cdr scored-pop)))) ;Add up the scored fitness
        0))

(defun main (n term)
    (progn
        (setf curgen 0) ;Set current generation to 0
        (setq pop ()) ;Initialize NIL population
        (loop while(< (length pop) n) do ;While pop is less than the pop-size add more critters
        (setf pop (append pop (list (create_random_child '()))))) ;Append a random critter to the population
        (loop while(< curgen term) do ;While generations are not done continue
              (setf curgen (+ curgen 1)) ;Add 1 to generation count
              (format t "Generation #~D" curgen)
              (terpri)
              (setq best (pop_fitness pop)) ;Get the population fitness
              (setq best-sorted (safe_sort_scored_pop best)) ;Sort the population by fitness
              (setq best-from-pop (first best-sorted)) ;Get the best from pop
              (setq worst-from-pop (last best-sorted)) ;Get worst from pop
              (setq avg-fitness (eval (/ (float (add_total_score best-sorted)) (float 50)))) ;Calculate average fitness of generation
              (format t "Average Fitness: ~#D" avg-fitness);Write avg fitness
              (terpri)
              (format t "Best from current generation: ~#D" best-from-pop)
              (terpri)
              (format t "Worst from current generation: ~#D" worst-from-pop)
              (terpri)
              (format t "Current generation:")
              (write pop)
              (terpri)
              (setq best-next-gen (get_front_upto_nth (- (/ n 2) 1) best-sorted))
              (setq surviving-pop (get_pop_from_scored best-next-gen))
              (setq pop (mate_population surviving-pop)))
        ))
(main pop-size 50) ;Start program from main
