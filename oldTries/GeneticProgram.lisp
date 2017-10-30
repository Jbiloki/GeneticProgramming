isp 2.49

;;(progn
;; (do until current pool filled ;; init population
;; create new expr
;; add expr to current pool )
;; (do until exceed terminal generation count
;;; bump generation count
;; (for each expr in current pool
;; calc fitness for expr )
;; save a copy of most fit expr for this generation
;; (do until no more exprs in current pool
;; select 2 exprs as parents
;; remove parents from current pool
;; select crossover point/node in each parent
;;make crossed kids
;; expose each kid to mutattion
;;add kids to next pool )
;;current pool = next pool ))

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

																																									(defun get_fitness (rcritter)
																																									 "Get score for critter. Dummy fcn: just return its length."
																																									  (length rcritter))


																																									  (defun create_random_child ()
																																									      (setq child())
																																									          (setf op (random 3))
																																										      (if (= op 0) (setf newop '+))
																																										          (if (= op 1) (setf newop '-))
																																											      (if (= op 2) (setf newop '*))
																																											          (setf child (append child (list newop)))
																																												      (setf numele (random 3))
																																												          (loop while(>= numele 0) do
																																													          (setf curnum (random 13))
																																														          (cond
																																															              ((< curnum 10) (setf posneg (random 2)))
																																																                  ((= posneg 1) (- posneg))
																																																		              ((= curnum 10) (setq curnum 'x))
																																																			                  ((= curnum 11) (setq curnum 'y))
																																																					              ((= curnum 12) (setq curnum 'z)))
																																																						              (nconc child (list curnum))
																																																							              (decf numele))
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

																																																											    (defun select_crossover (mom0 mom1)
																																																											        (setq cell0 (random_tree_cell mom0))
																																																												    (setq cell1 (random_tree_cell mom1))
																																																												        (child0 (make_kid mom0 cell0 cell1))
																																																													    (child1 (make_kid mom1 cell1 cell0))
																																																													        (return-from select_crossover (list child0 child1))
																																																														    )


																																																														    (defun main (n term)
																																																														        (progn
																																																															        (setf curgen 0)
																																																																        
																																																																	        (setq pop ())
																																																																		        (loop while(< (length pop) n) do
																																																																			        (setf pop (append pop (list (create_random_child )))))
																																																																				        (loop while(< curgen term) do
																																																																					              (setf curgen (+ curgen 1))
																																																																						                    (setq best (pop_fitness pop))
																																																																								                  (setq best-sorted (safe_sort_scored_pop best))
																																																																										                (setq best-from-pop (last best-sorted))
																																																																												              (setq newpop ())
																																																																													                    (loop while(>= (length pop) 2) do
																																																																															                        (setq r-mom0 (random_tree_cell pop))
																																																																																		                    (setq pop (remove r-mom0 pop))
																																																																																				                        (setq r-mom1 (random_tree_cell pop))
																																																																																							                    (setq pop (remove r-mom1 pop))
																																																																																									                        (write pop)
																																																																																												                    (write (list r-mom0 r-mom1))
																																																																																														                        (setq newpop (append newpop (select_crossover r-mom0 r-mom1)))
																																																																																																	                    (write newpop))
																																																																																																			            )
																																																																																																				            ))
																																																																																																					    (main 50 10)



