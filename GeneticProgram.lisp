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
															              (return-from create_random_child child)
																           )

																	   (defun pop_fitness ( rpop ) ;; Pop is a population.
																	    "Create Pop-Scored pairs (Score Critter) given Pop list of critters."
																	         (mapcar #'(lambda (critter)
																		      (let ((score (get_fitness critter)))
																		           (list score critter)))
																			        rpop))

																				(defun main (n term)
																				    (progn
																				            (setf curgen 0)
																					            
																						            (setq pop ())
																							            (loop while(< (length pop) n) do
																								            (setf pop (append pop (list (create_random_child )))))
																									            (loop while(< curgen term) do
																										                  (+ curgen 1)
																												                (write (pop_fitness pop)))
																														              )
																															              )
																																      (main 50 10)

