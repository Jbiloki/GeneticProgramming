isp 2.49

;gnu clisp 2.49

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

(defun create_random_child ()
    (setq child())
        (setf op (random 3))
	    (if (= op 0) (string newop '+))
	        (if (= op 1) (setq newop "/"))
		    (if (= op 2) (setq newop "*"))
		        (append child newop)
			    (setf numele (random 3))
			        (loop while(>= numele 0) do
				            (decf numele)
					                )
							    (write child)
							        )

								(defun main (n)
								    (progn
								        (setq pop ())
									        (loop while(< (length pop) n) do
										          (setf pop (append pop (list (create_random_child )))))
											      ))
											      (main 50)
											      
