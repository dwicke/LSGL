(defun main (gamepath statpath)
  (let ((agents '()) (games '()) (cur-game '()) (num-iter 0) (count-state-zero 0))
	
    ;; First make the game
    (with-open-file (stream gamepath)

    ;  (setf params (cons (read stream) params))
     ; (setf params (cons (read stream) params))
      
   ;   (setf params (reverse params));; reverse so 1st is 1st
   ;   (print params)
      (setf num-iter (read stream))
      (setf games (read stream));; read the game list
      ;; so now build the agents
     ; (setf agents (cons (apply (getf (elt params 0) :builder) (list games 0 statpath (elt params 0))) agents))
      ;(setf agents (cons (apply (getf (elt params 1) :builder) (list games 1 statpath (elt params 1))) agents))

      (setf agents (read-agents stream games statpath))

     ; (setf agents (reverse agents))
      

      ;; then the game loop
      (setf cur-game 0) ;; set the current game to be played
      (dotimes (i num-iter)
	(let ((actions '()) (rewards '()) (next-game '()))
	  ;; get the action from each agent given the game
	  (setf actions (mapcar #'(lambda (agent)  
				    (apply (agent-select agent ) (list cur-game agent games i) )) 
				agents))
	  ;; compute the next game using the transition probabilities and the joint action
	  (setf next-game (get-next-game cur-game actions games))
	  
	  (if (= next-game 0)
	      (setf count-state-zero (+ 1 count-state-zero)))
	  ;; hand out the reward to each agent by passing the reward and the current and next states
	  (setf rewards (get-rewards (elt games cur-game) actions))
	  
	  
	  ;; pass everything since depending on the algorithm might need different things.
	  (mapcar #'(lambda (agent) 
		      (apply (agent-update agent) (list agent games cur-game next-game rewards actions )))
		  agents)
	  ;; set cur-game to next-game
	  (setf cur-game next-game)
	  )
	)
      (print count-state-zero)
      agents

      
      )
    )
  )


(defun read-agents (stream games statpath)
  (let ((agents '()) (params '()) (num-agents 0))
    (dolist (game games)
      (if (> (length (butlast game)) num-agents)
	  (setf num-agents (length (butlast game)))))
    (reverse (dotimes (i num-agents agents)
	       (setf params  (read stream))
	        (setf agents (cons (apply (getf params :builder) (list games i statpath params)) agents))
     
      ))))


;; competitive games
(defun test-main ()
  (main "/home/drew/GMU/spring2013/mas/repeat.gt"  "/home/drew/GMU/spring2013/mas/" ))

(defun test-stackelberg ()
  (main "/home/drew/GMU/spring2013/mas/stackelberg.gt"  "/home/drew/GMU/spring2013/mas/"))

(defun test-matchpen ()
  (main "/home/drew/GMU/spring2013/mas/matchpen.gt"  "/home/drew/GMU/spring2013/mas/"))

(defun test-rock ()
  (main "/home/drew/GMU/spring2013/mas/rock.gt"  "/home/drew/GMU/spring2013/mas/"))

(defun test-tax () ;; stochastic with deterministic transitions
  (main "/home/drew/GMU/spring2013/mas/taxmodel.gt"  "/home/drew/GMU/spring2013/mas/"))

(defun test-power () ;; fully stochastic
    (main "/home/drew/GMU/spring2013/mas/power.gt"  "/home/drew/GMU/spring2013/mas/"))

;;coopertive games:

(defun test-climb () ;;stochastic climb
    (main "/home/drew/GMU/spring2013/mas/stochclimb.gt"  "/home/drew/GMU/spring2013/mas/"))

(defun test-regclimb ()
    (main "/home/drew/GMU/spring2013/mas/regclimb.gt"  "/home/drew/GMU/spring2013/mas/"))

(defun test-penalty ()
    (main "/home/drew/GMU/spring2013/mas/penalty.gt"  "/home/drew/GMU/spring2013/mas/"))





;; main helper functions

;; cur-game is the index of the current game in games
;; actions is the list of actions each player took
;; games is the list of possible games
(defun get-next-game (cur-game actions games) 
  ;; get the next game matrix based on the transition probabilities
  ;; of the current game by using the spinner
    (spinner (elt (joint-action (elt games cur-game) actions) (length actions)))
)

;; game is the current game matrix
;; actions is the list of actons each player took
;; returns a list of rewards each index matches the 
;; id of the player
(defun get-rewards (game actions)
  (butlast (joint-action game actions))
)

;; Utility functions

(defun spinner (probs)
  (let ((rand '()) (idx (- (length probs) 1)))
   (setf rand (random 1.0))
    ;; make the policy like a pie and choose the action the spinner lands in
    (dotimes (i (length probs) idx)
      (if (<= rand (elt probs i))
	  (return i)
	  (setf rand (- rand (elt probs i)))))
    )
)

;; produces a list of size n filled with random numbers between 0 and k
(defun rand-list (n k)
  (let ((rn '()))
    (dotimes (i n rn)
      (setf rn (cons (random k) rn)))))

;; get all of the possitions 
(defun all-pos (val all-vals)
  (loop
    for element in all-vals 
    and position from 0
     when (eql element val)
      collect position))

;; applies the boltzmann distribution
;; to the vals list
(defun boltz-dist (vals tem)
  (let* ((denom (mapcar #'(lambda (x) (exp (/ x tem))) vals))
	 (sum (apply '+ denom)))
  (mapcar #'(lambda (x) (/ (exp (/ x tem)) sum)) vals )))


;; given the game matrix and the actions
;; played return the list that corresponds
;; to the joint action in the game matrix
;(defun joint-action (game actions)
  ;; so the first player is row and second is column
  ;; need to offset from 0 since the dim is not included in the matrix
  ;; then use the formula row * numCols + column since row major
 ; (elt (elt game (length actions)) (+ (* (elt actions 0) (elt game 1)) (elt actions 1)))
;)


;; given the game matrix and the actions
;; played return the list that corresponds
;; to the joint action in the game matrix
;; now can have n-players
(defun joint-action (game actions)
     (elt (elt game (length actions)) (apply #'+ (maplist #'(lambda (x y)
                            (* (car x) (apply #'* (cdr y))))
                       actions
                        (concatenate 'list (butlast game) '(1)))))
)

(defun remove-paren (str)
  (remove #\( (remove #\) str)))



;; Agent structure:

(defstruct agent
  id
  total-util
  q-table ;; ( (game 1 / state 1) ... ) game 1 contains the payoff (qval) for each action
  policy  ;; same structure as q-table except prob of playing that action
  select ;; a function handle to select action
  update  ;; a function handle called to update the agent
  ;; a list of learning parameters
  params 
  stats ;; the file that stores the stats for the agent
)

(defun getparam (agent key)
  (getf (agent-params agent) key))

;; Agent functions:


;; Policy Hill Climb agent:

(defun build-phc (games id statpath params) 
  (let ((agent '())) ;; the agent I am building
    ;(setf agent (make-agent :id id :total-util 0 :select #'phc-select :update #'phc-update :params (list :gamma 0.1 :alpha 0.1 :delta 0.1)))
    (setf agent (make-agent :id id :total-util 0 :select #'phc-select :update #'phc-update :params params))
    ;; now set the policy table and build the q-table
    (dolist (game games agent)
      (setf (agent-q-table agent) (cons (make-list (elt game (agent-id agent)) :initial-element 0) (agent-q-table agent) ))
      (setf (agent-policy agent) (cons (make-list (elt game (agent-id agent)) :initial-element (/ 1 (elt game (agent-id agent)))) (agent-policy agent)))
    )
    ;; set the filename
    (setf (agent-stats agent) (concatenate 'string statpath "out" (write-to-string (agent-id agent)) ".stat"))
    (print (agent-stats agent))
    agent
  )
)

;; state-id the current state index into games
;; agent that is choosing the action
(defun phc-select (state-id agent games time)
  ;; need to add in exploration otherwise we will get stuck  
  
  (if (= (mod time 1000) 0)

 ;; write policy to file.
  (with-open-file (stream (agent-stats agent) :direction :output :if-does-not-exist :create :if-exists :append)
     (format stream "~d ~{~f ~}~%" state-id (elt (agent-policy agent) state-id))))
(spinner (elt (agent-policy agent) state-id))

)


(defun phc-update (agent games state-id next-state-id rewards actions)
  ;; update Q table value for the 
  (let* ((reward (elt rewards (agent-id agent))) 
	 (action (elt actions (agent-id agent)))
	 (qval (elt (elt (agent-q-table agent) state-id) action))
	 (alpha (getf (agent-params agent) :alpha))
	 (gamma (getf (agent-params agent) :gamma))
	 (policy (elt (agent-policy agent) state-id)) )
    ;; set the q-val
    (setf (elt (elt (agent-q-table agent) state-id) action) 
	  (+ (* (- 1 alpha)  qval) (* alpha (+ reward (* gamma (apply 'max (elt (agent-q-table agent) next-state-id)))))))
    
     

    ;; so now update the policy!
    (setf  (elt policy action) (+ (elt policy action) 
				  ;; check if the action taken is within the list of best actions
				  (if (position action (all-pos (apply 'max (elt (agent-q-table agent) state-id)) (elt (agent-q-table agent) state-id) ))
				      ;; so the action taken was the best action so update by delta
				      (getf (agent-params agent) :delta)
				      ;; otherwise it wasn't so update by subtracting a fraction of delta
				      (/ (- 0 (getf (agent-params agent) :delta)) (- (length (elt (agent-q-table agent) state-id)) 1)))))
    ;; now renormalize the policy.  need it to sum to 1.     
    (setf (elt (agent-policy agent) state-id) (boltz-dist (elt (agent-policy agent) state-id) 1))
    ;; make sure to zero things that are too small so that floating point stuff doesn't mess it up
    (setf  (elt (agent-policy agent) state-id) (mapcar #'(lambda (x) (if (< x 1e-10) 0.0 x))  (elt (agent-policy agent) state-id)))
					; (print (elt (agent-policy agent) state-id))
       

    )
)

;; Chicken agent

(defun build-chicken (games id statpath params)
  (let ((agent '())) ;; the agent I am building
    (setf agent (make-agent :id id :total-util 0 :select #'chicken-select :update #'chicken-update :params params))
   
))

(defun chicken-select (state-id agent games time)
  1 ;; always dare!!
)

(defun chicken-update (agent games state-id next-state-id rewards actions)
  (incf (agent-total-util agent) (elt rewards (agent-id agent))) ) ;; just update my util so i see how good i do.


;; so build a lenient learner for stochastic games
(defun build-lenient (games id statpath params)
  (let ((agent '())) ;; the agent I am building
    (setf agent (make-agent :id id :total-util 0 :select #'lenient-select :update #'lenient-update :params params))
    ;; now set the policy table and build the q-table
    (dolist (game games agent)
      (setf (agent-q-table agent) (cons (rand-list (elt game (agent-id agent)) .001) (agent-q-table agent) ))
      (setf (agent-policy agent) (cons (make-list (elt game (agent-id agent)) :initial-element (getf (agent-params agent) :maxtemp) ) (agent-policy agent)))
    )
    ;; set the filename
    (setf (agent-stats agent) (concatenate 'string statpath "lenient-out-" (write-to-string (agent-id agent)) ".stat"))
    (print (agent-stats agent))
    agent
  )
)

(defun lenient-select (state-id agent games time)

  ;; first calculate mintemp
  (let* ((mintemp (+ .000001 (apply 'min (elt (agent-policy agent) state-id))))
	 ;; calculate the W
	 (W (boltz-dist (elt (agent-q-table agent) state-id) mintemp))
	 (action (spinner W)))
    ;; update the temp[s,i] = temp[s,i] * delta
    (setf (elt (elt (agent-policy agent) state-id) action) (* (elt (elt (agent-policy agent) state-id) action) (getf (agent-params agent) :delta)))
    ;; write the distribution W to the file
    (with-open-file (stream (agent-stats agent) :direction :output :if-does-not-exist :create :if-exists :append)
      (format stream "~d ~{~f ~}~%" state-id W))
    ;; return the action
    action
    )
)
(defun lenient-update (agent games state-id next-state-id rewards actions)

  (let* ((rand-val (random 1.0))
	 (action (elt actions (agent-id agent)))
	 (reward (elt rewards (agent-id agent)))
	 (cur-util (elt (elt (agent-q-table agent) state-id) action))
	 (next-max-util (apply 'max (elt (agent-q-table agent) next-state-id)))
	 (temp (elt (elt (agent-policy agent) state-id) action)))
		   
    (if (or (<= cur-util reward) (< rand-val (+ .01 (expt (getparam agent :beta) (* (- 0 (getparam agent :alpha)) temp)))))
	(setf (elt (elt (agent-q-table agent) state-id) action)
	      ;; same equation as the one in the paper but factored in the max util of the next state
	      (+ (* (getparam agent :lambda) cur-util) (* (- 1 (getparam agent :lambda)) (+ reward (* (getparam agent :gamma) next-max-util))))))
    
    
)
)


(defun build-wolf (games id statpath params) 
  (let ((agent '())) ;; the agent I am building
    ;(setf agent (make-agent :id id :total-util 0 :select #'phc-select :update #'phc-update :params (list :gamma 0.1 :alpha 0.1 :delta 0.1)))
    ; need to add the average policy and the C value to the params list since they are not standard
    
    (setf agent (make-agent :id id :total-util 0 :select #'wolf-select :update #'wolf-update :params params))
    (setf (agent-params agent) (concatenate 'list params '(:C '() :avg-pol nil)));; need to add the other parameters
    (setf (getf (agent-params agent) :C) (make-list (length games) :initial-element 0))

    ;; now set the policy table and build the q-table
    (dolist (game games agent)
      (setf (getf (agent-params agent) :avg-pol) (cons (make-list (elt game (agent-id agent)) :initial-element 0) (getparam agent :avg-pol)))
      (setf (agent-q-table agent) (cons (make-list (elt game (agent-id agent)) :initial-element 0) (agent-q-table agent) ))
      (setf (agent-policy agent) (cons (make-list (elt game (agent-id agent)) :initial-element (/ 1 (elt game (agent-id agent)))) (agent-policy agent)))
    )

    ;; since cons I need to reverse the elements.
    (setf (agent-q-table agent) (reverse (agent-q-table agent)))
    (setf (agent-policy agent) (reverse (agent-policy agent)))
    (setf (getf (agent-params agent) :avg-pol) (reverse (getf (agent-params agent) :avg-pol)))
    ;; set the filename
    (setf (agent-stats agent) (concatenate 'string statpath "out" (write-to-string (agent-id agent)) ".stat"))
    (print (agent-stats agent))
    agent
  )
)

;; state-id the current state index into games
;; agent that is choosing the action
(defun wolf-select (state-id agent games time)
  ;; need to add in exploration otherwise we will get stuck  
  
  (if (= (mod time 1000) 0)
 ;; write policy to file.
  (with-open-file (stream (agent-stats agent) :direction :output :if-does-not-exist :create :if-exists :append)
     (format stream "~d ~{~f ~}~%" state-id (elt (agent-policy agent) state-id)) ) )

  ;; check out the average policy...
  (if (= (mod time 1000) 0)
 ;; write policy to file.
  (with-open-file (stream (concatenate 'string  (agent-stats agent) ".avg") :direction :output :if-does-not-exist :create :if-exists :append)
     (format stream "~d ~{~f ~}~%" state-id (elt (getf (agent-params agent) :avg-pol) state-id) ) ) )


  (cond ((and (not (null(getparam agent :epsilon)))
	      (< (random 1.0) (getparam agent :epsilon)))
	 ;; use a random policy if using epsilon
	 (spinner (mapcar #'(lambda (x) (+ 1 x)) (rand-list (length (elt (agent-policy agent) state-id)) 2.0)) ))
	
	(t (spinner (elt (agent-policy agent) state-id))));; get the action
)

(defun wolf-update (agent games state-id next-state-id rewards actions)
  ;; update Q table value for the 
  (let* ((reward (elt rewards (agent-id agent))) 
	 (action (elt actions (agent-id agent)))
	 (qval (elt (elt (agent-q-table agent) state-id) action))
	 (qvals (elt (agent-q-table agent) state-id))
	 (alpha (getf (agent-params agent) :alpha))
	 (gamma (getf (agent-params agent) :gamma))
	 (policy (elt (agent-policy agent) state-id))
	 (avg-pol (elt (getparam agent :avg-pol) state-id))
	 (C (elt (getparam agent :C) state-id))
	 (exp-act '()))
    ;; set the q-val this is the same as in PHC
    (setf (elt (elt (agent-q-table agent) state-id) action) 
	  (+ (* (- 1 alpha)  qval) (* alpha (+ reward (* gamma (apply 'max (elt (agent-q-table agent) next-state-id)))))))
    
    ;; update C(s) by adding 1 to it
    (setf (elt (getf (agent-params agent) :C) state-id) (+ 1 C))

    ;; update the average policy
    (setf (elt (getf (agent-params agent) :avg-pol) state-id) 
	  (mapcar #'(lambda (avgp actp) (+ avgp (* (/ 1 (elt (getparam agent :C) state-id)) (- actp avgp)))) avg-pol policy))
    

    ;; now calculate the expected action
    (setf exp-act  (apply '+ (mapcar #'(lambda (x y) (* x y) ) policy qvals)))

    ;; now update the delta
    (setf (getf (agent-params agent) :delta) (if (> exp-act 
						    (apply '+ (mapcar #'(lambda (x y) (* x y) ) 
								      qvals (elt (getf (agent-params agent) :avg-pol) state-id))))
						 (getparam agent :deltaw)
						 (getparam agent :deltal)))
    
    ;; so now update the policy!
    (setf  (elt policy action) (+ (elt policy action) 
				  ;; check if the action taken is within the list of best actions
				  ;; not the best way to do this at all.  should have gotten the q-val for the action and checked >= to all else.
				  (if (position action (all-pos (apply 'max (elt (agent-q-table agent) state-id)) (elt (agent-q-table agent) state-id) ))
				      ;; so the action taken was the best action so update by delta
				      (getf (agent-params agent) :delta)
				      ;; otherwise it wasn't so update by subtracting a fraction of delta
				      (/ (- 0 (getf (agent-params agent) :delta)) (- (length (elt (agent-q-table agent) state-id)) 1)))))
    ;; now renormalize the policy  using boltzmann distribution.
    (setf (elt (agent-policy agent) state-id) (boltz-dist (elt (agent-policy agent) state-id) 1))
    ;; make sure to zero things that are too small so that floating point stuff doesn't mess it up
    (setf  (elt (agent-policy agent) state-id) (mapcar #'(lambda (x) (if (< x 1e-10) 0.0 x))  (elt (agent-policy agent) state-id)))
    )
  )


(defun zeros-list(n)
  (make-list n :initial-element 0))


(defun build-dql (games id statpath params) 
  (let ((agent '())) ;; the agent I am building
    ;(setf agent (make-agent :id id :total-util 0 :select #'phc-select :update #'phc-update :params (list :gamma 0.1 :alpha 0.1 :delta 0.1)))
    ; need to add the average policy and the C value to the params list since they are not standard
    
    (setf agent (make-agent :id id :total-util 0 :select #'dql-select :update #'dql-update :params params))
    (setf (agent-params agent) (concatenate 'list params '(:cur-time 0 :damp 1 :rewards nil :next-states nil )));; need to add the other parameters
    (setf (getf (agent-params agent) :C) (make-list (length games) :initial-element 0))

    ;; now set the policy table and build the q-table
    (dolist (game games agent)
      (setf (getf (agent-params agent) :avg-pol) (cons (make-list (elt game (agent-id agent)) :initial-element 0) (getparam agent :avg-pol)))
      (setf (agent-q-table agent) (cons (make-list (elt game (agent-id agent)) :initial-element 0) (agent-q-table agent) ))
      (setf (getf (agent-params agent) :rewards) (cons (make-list (elt game (agent-id agent)) :initial-element '(0 0)) (getf (agent-params agent) :rewards) ))
      ;; so make a list like (((state0 state1 ...(for action 0 in state 0))...
      (setf (getf (agent-params agent) :next-states) 
	    (cons (make-list (elt game (agent-id agent)) :initial-element (zeros-list (length games))) (getf (agent-params agent) :next-states) ))
      (setf (agent-policy agent) (cons (make-list (elt game (agent-id agent)) :initial-element (/ 1 (elt game (agent-id agent)))) (agent-policy agent)))
    )

    ;; since cons I need to reverse the elements.
    (setf (agent-q-table agent) (reverse (agent-q-table agent)))
    (setf (agent-policy agent) (reverse (agent-policy agent)))
    (setf (getf (agent-params agent) :avg-pol) (reverse (getf (agent-params agent) :avg-pol)))
    (setf (getf (agent-params agent) :rewards) (reverse (getf (agent-params agent) :rewards)))
    (setf (getf (agent-params agent) :next-states) (reverse (getf (agent-params agent) :next-states)))
    ;; set the filename
    (setf (agent-stats agent) (concatenate 'string statpath "out" (write-to-string (agent-id agent)) ".stat"))
    (print (agent-stats agent))
    agent
  )
)


;; state-id the current state index into games
;; agent that is choosing the action
(defun dql-select (state-id agent games time)
  ;; need to add in exploration otherwise we will get stuck  
  
  (if (= (mod time 1000) 0)

 ;; write policy to file.
  (with-open-file (stream (agent-stats agent) :direction :output :if-does-not-exist :create :if-exists :append)
     (format stream "~d ~{~f ~}~%" state-id (elt (agent-policy agent) state-id))))


  
  ;; inc counter
  (incf (getf (agent-params agent) :cur-time))
  ;; get action
  (spinner (elt (agent-policy agent) state-id))

)

(defun reset-dql-rewards (agent games)
  (setf (getf (agent-params agent) :rewards) nil)
  (dolist (game games agent)
      (setf (getf (agent-params agent) :rewards)  (cons (make-list (elt game (agent-id agent)) :initial-element '(0 0)) (getf (agent-params agent) :rewards) ))
    )
  (setf (getf (agent-params agent) :rewards) (reverse (getf (agent-params agent) :rewards)))
)


(defun dql-update (agent games state-id next-state-id rewards actions)
  ;; update Q table value for the 
  (let* ((reward (elt rewards (agent-id agent))) 
	 (action (elt actions (agent-id agent)))
	 (qval (elt (elt (agent-q-table agent) state-id) action))
	 (alpha (getf (agent-params agent) :alpha))
	 (gamma (getf (agent-params agent) :gamma))
	 (policy (elt (agent-policy agent) state-id)) )

    ;; update the action reward value and increment the number of times action was performed
    (incf (elt (elt (elt (getf (agent-params agent) :rewards) state-id) action) 0) reward);; reward
    (incf (elt (elt (elt (getf (agent-params agent) :rewards) state-id) action) 1));; count

    ;; update the count for which state I enter by taking the action
    (incf (elt (elt (elt (getf (agent-params agent) :next-states)  state-id ) action) next-state-id))
    

    ;; so if the cur-time >= max-delay
    ;; then if max-delay is zero don't do anything
    ;; if not then set max-delay = e^-damp/4*cos(2*pi*damp) * max-delay
    ;; increase damp.
    ;; in either case
    ;; set cur-time to zero.
    ;; then proceed to calculate the averages for each of the actions
    ;; calculating the q-value using that averaged reward
    ;; once
    ;; then updating the policy.
    ;; finally reset rewards
    (when (>= (getparam agent :cur-time) (getparam agent :max-delay))
      (if (> (getparam agent :max-delay) 0)
	  (setf (getf (agent-params agent) :max-delay)
		(floor (* (getparam agent :max-delay) (exp (/ (- (getparam agent :damp)) 4.0)) (cos (* 2 pi (getparam agent :damp)))))))
      ;; increment damp
      (incf (getf (agent-params agent) :damp))
      ;; set cur-time to zero
      (setf (getf (agent-params agent) :cur-time) 0)
      (cond ((> (getparam agent :cur-time) 1)
	     ;; calculate the averages of the rewards for each action in all states
	     (dotimes (cstate (length games))
	       (setf (elt (getf (agent-params agent) :rewards) cstate)
		     (mapcar #'(lambda (x) (/ (first x) (second x))) 
			     (elt (getf (agent-params agent) :rewards) cstate))))
	     
	     ;; calculate the most likely next state for each action
	     (let ((next-st-id nil))
	       (dotimes (cstate (length games))
		 ;; in the future might want to break ties with the max randomly...
		 (setf next-st-id (cons (mapcar #'(lambda (x) (position (apply 'max x) x)) (elt (getparam agent :next-states) cstate)) next-st-id)))
	       (setf next-st-id (reverse next-st-id));; need to reverse since consing
	       
	
	       
	       ;; update the q-table
	       (dotimes (cstate (length games));; loop over the games
		 (dotimes (act (length (elt (agent-q-table agent) cstate)))
		   
		   (setf (elt (elt (agent-q-table agent) cstate) act) 
			 (+ (* (- 1 alpha)  (elt (elt (agent-q-table agent) cstate) act) ) (* alpha (+ (elt (elt (getparam agent :rewards) cstate) act) (* gamma (apply 'max (elt (agent-q-table agent) (elt (elt next-st-id cstate) act))))))))
		   )))
	     )

      

	    (t
    ;; set the q-val
    (setf (elt (elt (agent-q-table agent) state-id) action) 
	  (+ (* (- 1 alpha)  qval) (* alpha (+ reward (* gamma (apply 'max (elt (agent-q-table agent) next-state-id)))))))))
    
     

    ;; so now update the policy!
      (setf  (elt policy action) (+ (elt policy action) 
				  ;; check if the action taken is within the list of best actions
				  (if (position action (all-pos (apply 'max (elt (agent-q-table agent) state-id)) (elt (agent-q-table agent) state-id) ))
				      ;; so the action taken was the best action so update by delta
				      (getf (agent-params agent) :delta)
				      ;; otherwise it wasn't so update by subtracting a fraction of delta
				      (/ (- 0 (getf (agent-params agent) :delta)) (- (length (elt (agent-q-table agent) state-id)) 1)))))
    ;; now renormalize the policy.  need it to sum to 1.     
    (setf (elt (agent-policy agent) state-id) (boltz-dist (elt (agent-policy agent) state-id) 1))
    ;; make sure to zero things that are too small so that floating point stuff doesn't mess it up
    (setf  (elt (agent-policy agent) state-id) (mapcar #'(lambda (x) (if (< x 1e-10) 0.0 x))  (elt (agent-policy agent) state-id)))
    
    ( reset-dql-rewards agent games);; must reset the rewards
					; (print (elt (agent-policy agent) state-id))
       
    )
    )
)