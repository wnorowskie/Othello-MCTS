;; ========================================
;;  CMPU-365, Spring 2023
;;  Monte Carlo Tree Search -- TEMPLATE!
;; ========================================

;;  Contracts for the following functions used by MCTS algorithm
;; ----------------------------------------------------------
;;     GET-ROOT-NODE
;;     NEW-MC-TREE
;;     INSERT-NEW-NODE
;;     SIM-TREE
;;     SIM-DEFAULT (defined for you) 
;;     BACKUP
;;     UCT-SEARCH
;;     SELECT-MOVE

;;  In addition, for testing, the COMPETE function is defined for you.


;;  Your MCTS functions may call the following DOMAIN-DEPENDENT
;;  functions that are defined in "othello-starter.lisp":
;; ------------------------------------------------------------------
;;     COPY-GAME               -- creates a copy of the given othello game board
;;     MAKE-HASH-KEY-FROM-GAME -- returns list of the form (WHITE-PCS BLACK-PCS WHOSE-TURN)
;;     WHOSE-TURN              -- returns *BLACK* or *WHITE*

;;  Your MCTS functions may call the following DOMAIN-DEPENDENT
;;  functions that are defined in "othello-the-rest.lisp":
;; ------------------------------------------------------------------ 
;;     DO-MOVE!        --  does a move (destructively modifies game struct)
;;     LEGAL-MOVES     --  returns VECTOR of legal moves
;;     GAME-OVER?      --  returns T or NIL
;;     DEFAULT-POLICY  --  returns random legal move

;;  Your MCTS functions should not need to call any of the MACROs defined
;;  in "othello-macros.lisp".


;;  Note:  If a player has no legal moves, but the game isn't over, then that
;;         player *must* pass...


;;  MC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  KEY:          a hash-table key (compact rep'n of current state of game)
;;  WHOSE-TURN:   *BLACK* or *WHITE*
;;  NUM-VISITS:   the number of times this state has been visited
;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defstruct mc-node
  key             
  whose-turn      
  (num-visits 0)  
  veck-moves      
  veck-visits     
  veck-scores    
  )

;;  MC-TREE struct -- the MCTS tree
;; -------------------------------------------------------------
;;  HASHY:     a hash-table whose entries are (key,value), where
;;               key = compact repn of state, value = mc-node
;;  ROOT-KEY:  the hash-table key for the root node of the mcts tree

(defstruct mc-tree
  (hashy (make-hash-table :test #'equal))      
  root-key)

;;  GET-ROOT-NODE
;; ------------------------------------------------------
;;  INPUT:   TREE, a MCTS struct
;;  OUTPUT:  The MC-NODE corresponding to the root of the TREE

(defun get-root-node
    (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))

;; -------------------------------------------------
;;  Easiest to define the following functions
;;  in the following order (to facilitate testing)
;; -------------------------------------------------

;;  NEW-MC-TREE
;; ---------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived
;;           from GAME.

(defun new-mc-tree
    (game)
  (make-mc-tree :root-key (make-hash-key-from-game game)))

;;  INSERT-NEW-NODE
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.

(defun insert-new-node
    (game tree key)
  (let* ((moves (legal-moves game))
	 (num-moves (length moves))
	 (nodey (make-mc-node 
		 :key key
		 :veck-moves moves
		 :veck-visits (make-array num-moves :initial-element 0)
		 :veck-scores (make-array num-moves :initial-element 0)
		 :whose-turn (whose-turn game))))
    ;; insert nodey into tree
    (setf (gethash key (mc-tree-hashy tree)) nodey)
    ;; return the node
    nodey))

;;  SELECT-MOVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector, or NIL
;;           if there are no moves.

(defun select-move
    (nodey c)
  (let* ((player (mc-node-whose-turn nodey))
(moves (mc-node-veck-moves nodey))
(num-moves (length moves))
(best-index nil)
(hv 0)
(visits (mc-node-veck-visits nodey)))
    (cond
     ;; No legal moves!
     ((= num-moves 0)
      ;; signal failure
      nil)
     ;; Only one legal move
     ((= num-moves 1)
      ;; return it
      0)
     ;; Two or more moves
     (t

      (dotimes (i num-moves)
;; if move hasn't been done, return its index
(cond ((or (eq (aref visits i) 0)
  (eq (mc-node-num-visits nodey) 0))
      (return-from select-move i)))

(let* ((move-value (aref (mc-node-veck-scores nodey) i))
      (n-visits (mc-node-num-visits nodey))
      (move-visits (aref (mc-node-veck-visits nodey) i))
      (u-value (* c (sqrt (/ (log n-visits) move-visits)))))
 (if (equalp player *BLACK*)
      (cond ((< hv (+ move-value u-value))
     (setf best-index i)
     (setf hv (+ move-value u-value))))
   (cond ((> hv (- move-value u-value))
  (setf best-index i)
  (setf hv (- move-value u-value)))))))
      best-index))))
     

;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is represented by a KEY into the hashtable, and each move_i
;;    is an INDEX into the MOVES vector of the node assoc with state_i.
;;    NOTE:  The last statek is the state that was not previously in the tree
;;           but for which a new node will be created, and move_k (the last
;;           move) is selected from the info in that node.

(defun sim-tree
    (game tree c)
  (let (;; KEY-MOVE-ACC:  used to accumulate the list of STATE/KEYS and MOVES/INDICES
(key-move-acc nil)
;; HASHY:  the hash-table where KEY represents STATE; and VALUE is the corresponding NODE
(hashy (mc-tree-hashy tree)))
    ;; Keep going as long as the game is not over...
    (while (not (game-over? game))
      (let* (;; KEY:  Hash key for current state of game
    (key (make-hash-key-from-game game))
    ;; NODEY:  The MC-NODE corresponding to KEY (or NIL if not in tree)
    (nodey (gethash key hashy)))
;; Case 1:  When key not yet in tree...
(when (null nodey)
 ;; create a new node and insert it into the tree/hash-table
 ;; then select a final move to do; and DO it!
 ;; last entries pushed onto accumulator should be the key for the new node and
          ;;    then the selected move
 ;; return the reversed accumulator
 (let* ((n-node (insert-new-node game tree key))
(move-index (select-move n-node c))
(move (aref (mc-node-veck-moves n-node) move-index)))
   
   (do-move! game nil (first move) (second move))
   (setf key-move-acc (cons key-move-acc (cons key move))))
 (reverse key-move-acc))
;; Case 2:  Key already in tree (i.e., there's a node for this state already in tree)
;; select a move to do next, DO the move, push the KEY and MOVE-index onto the accumulator.
(let* ((move-index (select-move nodey c))
      (move (aref (mc-node-veck-moves nodey) move-index)))
 
   (do-move! game nil (first move) (second move))
   (setf key-move-acc (cons key-move-acc (cons key move))))))
    ;; After the WHILE... return the reverse accumulated key/move list
    (reverse key-move-acc)))
 
;;  SIM-DEFAULT -- defined for you!
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
    (game)
  (default-policy game))

;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup
    (hashy key-move-acc result)
  (dolist (key-move key-move-acc)
    (let* ((key (car key-move))
  (move (cdr key-move))
  (node (gethash key hashy))
  (index (position move (mc-node-veck-moves node)))
  (vv (aref (mc-node-veck-visits node) index))
  (cur-score (aref (mc-node-veck-scores node) index))
  ;; tune our score according to results
  (new-score (+ cur-score (/ (- result cur-score) vv))))
      ;; increment visits and set our new-score for the move
      (incf (mc-node-num-visits node) 1)
      (setf (aref (mc-node-veck-scores node) index) new-score)
      (incf (aref (mc-node-veck-visits node) index) 1))))

;;  UCT-SEARCH
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS.

;;  The following global parameter can be used to decide whether
;;  UCT-SEARCH should print out stats about the current round
;;  of MCTS.  The COMPETE function sets *verbose* to T; the
;;  COMPETE-NO-PRINTING function sets it to NIL.  

(defparameter *verbose* t)


(defun uct-search
    (orig-game num-sims c)
  (let* ((tree (new-mc-tree orig-game))
(hashy (mc-tree-hashy tree)))
    ;; Use DOTIMES for the number of simulations
    (dotimes (n num-sims)
      (let* ((c-game (copy-game orig-game))
    (acc (sim-tree c-game tree c))
    (result (if (game-over? c-game)
(if (equal (whose-turn c-game) *BLACK*) 1
  -1)
      (sim-default c-game))))
(backup hashy acc result)))
   
    ;; After all the simulations, select the best move (using c = 0 because we are not exploring anymore)
    ;; print out some stats
    ;; and return the move    
    (let* ((orig-node (gethash (make-hash-key-from-game orig-game) hashy))
 (best-move (select-move orig-node 0))
 (bm-pos (position best-move (mc-node-veck-moves orig-node)))
 (score (aref (mc-node-veck-scores orig-node) bm-pos)))

      ;; print stuff
      (when (*verbose*)
(format t "Best score: ~A~%" score)
(format t "Score veck: ~A~%" (mc-node-veck-scores orig-node))
(format t "Visits veck: ~A~%" (mc-node-veck-visits orig-node)))  
      ;; return best move
      best-move)))


;;  COMPETE -- defined for you!
;; ------------------------------------------------------------------------------
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           WHITE-NUM-SIMS, the number of simulations for each of white's moves
;;           WHITE-C, the exploration/exploitation constant used by white
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun compete
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
(format t "BLACK'S TURN!~%")
(format t "~A~%"
(apply #'do-move! g nil (uct-search g black-num-sims black-c))))
       (t
(format t "WHITE'S TURN!~%")
(format t "~A~%"
(apply #'do-move! g nil (uct-search g white-num-sims white-c))))))))


;;  COMPETE-NO-PRINTING
;; --------------------------------------------------
;;  Same as COMPETE, but only shows the end result

(defun compete-no-printing
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
(format t "B ")
(apply #'do-move! g nil (uct-search g black-num-sims black-c)))
       (t
(format t "W ")
(apply #'do-move! g nil (uct-search g white-num-sims white-c)))))
    (format t "~%~A~%" g)))




(defun group-in-tuples (lst)
  (labels ((group-helper (lst result)
             (if (null lst)
                 result
                 (let ((tuple (list (first lst) (second lst))))
                   (group-helper (nthcdr 2 lst) (cons tuple result))))))
    (reverse (group-helper lst nil))))