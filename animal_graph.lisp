;;Common graph file for both depth-first and breadth first to make use of

;;Makes a new statelist, with params as declared in the assignment
(defun make-state (FP GP WP CP)
  (list FP GP WP CP)
  )

;;Opposite side function
(defun opposite (letter)
  (if (equal letter 'e)
    'w
    'e
    )
  )

;;Checks to see if a state is safe
(defun isSafeState (state)
  (cond
    ((and (equal (goatLocation state) (wolfLocation state))
        (not (equal (farmerLocation state) (wolfLocation state)))) nil)
    ((and (equal (goatLocation state) (cabbageLocation state))
        (not (equal (farmerLocation state) (goatLocation state)))) nil)
    (t state)
        )
    )

;;Travelling functions
(defun moveAlone(state)
  (isSafeState
    (make-state
      (opposite (farmerLocation state))
      (goatLocation state)
      (wolfLocation state)
      (cabbageLocation state)
      )
    )
  )
;;only moves the animals when the farmer is on the same side as the animals
(defun moveGoat(state)
  (if (equal (farmerLocation state) (goatLocation state))
    (isSafeState
      (make-state
        (opposite (farmerLocation state))
        (opposite (goatLocation state))
        (wolfLocation state)
        (cabbageLocation state))
      )
    nil
    )
  )

(defun moveWolf(state)
  (if (equal (farmerLocation state) (wolfLocation state))
    (isSafeState
      (make-state
        (opposite (farmerLocation state))
        (goatLocation state)
        (opposite (wolfLocation state))
        (cabbageLocation state)
      )
    )
    nil
  )
)
;;Why does he need this cabbage so badly?
(defun moveCabbage(state)
  (if (equal (farmerLocation state) (cabbageLocation state))
    (isSafeState
      (make-state
        (opposite (farmerLocation state))
        (goatLocation state)
        (wolfLocation state)
        (opposite (cabbageLocation state)))
      )
    nil
    )
  )

;;GET functions for the locations of everyone in this crazy gang
(defun farmerLocation(state)
  (nth 0 state)
  )

(defun goatLocation(state)
  (nth 1 state)
  )

(defun wolfLocation(state)
  (nth 2 state)
  )

(defun cabbageLocation(state)
  (nth 3 state)
  )

;;Magic flatten by one function that takes away nils and extra brackets
(defun flattenByOne (listOfLists)
	(if listOfLists
		(append (car listOfLists) (flattenByOne (cdr listOfLists)))
		nil
		)
	)

;;Helper for the queue
(defun getAllPotentialMoves (state)
  (setq listOfPotentialStates nil)
  (if (moveAlone state)
    (setq listOfPotentialStates (append listOfPotentialStates (list (moveAlone state))))
    )
  (if (moveGoat state)
    (setq listOfPotentialStates (append listOfPotentialStates (list (moveGoat state))))
    )
  (if (moveWolf state)
    (setq listOfPotentialStates (append listOfPotentialStates (list (moveWolf state))))
    )
  (if (moveCabbage state)
    (setq listOfPotentialStates (append listOfPotentialStates (list (moveCabbage state))))
    )
  listOfPotentialStates
  )
  
;;Simplifies the print out function a bit, making for easier reading
;;Reused from previous assignment
(defun convertPathToString(pathList)
  (format t "A potential path is: ")
  (loop for i in pathList
    do(
        if (equal i (nth (- (length pathList) 1) pathList))
          (format t "~D" i)
          (format t "~D -> " i)
      )
    )
  (terpri)
  )
