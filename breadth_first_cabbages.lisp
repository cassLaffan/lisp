(load "animalGraph.lisp")

;;breadth first, none recursive
(defun findPath (start goal)
  (setq pathList nil)
  (setq stateQueue nil)
  (setq pathQueue nil)

  ;;initial states
  (setq tempPaths (getAllPotentialMoves start))
  (loop for i in tempPaths
    do(setq stateQueue (append stateQueue (list i)))
    )

  (loop for i in tempPaths
    do(setq pathQueue (list (append pathQueue (list start))))
    )

  (loop
    (setq tempState (pop stateQueue))
    (setq tempPath (pop pathQueue))

    (setq tempPath (append tempPath (list tempState)))

    (cond
      ((equal tempState goal) (setq pathList (append pathList (list tempPath))))
      (t
        (setq tempPaths (getAllPotentialMoves tempState))
        (loop for i in tempPaths
          do(
            if (not (member i tempPath :test #'equal))
                (setq stateQueue (append stateQueue (list i)))
              )
            )
        (loop for i in tempPaths
          do(
            if (not (member i tempPath :test #'equal))
              (setq pathQueue (append pathQueue (list tempPath)))
            )
          )
        )
      )
    (when (and (not stateQueue) (not pathQueue)) (return pathList))
    )

  pathList
)

(defun start ()
  (setq paths (findPath '(e e e e) '(w w w w)))
  (loop for i in paths
    do(convertPathToString i)
    )
  )

(start)
