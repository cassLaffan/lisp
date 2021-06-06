(load "animalGraph.lisp")

(defun findPath (histList start goal)
	(if (or (equal start nil) (member start histList :test #'equal))
		nil
		(if (equal start goal)
			(list (append histList (list start)))
			;;Without this flatten function call, it would return nested upon nested lists
			(flattenByOne (list
  			(findPath (append histList (list start)) (moveAlone start) goal)
  			(findPath (append histList (list start)) (moveGoat start) goal)
  			(findPath (append histList (list start)) (moveWolf start) goal)
  			(findPath (append histList (list start)) (moveCabbage start) goal)
				)
			)
    )
  )
)

(defun start ()
  (setq path (findPath nil '(e e e e) '(w w w w)))
	;;ensures the output is readible
	(loop for i in path
		do(
			convertPathToString i
			)
		)
  )

(start)
