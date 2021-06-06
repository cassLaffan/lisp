(load "graph.lisp")

;;Creates a list of all the nodes that begin with the next child to be iterated on.
(defun returnChildNodes(child g)
  (setq childList nil)
  (loop for i in (graph-edgeList g)
    do
      (if (string= (edge-source i) child)
          (setq childList (append childList (list (edge-destination i))))
        )
    )
  childList
)

(defun searchChildNodes(historyList childNodes goal g)
  (if (not childNodes)
    nil
    (let ((path (findPath historyList (car childNodes) goal g)))
      (if (not path)
        (searchChildNodes historyList (cdr childNodes) goal g)
        path
        )
      )
    )
)

(defun findPath(historyList currNode goal g)
  (if (string= currNode goal)
    (append historyList (list currNode))
    (if (member currNode historyList :test #'string=)
      nil
      (let
        ((childNodes (set-difference (returnChildNodes currNode g) historyList :test #'string=)))
        (searchChildNodes (append historyList (list currNode)) childNodes goal g)
        )
      )
    )
)

(defun start(g)
  ;;finds first node, can be anything starting with A
  (setf myStart "A")
  (setf myGoal "Z")
  (setq n 0)

  (setq pathHolder (findPath nil myStart myGoal g))
  (setq P nil)
  (loop
    (when (= n (- (length pathHolder) 1)) (return P))
    (setq P (append P (list (concatenate 'string (nth n pathHolder) "->"))))
    (setq n (+ n 1))
  )
  (append P (list (nth n pathHolder)))
)

(setq myGraph (createGraph))
(setq myPath (start myGraph))
(format t "My graph is: ~D" myGraph)
(terpri)
(format t "The path is: ~D" myPath)
