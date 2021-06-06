(load "graph.lisp")
;;Allows for lists of paths and their weights
(defstruct path
  currPath
  totalWeight
)
;;returns a list of child edges (as in, their node and any node attached to them)
(defun returnChildNodes(child pathObj g)
  (setq childList nil)
  (loop for i in (graph-edgeList g)
    do
      (if
        (and
          (string= (edge-source i) child)
          (not (member (edge-destination i) (path-currPath pathObj) :test #'string=))
          )
        (setq childList (append childList (list i)))
        )
    )
  childList
)
;;Recursively compares the list of path objects at hand with the current smallest path
;;Returns the smallest path in a given list in the end
(defun getSmallestPath(pathObjs smallestPath)
  (if (not pathObjs)
    smallestPath
    (if (not (car pathObjs))
      (getSmallestPath (cdr pathObjs) smallestPath)
      (if (or (not smallestPath) (< (path-totalWeight (car pathObjs)) (path-totalWeight smallestPath)))
        (getSmallestPath (cdr pathObjs) (car pathObjs))
        (getSmallestPath (cdr pathObjs) smallestPath)
        )
      )
    )
)
;;Searches all childnodes while also ensuring the smallest of the searched gets returned
;;Returns a path object
(defun searchChildNodes(pathObj childEdges goal g)
  (getSmallestPath
    (map 'list
      (lambda (childEdge)
        (findPath
          (make-path :currPath (path-currPath pathObj) :totalWeight (+ (path-totalWeight pathObj) (edge-weight childEdge)))
          (edge-destination childEdge) goal g)
        )
      childEdges
      )
    nil
    )
)
;;Recursively searches by depth-first search
;;Will return the shortest path because of the previous two functions
(defun findPath(pathObj currNode goal g)
  (if (string= currNode goal)
    (make-path :currPath (append (path-currPath pathObj) (list currNode)) :totalWeight (path-totalWeight pathObj))
    (if (member currNode (path-currPath pathObj) :test #'string=)
      nil
      (let
        ((childEdges (returnChildNodes currNode pathObj g)))
        (searchChildNodes
          (make-path :currPath (append (path-currPath pathObj) (list currNode)) :totalWeight (path-totalWeight pathObj))
          childEdges goal g)
        )
      )
    )
)

(defun convertPathToString(pathList)
  (setq P nil)
  (setq n 0)
  (loop
    (when (= n (- (length (path-currPath pathList)) 1)) (return P))
    (setq P (append P (list (concatenate 'string (nth n (path-currPath pathList))) "->")))
    (setq n (+ n 1))
  )
  (append P (list (nth n (path-currPath pathList))))
)

(defun start(g)
  (setf myStart "A")
  (setf myGoal "Z")

  (setq pathHolder (findPath (make-path :currPath nil :totalWeight 0) myStart myGoal g))

  pathHolder
)

(setq myGraph (createGraph))
(format t "The graph is: ~D" myGraph)
(terpri)
(setq myPath (start myGraph))
(format t "The path is: ~D" (convertPathToString myPath))
(terpri)
(format t "The cost is: ~D" (path-totalWeight myPath))
