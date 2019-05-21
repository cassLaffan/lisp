;;====START GRAPH DEFINITION====
(defstruct edge
  source
  destination
  weight
)

(defstruct graph
  edgeList
)
;;Logic: the entry can't have more than the maximum amount of nodes

(defun createEdges()
  (setq nodeOne (make-edge :source "A" :destination "B" :weight 2))
  (setq nodeTwo (make-edge :source "B" :destination "C" :weight 3))
  (setq nodeThree (make-edge :source "C" :destination "H" :weight 5))
  (setq nodeFour (make-edge :source "D" :destination "I" :weight 4))
  (setq nodeFive (make-edge :source "E" :destination "B" :weight 9))
  (setq nodeSix (make-edge :source "H" :destination "E" :weight 1))
  (setq nodeSeven (make-edge :source "H" :destination "D" :weight 5))
  (setq nodeEight (make-edge :source "J" :destination "H" :weight 8))
  (setq nodeNine (make-edge :source "A" :destination "E" :weight 2))
  (setq nodeTen (make-edge :source "G" :destination "F" :weight 3))
  (setq nodeEleven (make-edge :source "H" :destination "J" :weight 2))
  (setq nodeTwelve (make-edge :source "J" :destination "K" :weight 1))
  (setq nodeThirteen (make-edge :source "K" :destination "I" :weight 4))
  (setq nodeFourteen (make-edge :source "K" :destination "X" :weight 5))
  (setq nodeFifteen (make-edge :source "X" :destination "R" :weight 7))
  (setq nodeSixteen (make-edge :source "X" :destination "Y" :weight 2))
  (setq nodeSeventeen (make-edge :source "R" :destination "Z" :weight 1))
  (setq nodeEighteen (make-edge :source "J" :destination "M" :weight 4))

  (setq a (list nodeOne nodeTwo nodeThree nodeFour nodeFive nodeSix nodeSeven nodeEight nodeNine nodeTen
              nodeEleven nodeTwelve nodeThirteen nodeFourteen nodeFifteen nodeSixteen nodeSeventeen nodeEighteen))
  a
)

(defun createGraph()

  (setq currentEdges (createEdges))

  (setq newGraph (make-graph :edgeList currentEdges))
  newGraph
)
;;=====END GRAPH DEFINION======
