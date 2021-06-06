(defvar vowelList "aeiouAEIOU")

(defun isVowel(l)
  (find l vowelList)
)

(defun isConsosant(l)
  (not (isVowel l))
)

(defun isSequenceOne(w)
  (and (isVowel (char w 0)) (isConsosant (char w 1)) (isVowel (char w 2)))
)

(defun isSequenceTwo(w)
  (and (isVowel (char w 0)) (isConsosant (char w 1)) (isConsosant (char w 2)) (isVowel (char w 3)))
)

(defun createSequenceOne(w)
  (concatenate 'string (subseq w 0 3) "-")
)

(defun createSequenceTwo(w)
  (concatenate 'string (concatenate 'string (subseq w 0 2) "-") (subseq w 2 3))
)

(defun decompose(w)
  (setf a "")
  (if (>= 3 (length w))
    (setf a w)
    (if (isConsosant (char w 0))
      (setf a (concatenate 'string (subseq w 0 1) (decompose (subseq w 1))))
      (if (isSequenceOne w)
        (setf a (concatenate 'string (createSequenceOne w) (decompose (subseq w 3))))
        (if (isSequenceTwo w)
          (setf a (concatenate 'string (createSequenceTwo w) (decompose (subseq w 3))))
          (setf a (concatenate 'string (subseq w 0 1) (decompose (subseq w 1))))
          )
        )
      )
    )
  a
)

(write-line "Please enter a word:")
(setf word (string (read)))
(setf answer (decompose word))
(format t "~D is pronounced as so: ~D" word answer)

(terpri)
