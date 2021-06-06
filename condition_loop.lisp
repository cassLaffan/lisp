(setq i 0)

(loop while (= i 0)
  do
    (if (not nil)
      (progn
        (write-line "Flag")
        (setq i (+ i 1))
        )
      (progn
        (write-line "Second flag")
        )
      )
)
