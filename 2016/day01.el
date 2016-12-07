(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(split-string (get-string-from-file "day01.input.txt") ", ")

(defun make-state (position facing-direction &optional trail)
  (list position facing-direction trail))

(defun get-position (state)
  (first state))

(defun get-facing-direction (state)
  (second state))

(defun get-trail (state)
  (third state))

(defun inc-direction (direction)
  (cond ((equal direction "N") "E")
        ((equal direction "E") "S")
        ((equal direction "S") "W")
        ((equal direction "W") "N")))

(defun dec-direction (direction)
  (cond ((equal direction "N") "W")
        ((equal direction "E") "N")
        ((equal direction "S") "E")
        ((equal direction "W") "S")))

(defun turn (state direction)
  (make-state (get-position state)
              (cond ((equal direction "L") (dec-direction (get-facing-direction state)))
                    ((equal direction "R") (inc-direction (get-facing-direction state))))
	      (get-trail state)))

(defun move (state distance)
  (let* ((direction (get-facing-direction state))
         (position (cond ((equal direction "N") (mapcar* '+ (list 0 (- distance)) (get-position state)))
                         ((equal direction "E") (mapcar* '+ (list distance 0) (get-position state)))
                         ((equal direction "S") (mapcar* '+ (list 0 distance) (get-position state)))
                         ((equal direction "W") (mapcar* '+ (list (- distance) 0) (get-position state)))))
	 (trail (cons (get-position state) (get-trail state))))
    (make-state position direction trail)))


(defun step (state step)
  (let* ((splitted (mapcar 'char-to-string step))
         (direction (first splitted))
         (distance (string-to-number (apply 'concat (rest splitted)))))
    (move (turn state direction) distance)
    ))

(defun all-steps (state steps)
  (reduce (lambda (state next) (step state next)) steps :initial-value state))

(defun manhattan-distance (pos1 pos2)
  (reduce '+ (mapcar 'abs (mapcar* '- pos2 pos1))))

(defun find-first-dup (l)
  (let ((value l)
	(result nil))
    (dolist (pos l value)
      (when (and (member pos (cdr value))
		 (not result))
	(setq result pos))
      (setq value (cdr value)))
    result))
(find-first-dup '(1 2 3 1 3 4 5))

(defun day01 (s)
  (let* ((input-list (split-string s ", "))
         (final-state (all-steps (make-state '(0 0) "N") input-list))
         (final-position (get-position final-state))
         (distance (manhattan-distance '(0 0) final-position))
         )
    (list distance final-state)
    ))

(defun day01-part2 (s)
  (let* ((input-list (split-string s ", "))
         (final-state (all-steps (make-state '(0 0) "N") input-list))
	 (final-position (find-first-dup (reverse (get-trail final-state))))
         (distance (manhattan-distance '(0 0) final-position))
         )
    (list distance final-state)
    ))

(all-steps (make-state '(0 0) "N") '("R5" "R5" "R5"))
(all-steps (make-state '(0 0) "N") (split-string "R5, L5, R5, R3" ", "))
(manhattan-distance '(0 0) '(10 -2))

(manhattan-distance '(0 0) (get-position (all-steps (make-state '(0 0) "N") (split-string (get-string-from-file "day01.input.txt") ", "))))
(all-steps (make-state '(0 0) "N") (split-string (get-string-from-file "day01.input.txt") ", "))

(day01 (get-string-from-file "day01.input.txt"))
