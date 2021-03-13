(defvar facts)
(defvar rules)
(set 'facts
      '((animal eats meat)
        (animal is dog)
        (animal has hair)
        (animal is mammal)
       )
 )


(set 'rules 
      '(
        (rule id1
         (if
          (animal has pointed teeth)
          (animal is dog)
          (animal eats meat)
          )
         (then (animal is carnivore))
         )
        (rule id2
         (if
          (animal is lion)
          (animal is tiger)
          )
         (then (animal is carnivore))
         )
       )
      )


;;;; Match function matches a pattern to an assertion
(defun match (pattern  assertion)
  (cond
    ((and
      (null pattern)
      (null assertion))  T ) ; echek empty pattern and empty assertion
    ((or
      (null pattern)
      (null assertion)) nil)
    ((or
      (equal (car pattern) '?)
      (equal (car pattern) (car assertion)))
     (match (cdr pattern) (cdr assertion)))
    ((equal (car pattern) '*)
     (cond
       ((match (cdr pattern) (cdr assertion)))
       ((match pattern (cdr assertion)))
       )
     )
   )
  )

;;;; function loops for chat
(defun loopchat ()
  (format t "welcome to chat")
  (prog (message)
     loop
        (cond
          ((null message)
           (setq message (read-line))
           (terpri)
           (print message)
           (terpri)
           )
          ) 
        (if (match '(bye) (list (read-from-string message)))
            (return '(goodbye))
            (setq message nil)
          )
        (terpri)
        (go loop)
        )
  )



;;;; function adds a new fact to the list of facts 
(defun remember (new)
  (cond
    ((member new facts) nil)
    (T (setq facts (cons new facts))
       )
    )
  )



;;;; function checks if a fact belongs to the list of facts 
(defun recall (fact facts)
  (unless (null facts)
  (if
   (equal fact (first facts))
   (princ fact)
   (recall fact (rest facts))
   )
  )
  )

;;;; function tests if a the premises of a rule  in the list of facts


(defun testrule (rules facts)
  (unless (null rules) 
    (let (conditions)
      (setq conditions (car rules))
      (unless (null conditions)
        (dolist
            (condition (rest (nth 2 conditions)))
          (cond
            ((member condition facts :test #'equal) (princ (rest (nth 3 conditions))) (return t) )
            )
          )
        )
      )
    (testrule (rest rules) facts)
    )
  )


;;;; Test section
;(defvar a)
;(setq a '(animal has dark spot))

;(remember a)


;(defvar b)
;(setq b '(animal is dog))

;(when (member b facts :test #'equal) (princ "OK"))

;(testif (car rules) facts)
;(princ  (rest (nth 3 (car rules))))
;;;;
;(if (testif (car rules) facts)
;    (write-line "OK")
;    (write-line "KO")
;    )

;;;;
;(if (recall b facts)
;    (write-line "OK")
;    (write-line "KO")
;    )

;(dolist (condition   (rest (nth 2 (car rules)))) (when (member condition facts :test #'equal) (princ condition)))

(testrule  rules facts)
