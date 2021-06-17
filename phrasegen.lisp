
(defparameter Articles '(the  a))

(defparameter Verbs '(eat  take sing drive see))

(defparameter Nouns '(ball man woman table chair))


(defun sentence (articles nouns verbs)
  (append (noun-phrase articles nouns)    (verb-phrase verbs (noun-phrase articles nouns)))
  )


(defun noun-phrase (Article Noun)
  (append  (random-elt Article) (random-elt Noun))
  )

(defun verb-phrase (Verb noun-phrase)
  (append (random-elt Verb) noun-phrase)
  )

;;;; This function selects a random element among the choice
(defun random-elt (choice)
  (list (elt choice (random (length choice))))
  )

#|
sentence => noun-phrase + verb-phrase
noun-phrase => article + noun
verb-phrase => verb + noun-phrase
|#
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase) )
    (noun-phrase -> (article noun))
    (verb-phrase -> (verb noun-phrase))
    (Article -> the a)
    (Noun -> Man Woman Ball)
    (Verb -> eat sing dance go)
    )
  )

(defvar *grammar* *simple-grammar*
  "The grammar used by generate."
  )

(defun rule-rhs (rule)
  "Right hand side"
  (first rule)
  )

(defun rule-lhs (rule)
  "Left hand side"
  (rest (rest rule))
  )

(defun rewrite (category)
  "rewrite"
  (rule-rhs (assoc category *grammar*))
  )

(defun mappend (fn a-list)
  ""
  (apply #'append (mapcar fn a-list))
  )


(defun generate (phrase)
  "Generate a phrase"
  (cond 
    ((listp phrase) (mappend #'generate phrase ))
    ((rewrite phrase) (generate (random-elt (rewrite phrase))))
    (t (list phrase))
   )
  )
