;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

P02 Find the last but one box of a list.
    Example:
    * (my-but-last '(a b c d))
    (C D)


(defun my-but-last (list)
  (cond
    ((or (null list) (null (cdr list))) nil)
    ((null (cdr (cdr list))) list)
    (t (my-but-last (cdr list)))))


(time (my-but-last '(a b c d m n c)))

(defun my-but-last (list)
  (let ((m (length list)))
    (cond 
      ((eq 0 m) nil)
      ((eq 1 m) list)
      (t (subseq list (- m 2) m)))))
(my-but-last '(m b c))

P05 (*) Reverse a list.

(defun my-reverse (list)
(labels 
    ((reverse-aux (list list-aux)
		 (cond
		   ((null list) list-aux)
		   (t (reverse-aux (cdr list) (cons (car list) list-aux))))))
  (reverse-aux list nil)))
(my-reverse '(b c d m a c))


P06 (*) Find out whether a list is a palindrome.
    A palindrome can be read forward or backward; e.g. (x a m a x).


(defun is_palindrome (list)
  (let ((result t))
    (do ((n 0 (+ n 1))
	 (m (length list) (- m 1)))
	((or (> n m) (not result)) result)
    (when (not (eql (elt list n) (elt list (- m 1))))
      (setf result nil)))))
(is_palindrome '(x a m a x))

P07 (**) Flatten a nested list structure.
    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

    Example:
    * (my-flatten '(a (b (c d) e)))
    (A B C D E)

    Hint: Use the predefined functions list and append.



(defun my-flatten (list)
(cond 
  ((null list) nil)
  ((listp (car list)) (append (my-flatten (car list)) (my-flatten (cdr list))))
  (t (cons (car list) (my-flatten (cdr list))))))

(my-flatten '(a (b (c d) e)))


     
P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

    Example:
    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)



(defun compress (list)
(labels 
    ((compress-aux (list last)
       (cond 
	 ((null list) nil)
	 ((eq (car list) last) (compress-aux (cdr list) last))
	 (t (cons (car list) (compress-aux (cdr list) (car list)))))))
  (cond 
    ((null list) nil)
    (t (cons (car list) (compress-aux (cdr list) (car list)))))))

     
P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.

    Example:
    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))



(defun pack (list)
; use pack-aux with a list of two list elements, first the sublist to be created 
; the second is the rest of the original list
; add elements to sublist until the elements differ
  (labels
      ((pack-aux (list)
	 (cond 
	   ((null (car (cdr list))) list)
	   (t (let 
		  ((sublist (car list))
		   (restlist (car (cdr list))))
		(cond 
		  ((eql (car sublist) (car restlist))
		   (pack-aux (list (cons (car sublist) sublist) (cdr restlist))))
		  (t list)))))))
    (cond 
      ((null list) nil)
      (t (let 
	     ((list-aux (pack-aux (list (cons (car list) ()) (cdr list)))))
	   (cons (car list-aux) (pack (car (cdr list-aux)))))))))
