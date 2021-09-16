(defun count_between  (lst)
  (defun _betw  (lst2)
    (if (or (and (< (car lst2) (cadr lst2)) (> (caddr lst2) (cadr lst2)))
            (and (> (car lst2) (cadr lst2)) (< (caddr lst2) (cadr lst2))))
      1
      0))
  (cond ((null lst) 0)
        ((> 3 (length lst)) 0)
        (t (+ (_betw lst) (count_between (cdr lst))))))

(defun between (lst) 
  (defun _betw2 (lst2) 
    (if 
      (or (and (< (car lst2) (cadr lst2)) (> (caddr lst2) (cadr lst2))) 
          (and (> (car lst2) (cadr lst2)) (< (caddr lst2) (cadr lst2))))
      (list (cadr lst2))
      '()))
  (cond 
    ((null lst) '())
    ((> 3 (length lst)) '())
    (t (append (_betw2 lst) (between (cdr lst))))))
