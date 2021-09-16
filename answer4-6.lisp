(defun neg_first (lst) 
  (defun _nf (lst nlst nnlst) 
    (cond 
      ((null lst) (append nlst nnlst))
      (t
       (if (> 0 (car lst)) 
         (_nf (cdr lst) (append nlst (list (car lst))) nnlst)
         (_nf (cdr lst) nlst (append nnlst (list (car lst))))))))
  (_nf lst '() '()))

(defun transpose_t (lst) 
  (defun _car (lst) 
    (cond 
      ((null lst) '())
      (t (cons (car (car lst)) (_car (cdr lst))))))
  (defun _cdr (lst) 
    (cond 
      ((null lst) '())
      (t (cons (cdr (car lst)) (_cdr (cdr lst))))))
  (cond 
    ((null (car lst)) '())
    (t (cons (_car lst) (transpose_t (_cdr lst))))))

(defun cartesian_prod (lst1 lst2) 
  (defun _cp (a lst) 
    (cond 
      ((null lst) '())
      (t (cons (list a (car lst)) (_cp a (cdr lst))))))
  (cond 
    ((null lst1) '())
    (t (append (_cp (car lst1) lst2) (cartesian_prod (cdr lst1) lst2)))))
