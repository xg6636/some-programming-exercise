(defun neg_first (lst) 
  (defun _nf (lst nlst nnlst) 
    (cond 
      ((null lst) (append nlst nnlst))
      (t
       (if (> 0 (car lst)) 
         (_nf (cdr lst) (append nlst (list (car lst))) nnlst)
         (_nf (cdr lst) nlst (append nnlst (list (car lst))))))))
  (_nf lst '() '()))
