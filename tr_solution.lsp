(defun tr/power (x k) 
  ;; exercise 3.1 of the book think recursively
  ;; k is a nonenegative integer.
  (cond 
    ((= 0 k) 1)
    ((= 1 k) x)
    (t (* x (tr/power x (- k 1))))))


(defun tr/cannonballs (n) 
  ;; exercise 3.2 of the book think recursively
  ;; k is a positive integer.
  (cond 
    ((= 1 n) 1)
    (t (+ (* n n) (tr/cannonballs (- n 1))))))


(defun tr/square (n) 
  ;; exercise 3.3 of the book think recursively
  (cond 
    ((= 0 n) 0)
    ((= 1 n) 1)
    (t (+ (* 2 n) -1 (tr/square (- n 1))))))


(defun tr/gcd (x y) 
  ;; exercise 3.4 of the book think recursively
  ;; this algorithm is based on Euclid's discovery.
  ;; x and y are 2 nonnegative integers,and x < y.
  (cond 
    ((= 0 x) y)
    ((> x y) (tr/gcd y x))
    (t (tr/gcd x (- y x)))))


(defun pnk (n k) 
  ;; exercise 3.5 of the book think recursively
  ;; n and k are 2 positive integers
  (cond 
    ((= 1 k) n)
    (t (* n (pnk (- n 1) (- k 1))))))


(defun cnk (n k) 
  ;; exercise 3.6 of the book think recursively
  ;; n and k are 2 positive integers
  (cond 
    ((> 0 n) 0)
    ((> 0 k) 0)
    ((= 0 k) 1)
    (t (+ (cnk (- n 1) k) (cnk (- n 1) (- k 1))))))


(defun tr/digit-sum (n) 
  (cond 
    ((> 1 n) 0)
    (t (+ (tr/digit-sum (/ n 10)) (rem n 10)))))


(defun tr/telephone-number->letters (nums / _chars _comb1 _comb2 _comb3 nchr) 
  ;; exercise 6.7 of the book think recursively
  (defun _chars (n / dat) 
    (setq dat '(("2" "a" "b" "c")
                ("3" "d" "e" "f")
                ("4" "g" "h" "i")
                ("5" "j" "k" "l")
                ("6" "m" "n" "o")
                ("7" "p" "r" "s")
                ("8" "t" "u" "v")
                ("9" "w" "x" "y")))
    (cond 
      ((= "" n) '())
      ((= "-" (substr n 1 1)) (_chars (substr n 2)))
      ((= "0" (substr n 1 1)) (_chars (substr n 2)))
      ((= "1" (substr n 1 1)) (_chars (substr n 2)))
      (t (cons (cdr (assoc (substr n 1 1) dat)) (_chars (substr n 2))))))
  (defun _comb1 (ch lst) 
    (cond 
      ((null lst) '())
      (t (cons (strcat ch (car lst)) (_comb1 ch (cdr lst))))))
  (defun _comb2 (char1 char2 temp) 
    (cond 
      ((null char1) (apply 'append (reverse temp)))
      (t (_comb2 (cdr char1) char2 (cons (_comb1 (car char1) char2) temp)))))
  (defun _comb3 (chars) 
    (cond 
      ((null (cadr chars)) (car chars))
      (t (_comb3 (cons (_comb2 (car chars) (cadr chars) '()) (cddr chars))))))
  (setq nchr (_chars nums))
  (cond 
    ((null nchr) '())
    ((null (cadr nchr)) (_comb3 (cons '("") nchr)))
    (t (_comb3 nchr))))


(defun selecting-sort (lst / i) 
  ;; section 7.1 of the book think recursively
  (cond 
    ((null lst) '())
    (t
     (setq i (apply 'min1 (list lst)))
     (cons i (selecting-sort (remfirst i lst))))))


(defun remfirst (ele lst / _remo) 
  (defun _remo (ele lst temp) 
    (cond 
      ((null lst) temp)
      ((equal ele (car lst)) (append (reverse temp) (cdr lst)))
      (t (_remo ele (cdr lst) (cons (car lst) temp)))))
  (_remo ele lst '()))


(defun minfirst (lst) 
  (defun _min (lst2 lst1) 
    (cond 
      ((null lst2) lst1)
      ((< (car lst1) (car lst2))
       (_min (cdr lst2) (append lst1 (list (car lst2)))))
      (t (_min (cdr lst2) (cons (car lst2) lst1)))))
  (_min (cdr lst) (list (car lst))))


(defun min1 (lst) (car (minfirst lst)))


(defun sort2 (lst / i) 
  (cond 
    ((null lst) '())
    (t (setq i (minfirst lst)) (cons (car i) (sort2 (cdr i))))))


(defun fractal-polygon (edgecnt edgelen level ptbegin / pts1 pts2) 
  ;; exercise 9.5 of the book think recursively
  ;; all arguments are positive number
  (cond 
    ((> 3 edgecnt) '())
    ((> 0 edgelen) '())
    ((= 1 level) (draw-polygon (polygon1-points edgecnt edgelen ptbegin 0)))
    (t
     (setq pts1 (polygon1-points edgecnt edgelen ptbegin 0))
     (draw-polygon pts1)
     (setq pts2 (fracpoly edgecnt (* 0.5 edgelen) (- level 1) pts1 '())))))


(defun polygon1-points (n len pnt ang0 / _pts) 
  (defun _trianpts (len pt an0 / pt2 pt3) 
    (setq pt2 (polar pt an0 len)
          pt3 (polar pt2 (+ an0 (/ pi 0.5 3)) len))
    (list pt2 pt3))
  (defun _polypts (n len pt an0 i / pt1) 
    (cond 
      ((> 2 i) '())
      (t
       (setq pt1 (polar pt (+ an0 (* (- n i) (pangle n))) len))
       (cons pt1 (_polypts n len pt1 an0 (- i 1))))))
  (cond 
    ((= 3 n) (cons pnt (_trianpts len pnt ang0)))
    ((< 3 n) (cons pnt (_polypts n len pnt ang0 n)))
    (t nil)))


(defun draw-polygon3 (pts) 
  (command "_.line")
  (foreach e pts (command e))
  (command (car pts) ""))


(defun draw-polygon (pts) 
  (command "_.pline")
  (foreach e pts (command e))
  (command (car pts) ""))


(defun pangle (n) 
  (cond 
    ((> 3 n) nil)
    ((= 3 n) (/ pi 3))
    (t (/ (* 2.0 pi) n))))

(defun make-polygons (n len pts-start / _make) 
  (defun _make (cnt leng pts00 temp / a1 pts1) 
    (cond 
      ((null pts00) '())
      ((not (cadr pts00))
       (setq a1   (pangle cnt)
             pts1 (polygon1-points cnt leng (car pts00) a1))
       (draw-polygon pts1)
       (cons pts1 (_make cnt leng (cdr pts00) a1)))
      (t
       (setq a1   (+ pi (angle (car pts00) (cadr pts00)))
             pts1 (polygon1-points cnt leng (car pts00) a1))
       (draw-polygon pts1)
       (cons pts1 (_make cnt leng (cdr pts00) a1)))))
  (_make n len pts-start nil))


(defun fracpoly (n len lev pts temp) 
  (cond 
    ((= 0 lev) '())
    ((null temp)
     (fracpoly n len lev pts (append (make-polygons n len pts) temp)))
    (temp (fracpoly n (* 0.5 len) (- lev 1) (apply 'append temp) '()))
    (t
     (fracpoly n (* 0.5 len) lev pts (append (make-polygons n len temp) temp)))))