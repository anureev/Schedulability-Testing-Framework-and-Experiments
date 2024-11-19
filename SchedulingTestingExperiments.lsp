; Data structures

;; Tasks

(defstruct (task (:print-function print-task)) I C D)

(defun print-task (p stream depth)
  (format stream "#<~A, ~A, ~A>" (task-I p) (task-C p) (task-D p)))

;; Algoritms

(defstruct (alg (:print-function print-alg)) N F)

(defun print-alg (p stream depth)
  (format stream "#<~A, ~A>" (alg-N p) (alg-F p)))

; Library for generating and checking datasets

;; Uunifast algorithm - Generating task sets

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun uunifast (n ut)
  (prog ((sum-u ut) (u nil) next-sum-u)
     (do ((i 1 (+ i 1)))
	 ((>= i n))
       (setf next-sum-u
	     (* sum-u (expt (random 1.0) (/ 1.0 (- n 1.0)))))
       (setf u (cons (- sum-u next-sum-u) u))
       (setf sum-u next-sum-u))
     (setf u (cons sum-u u))
     (return (reverse u))))

(defun generate-taskset (n ut dgen scale)
  (let ((u (uunifast n ut)))
    (do ((i 1 (+ i 1))
	 Di
	 Ci
	 (ts nil))
	((> i n) (reverse ts))
      (setf Di (funcall dgen))
      (setf Ci (funcall scale (* Di (nth (- i 1) u))))
      (setf Di (funcall scale Di))
      (setf ts (cons (make-task :I i :C Ci :D Di) ts)))))

(defun generate-dataset (n ut dgen scale tss-size)
  (do ((i 1 (+ i 1))
       (ts nil (cons (generate-taskset n ut dgen scale) ts)))
      ((> i tss-size) ts)))

(defun generate-taskset-without-ut (n Dmax)
    (do ((i 1 (+ i 1))
	 Di
	 Ci
	 (ts nil))
	((> i n) (reverse ts))
      (setf Di (random-from-range 2 Dmax))
      (setf Ci (random-from-range 1 Di))
      (setf ts (cons (make-task :I i :C Ci :D Di) ts))))

(defun generate-dataset-without-ut (n Dmax tss-size)
  (do ((i 1 (+ i 1))
       (ts nil (cons (generate-taskset-without-ut n Dmax) ts)))
      ((> i tss-size) ts)))

;; Testing

(defun test-ut-m-n (mgen ngen ugen dgen scale tss-size algs file)
  (with-open-file (str file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str
	    "Util; ProcNum; TaskNum; AlgNum; FeasNum; InfeasNum; UnkNum~%")
    (prog (dset ts-res F-num I-num U-num)
       (dolist (m (funcall mgen))
	 (dolist (ut (funcall ugen))
	   (dolist (n (funcall ngen m))
	     (setf dset (generate-dataset n (* ut m) dgen scale tss-size))
	     (dolist (alg algs)
	       (setf F-num 0)
	       (setf I-num 0)
	       (setf U-num 0)
	       (dolist (ts dset)
		 (setf ts-res (funcall (alg-F alg) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num (+ F-num 1)))
		       ((equal ts-res 'infeasible) (setf I-num (+ I-num 1)))
		       (T (setf U-num (+ U-num 1)))))
	       (format str "~a; ~a; ~a; ~a; ~a; ~a; ~a~%"
		       (substitute #\, #\. (write-to-string ut))
		       m
		       n
		       (alg-N alg)
		       F-num
		       I-num
		       U-num))))))))


(defun test-ut-m-n-dif (mgen ngen ugen dgen scale tss-size algs file)
  (with-open-file (str file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str
	    "Util; ProcNum; TaskNum; FeasNum; InfeasNum; UnkNum~%")
    (prog (dset ts-res F-num1 I-num1 U-num1 F-num2 I-num2 U-num2 alg1 alg2)
       (dolist (m (funcall mgen))
	 (dolist (ut (funcall ugen))
	   (dolist (n (funcall ngen m))
	     (setf dset (generate-dataset n (* ut m) dgen scale tss-size))
	     (setq alg1 (car algs))
	     (setq alg2 (car (cdr algs)))
	       (setf F-num1 0)
	       (setf I-num1 0)
	     (setf U-num1 0)
	     (setf F-num2 0)
	       (setf I-num2 0)
	       (setf U-num2 0)
	       (dolist (ts dset)
		 (setf ts-res (funcall (alg-F alg1) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num1 (+ F-num1 1)))
		       ((equal ts-res 'infeasible) (setf I-num1 (+ I-num1 1)))
		       (T (setf U-num1 (+ U-num1 1))))
		 (setf ts-res (funcall (alg-F alg2) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num2 (+ F-num2 1)))
		       ((equal ts-res 'infeasible) (setf I-num2 (+ I-num2 1)))
		       (T (setf U-num2 (+ U-num2 1)))))
	       (format str "~a; ~a; ~a; ~a; ~a; ~a~%"
		       (substitute #\, #\. (write-to-string ut))
		       m
		       n
		       (- F-num1 F-num2)
		       (- I-num1 I-num2)
		       (- U-num1 U-num2))))))))

(defun test-ut-m-n-rel (mgen ngen ugen dgen scale tss-size algs file)
  (with-open-file (str file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str
	    "Util; ProcNum; TaskNum; FeasNum; InfeasNum; UnkNum~%")
    (prog (dset ts-res F-num1 I-num1 U-num1 F-num2 I-num2 U-num2 alg1 alg2)
       (dolist (m (funcall mgen))
	 (dolist (ut (funcall ugen))
	   (dolist (n (funcall ngen m))
	     (setf dset (generate-dataset n (* ut m) dgen scale tss-size))
	     (setq alg1 (car algs))
	     (setq alg2 (car (cdr algs)))
	       (setf F-num1 0)
	       (setf I-num1 0)
	     (setf U-num1 0)
	     (setf F-num2 0)
	       (setf I-num2 0)
	       (setf U-num2 0)
	       (dolist (ts dset)
		 (setf ts-res (funcall (alg-F alg1) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num1 (+ F-num1 1)))
		       ((equal ts-res 'infeasible) (setf I-num1 (+ I-num1 1)))
		       (T (setf U-num1 (+ U-num1 1))))
		 (setf ts-res (funcall (alg-F alg2) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num2 (+ F-num2 1)))
		       ((equal ts-res 'infeasible) (setf I-num2 (+ I-num2 1)))
		       (T (setf U-num2 (+ U-num2 1)))))
	       (format str "~a; ~a; ~a; ~a; ~a; ~a~%"
		       (substitute #\, #\. (write-to-string ut))
		       m
		       n
		       (float (/ F-num2 F-num1))
		       (- I-num1 I-num2)
		       (- U-num1 U-num2))))))))

(defun test-m-n-without-ut (mgen ngen Dmax tss-size algs file)
  (with-open-file (str file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str
	    "ProcNum; TaskNum; AlgNum; FeasNum; InfeasNum; UnkNum~%")
    (prog (dset ts-res F-num I-num U-num)
       (dolist (m (funcall mgen))
	   (dolist (n (funcall ngen m))
	     (setf dset (generate-dataset-without-ut n Dmax tss-size))
	     (dolist (alg algs)
	       (setf F-num 0)
	       (setf I-num 0)
	       (setf U-num 0)
	       (dolist (ts dset)
		 (setf ts-res (funcall (alg-F alg) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num (+ F-num 1)))
		       ((equal ts-res 'infeasible) (setf I-num (+ I-num 1)))
		       (T (setf U-num (+ U-num 1)))))
	       (format str "~a; ~a; ~a; ~a; ~a; ~a~%"
		       m
		       n
		       (alg-N alg)
		       F-num
		       I-num
		       U-num)))))))

(defun test-m-n-without-ut-dif (mgen ngen Dmax tss-size algs file)
  (with-open-file (str file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str
	    "ProcNum; TaskNum; FeasNum; InfeasNum; UnkNum~%")
    (prog (dset ts-res F-num1 I-num1 U-num1 F-num2 I-num2 U-num2 alg1 alg2)
       (dolist (m (funcall mgen))
	   (dolist (n (funcall ngen m))
	     (setf dset (generate-dataset-without-ut n Dmax tss-size))
	     (setq alg1 (car algs))
	     (setq alg2 (car (cdr algs)))
	     (setf F-num1 0)
	     (setf I-num1 0)
	     (setf U-num1 0)
	     (setf F-num2 0)
	     (setf I-num2 0)
	     (setf U-num2 0)
	       (dolist (ts dset)
		 (setf ts-res (funcall (alg-F alg1) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num1 (+ F-num1 1)))
		       ((equal ts-res 'infeasible) (setf I-num1 (+ I-num1 1)))
		       (T (setf U-num1 (+ U-num1 1))))
		 (setf ts-res (funcall (alg-F alg2) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num2 (+ F-num2 1)))
		       ((equal ts-res 'infeasible) (setf I-num2 (+ I-num2 1)))
		       (T (setf U-num2 (+ U-num2 1)))))
	       (format str "~a; ~a; ~a; ~a; ~a~%"
		       m
		       n
		       (- F-num1 F-num2)
		       (- I-num1 I-num2)
		       (- U-num1 U-num2)))))))


(defun test-m-n-without-ut-rel (mgen ngen Dmax tss-size algs file)
  (with-open-file (str file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str
	    "ProcNum; TaskNum; FeasNum; InfeasNum; UnkNum~%")
    (prog (dset ts-res F-num1 I-num1 U-num1 F-num2 I-num2 U-num2 alg1 alg2)
       (dolist (m (funcall mgen))
	   (dolist (n (funcall ngen m))
	     (setf dset (generate-dataset-without-ut n Dmax tss-size))
	     (setq alg1 (car algs))
	     (setq alg2 (car (cdr algs)))
	     (setf F-num1 0)
	     (setf I-num1 0)
	     (setf U-num1 0)
	     (setf F-num2 0)
	     (setf I-num2 0)
	     (setf U-num2 0)
	       (dolist (ts dset)
		 (setf ts-res (funcall (alg-F alg1) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num1 (+ F-num1 1)))
		       ((equal ts-res 'infeasible) (setf I-num1 (+ I-num1 1)))
		       (T (setf U-num1 (+ U-num1 1))))
		 (setf ts-res (funcall (alg-F alg2) m n ts))
		 (cond ((equal ts-res 'feasible) (setf F-num2 (+ F-num2 1)))
		       ((equal ts-res 'infeasible) (setf I-num2 (+ I-num2 1)))
		       (T (setf U-num2 (+ U-num2 1)))))
	       (format str "~a; ~a; ~a; ~a; ~a~%"
		       m
		       n
		       (- F-num1 F-num2)
		       (float (/ I-num2 I-num1))
		       (- U-num1 U-num2)))))))

;; Values of test parameters

(defun from-num-step (from num step)
  (do ((i from (+ i step)) (j num (- j 1)) (res nil))
      ((equal j 0) (reverse res))
    (setf res (cons i res))))

(defun mgen1 (from num step)
 (lambda () (from-num-step from num step)))

(defun ngen1 (num step)
  (lambda (m) (from-num-step (+ m 1) num step)))

(defun ngen2 ()
  (lambda (m) (list (+ m 1))))

(defun ngen3 (from num step)
 (lambda (m) (from-num-step from num step)))

(defun ugen1 (from num step)
  (lambda () (from-num-step from num step)))

(defun scale1 (shift)
  (lambda (x) (round (* x (expt 10 shift)) 1)))

(defun scale2 ()
  (lambda (x) x))

(defun dgen1 (d-max)
  (lambda () (random-from-range 1 (+ 1 d-max))))

; Algorithms

;; Alg1

(defun Alg1 (m n ts)
  (do ((i 1 (+ i 1)))
      ((> i n))
    (do ((j 1 (+ j 1)))
	((> j n) (return-from Alg1 'infeasible))
      (let ((Cj (task-C (nth (- j 1) ts)))
	    (Di (task-D (nth (- i 1) ts))))
	(if (not (or (equal i j) (<= Di Cj))) (return nil))))
    (do ((j 1 (+ j 1)))
	((>= j i))
      (let ((Ci (task-C (nth (- i 1) ts)))
	    (Cj (task-C (nth (- j 1) ts)))
	    (Di (task-D (nth (- i 1) ts)))
	    (Dj (task-D (nth (- j 1) ts))))
	(if (and (< Cj Di) (< Di (* 2 Cj)) (> Ci (- Dj Cj)))
	    (return-from Alg1 'infeasible)))))
  (return-from Alg1 'feasible))

;; Alg2

(defun Alg2 (m n ts)
  (do ((i 1 (+ i 1)))
      ((> i n))
    (do ((j 1 (+ j 1)))
	((> j n) (return-from Alg2 'infeasible))
      (let ((Cj (task-C (nth (- j 1) ts)))
	    (Di (task-D (nth (- i 1) ts))))
	(if (not (or (equal i j) (<= Di Cj))) (return nil))))
    (do ((j 1 (+ j 1)))
	((>= j i))
      (let ((Ci (task-C (nth (- i 1) ts)))
	    (Cj (task-C (nth (- j 1) ts)))
	    (Di (task-D (nth (- i 1) ts)))
	    (Dj (task-D (nth (- j 1) ts))))
	(if (and (< Cj Di) (< Di (* 2 Cj)) (> Ci (- Dj Cj)))
	    (return-from Alg2 'infeasible)))))
  (return-from Alg2 'unknown))

(defun Alg2-pseudo (m n ts)
  (if (<= n m) 'unknown (Alg2 m n ts)))

;; LeeShin2014 Lemma 6 Page 110 (9)
;; DOI: https://doi.org/10.1109/TPDS.2013.2297098

(defun W (TT C D L)
  (+ (* (floor (- (+ L D) C)) C)
     (min C
	  (- (- (+ L D) C)
	     (* (float (/ (- (+ L D) C) TT)) TT)))))

(defun RHS4 (m n ts k)
  (prog ((sum1 0) (sum2 0) Ti Ci Di Dk Ck tsubset x y)
     (setf Ck (task-C (nth (- k 1) ts)))
     (setf Dk (task-D (nth (- k 1) ts)))
     (setf x (+ (- Dk Ck) 1))
     (do ((i (+ k 1) (+ i 1)))
	 ((> i n))
       (setf Ci (task-C (nth (- i 1) ts)))
       (setf Di (task-D (nth (- i 1) ts)))
       (setf Ti Di)
       (setf sum1 (+ sum1 (min (W Ti Ci Di x) x))))
     (setq y (sort (copy-list (subseq ts k))
		    #'(lambda (t1 t2) (> (task-C t1) (task-C t2)))))
     (setq tsubset (if (> m (length y)) y (subseq y 0 m)))
     (dolist (ti tsubset)
       (setq Ci (task-C ti))
       (setf sum2 (+ sum2 (min (- Ci 1) x))))
     (return-from RHS4 (/ (+ sum1 sum2) m))))

(defun LeeShin2014 (m n ts)
  (prog (Ck Dk)
     (do ((k 1 (+ k 1)))
	 ((> k n))
       (setf Ck (task-C (nth (- k 1) ts)))
       (setf Dk (task-D (nth (- k 1) ts)))
       (if (not (< (RHS4 m n ts k) (+ (- Dk Ck) 1)))
	   (return-from LeeShin2014 'unknown)))
     (return-from LeeShin2014 'feasible)))

;; BaekLee2020 Lemma 8 Page 110 (9)
;; DOI: https://doi.org/10.1109/LES.2020.2966681

(defun nk-val (k ts)
  (- (length ts) k))

(defun RHS6 (m ts k)
  (prog (LO dif)
     (setq dif (- m (nk-val k ts)))
     (setq LO (sort (copy-list (subseq ts 0 k))
		    #'(lambda (t1 t2) (> (task-C t1) (task-C t2)))))
     (return-from RHS6 (if (> dif (length LO)) "error" (- (task-C (nth dif LO)))))))

(defun BaekLee2020 (m n ts)
  (prog (Ck Dk nk)
  (do ((k 1 (+ k 1)))
      ((> k n))
    (setf nk (nk-val k ts))
    (setf Ck (task-C (nth (- k 1) ts)))
    (setf Dk (task-D (nth (- k 1) ts)))
    (if (not (and
	      (or (> nk (- m 1)) (< (min (RHS4 m n ts k) (RHS6 m ts k)) (+ (- Dk Ck) 1)))
	      (or (< nk m) (< (RHS4 m n ts k) (+ (- Dk Ck) 1)))))
	(return-from BaekLee2020 'unknown)))
  (return-from BaekLee2020 'feasible)))

;; ChwaLee2022 Page 57 (3)
;; DOI: https://doi.org/10.1109/LES.2021.3112671

(defun Wi (l ti)
  (let* ((Ci (task-C ti))
	 (Di (task-D ti))
	 (Ti Di)
	 (floor1 (floor (/ l Ti))))
    (+ (* floor1 Ci)
       (max 0
	    (min Ci
		 (- (- l (* floor1 Ti))
		    (- Di Ci)))))))

(defun WiPrime (l ti)
  (let* ((Ci (task-C ti))
	(Di (task-D ti))
	(Ti Di)
	(floor1 (floor (/ l Ti))))
    (+ (* floor1 Ci) (min Ci (- l (* floor1 Ti))))))

(defun Diff (m n ts-p tk alpha)
  (prog (n1 Ck Dk (Diff1 nil))
     (setf Ck (task-C tk))
     (setf Dk (task-D tk))
     (setf n1 (- n (length ts-p)))
     (if (<= m n1) (return-from Diff 0)
	 (prog ()
	     (dolist (ti ts-p)
	       (setf Diff1
		     (cons (- (WiPrime (+ (- Dk Ck) alpha) ti)
			      (Wi (+ (- Dk Ck) alpha) ti))
			   Diff1)))
	    (return-from Diff
	      (reduce '+
		      (subseq (sort Diff1 #'<) 0 (- m n1))))))))

(defun Eq3 (m n ts-p tk alpha)
  (prog (Ck Dk sum1)
     (setf Ck (task-C tk))
     (setf Dk (task-D tk))
     (setf sum1 0)
     (dolist (ti ts-p)
       (setf sum1 (+ sum1 (Wi (+ (- Dk Ck) alpha) ti))))
     (return-from Eq3
       (< (* m (+ (- Dk Ck) alpha))
	  (+ (+ alpha (Diff m n ts-p tk alpha)) sum1)))))

(defun delete-nth (i l)
  (delete-if (constantly t) l :start (- i 1) :count 1))

(defun not-Eq3-for-each-alpha (m n ts-p tk Ck)
  (do ((alpha 1 (+ alpha 1)))
      ((> alpha Ck) (return-from not-Eq3-for-each-alpha T))
    (if (Eq3 m n ts-p tk alpha)
	(return-from not-Eq3-for-each-alpha nil))))

(defun ChwaLee2022 (m n ts)
  (prog ((ts-nop-passed nil) (ts-nop-rest ts) (ts-p nil) n1 tk Ck)
     (do ((p n (- p 1)))
	 ((< p 1) (return-from ChwaLee2022 'unknown))
       (setq n1 (length ts-nop-rest))
       (tagbody
	 (do ((k 1 (+ k 1)))
	     ((> k n1))
	   (setq tk (car ts-nop-rest))
	   (setf ts-nop-passed (cons tk ts-nop-passed))
	   (setq ts-nop-rest (cdr ts-nop-rest))
	  (setf Ck (task-C tk))
	  (if (not-Eq3-for-each-alpha m n ts-p tk Ck)
	      (prog ()
		 (setf ts-p (cons tk ts-p))
		 (setq ts-nop-rest (append (cdr ts-nop-passed) ts-nop-rest))
		 (setq ts-nop-passed nil)
		 (go below))))
	below)
       (if (not (equal (length ts-p) (+ (- n p) 1)))
	   (return-from ChwaLee2022 'infeasible)))))

(defun ChwaLee2022-pseudo (m n ts)
  (if (<= n m) 'unknown (ChwaLee2022 m n ts)))

;; Tests

;;; Actual

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 8)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "LeeShin2014" :F #'LeeShin2014))
 "m-ut-n-nm1-1000-Alg1-LeeShin2014.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "LeeShin2014" :F #'LeeShin2014))
 "m-ut-n-nm1-100-Alg1-LeeShin2014.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 8)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "BaekLee2020" :F #'BaekLee2020))
 "m-ut-n-nm1-1000-Alg1-BaekLee2020.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "BaekLee2020" :F #'BaekLee2020))
 "m-ut-n-nm1-1000-Alg1-BaekLee2020.csv")

(test-m-n-without-ut
 (mgen1 2 20 1)
 (ngen1 10 1)
 100
 100
 (list (make-alg :N "Alg2" :F #'Alg2) (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-n-100-100-Alg2-ChwaLee2022.csv")

(test-m-n-without-ut
 (mgen1 2 20 1)
 (ngen3 2 30 1)
 100
 100
 (list (make-alg :N "Alg2" :F #'Alg2-pseudo) (make-alg :N "ChwaLee2022" :F #'ChwaLee2022-pseudo))
 "m-n-100-100-Alg2-ChwaLee2022-pseudo.csv")

;;; Other 

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1))
 "m-ut-n-nm1-1000-Alg1-comma.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen1 20 1)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "Alg2" :F #'Alg2))
 "m-ut-n-1000-Alg2-comma.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "LeeShin2014" :F #'LeeShin2014))
 "m-ut-n-nm1-1000-LeeShin2014-comma.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "BaekLee2020" :F #'BaekLee2020))
 "m-ut-n-nm1-1000-BaekLee2020-comma.csv")

(test-ut-m-n-dif
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "LeeShin2014" :F #'LeeShin2014))
 "m-ut-n-nm1-100-Alg1-LeeShin2014-comma.1.csv")

(test-ut-m-n-rel
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "LeeShin2014" :F #'LeeShin2014))
 "m-ut-n-nm1-1000-Alg1-LeeShin2014-rel-comma.1.csv")

(test-ut-m-n-dif
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 1000
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "BaekLee2020" :F #'BaekLee2020))
 "m-ut-n-nm1-1000-Alg1-BaekLee2020-comma.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 3)
 30
 (list (make-alg :N "Alg1" :F #'Alg1) (make-alg :N "BaekLee2020" :F #'BaekLee2020))
 "m-ut-n-nm1-30-Alg1-BaekLee2020-comma.1.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 3)
 100
 (list (make-alg :N "BaekLee2020" :F #'BaekLee2020))
 "m-ut-n-nm1-100-BaekLee2020-comma.1.csv")

(test-ut-m-n
 (mgen1 2 5 1)
 (ngen1 3 1)
 (ugen1 1/10 9 1/10)
 (dgen1 100)
 (scale1 3)
 30
 (list (make-alg :N "Alg2" :F #'Alg2) (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-ut-n-nm1-30-Alg1-ChwaLee2022-comma.csv")

(test-m-n-without-ut
 (mgen1 2 20 1)
 (ngen1 10 1)
 100
 100
 (list (make-alg :N "Alg2" :F #'Alg2) (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-ut-n-nm1-30-Alg1-ChwaLee2022.csv")

(test-m-n-without-ut-rel
 (mgen1 2 20 1)
 (ngen1 10 1)
 100
 100
 (list (make-alg :N "Alg2" :F #'Alg2) (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-ut-n-nm1-30-Alg1-ChwaLee2022-rel.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 2)
 10
 (list (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-ut-n-nm1-100-ChwaLee2022-comma.1.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale2)
 100
 (list (make-alg :N "Alg2" :F #'Alg2))
 "m-ut-n-nm1-100-Alg1-comma.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen1 20 1)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 1)
 10
 (list (make-alg :N "Alg2" :F #'Alg2) (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-ut-n-100-Alg1-ChwaLee2022-comma.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 1/10 9 1/10)
 (dgen1 100)
 (scale1 1)
 30
 (list (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-ut-n-nm1-100-ChwaLee2022-comma.1.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen1 20 1)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 1)
 50
 (list (make-alg :N "ChwaLee2022" :F #'ChwaLee2022))
 "m-ut-n-30-ChwaLee2022-comma.1.csv")

(test-ut-m-n
 (mgen1 2 20 1)
 (ngen2)
 (ugen1 0.1 9 0.1)
 (dgen1 100)
 (scale1 2)
 30
 (list (make-alg :N "ChwaLee2022" :F #'ChwaLee2022) (make-alg :N "Alg2" :F #'Alg2))
 "m-ut-n-nm1-100-ChwaLee2022-Alg2-comma.csv")

