; Data structures

;; Tasks

(defstruct (task) C D T)

;; Tasksets

(defstruct (taskset) M N U UC tasks)

#|
M - number of processors
N - number of tasks
U - utilization
UC - utilization class
tasks - list of tasks
|#

; Algorithms

;; Alg1

(defun implies (x y)
  (cond ((equal x nil) T)
	(T y)))

(defun Alg1 (ts)
  (do ((n (taskset-N ts))
       (tasks (taskset-tasks ts))
       (i 1 (+ i 1)))
      ((> i n))
    (do ((j 1 (+ j 1)))
	((> j n) (return-from Alg1 'infeasible))
      (let ((Cj (task-C (nth (- j 1) tasks)))
	    (Di (task-D (nth (- i 1) tasks))))
	(if (not (implies (not (equal i j)) (< Di Cj))) (return))))
    (do ((j 1 (+ j 1)))
	((>= j i))
      (let ((Ci (task-C (nth (- i 1) tasks)))
	    (Cj (task-C (nth (- j 1) tasks)))
	    (Di (task-D (nth (- i 1) tasks)))
	    (Dj (task-D (nth (- j 1) tasks))))
	(if (and (<= Cj Di) (< Di (* 2 Cj)) (> Ci (- Dj Cj)))
	    (return-from Alg1 'infeasible)))))
  (return-from Alg1 'schedulable))

;; Alg2

(defun Alg2 (ts)
  (do ((n (taskset-N ts))
       (tasks (taskset-tasks ts))
       (i 1 (+ i 1)))
      ((> i n))
    (do ((j 1 (+ j 1)))
	((> j n) (return-from Alg2 'infeasible))
      (let ((Cj (task-C (nth (- j 1) tasks)))
	    (Di (task-D (nth (- i 1) tasks))))
	(if (not (implies (not (equal i j)) (< Di Cj))) (return))))
    (do ((j 1 (+ j 1)))
	((>= j i))
      (let ((Ci (task-C (nth (- i 1) tasks)))
	    (Cj (task-C (nth (- j 1) tasks)))
	    (Di (task-D (nth (- i 1) tasks)))
	    (Dj (task-D (nth (- j 1) tasks))))
	(if (and (<= Cj Di) (< Di (* 2 Cj)) (> Ci (- Dj Cj)))
	    (return-from Alg2 'infeasible)))))
  (return-from Alg2 'unknown))

;; LeeShin2014

#|
Lemma 6 Page 110 (9)
DOI: https://doi.org/10.1109/TPDS.2013.2297098
|#

(defun W (TT C D L)
  (+ (* (floor (/ (- (+ L D) C) TT)) C)
     (min C
	  (- (- (+ L D) C)
	     (* (floor (/ (- (+ L D) C) TT)) TT)))))

(defun RHS4 (m ts k)
  (prog ((sum1 0) (sum2 0) Ti Ci Di Dk Ck tsubset x y)
     (setf Ck (task-C (nth (- k 1) ts)))
     (setf Dk (task-D (nth (- k 1) ts)))
     (setf x (+ (- Dk Ck) 1))
     (do ((i (- k 1) (- i 1)))
	 ((< i 1))
       (setf Ci (task-C (nth (- i 1) ts)))
       (setf Di (task-D (nth (- i 1) ts)))
       (setf Ti Di)
       (setf sum1 (+ sum1 (min (W Ti Ci Di x) x))))
     (setq y (sort (subseq ts k)
		    #'(lambda (t1 t2) (> (task-C t1) (task-C t2)))))
     (setq tsubset (if (> m (length y)) y (subseq y 0 m)))
     (dolist (ti tsubset)
       (setq Ci (task-C ti))
       (setf sum2 (+ sum2 (min (- Ci 1) x))))
     (return-from RHS4 (/ (+ sum1 sum2) m))))

(defun LeeShin2014 (ts)
  (prog ((m (taskset-M ts))
	 (n (taskset-N ts))
	 (tasks (taskset-tasks ts))
	 Ck Dk)
     (do ((k 1 (+ k 1)))
	 ((> k n))
       (setf Ck (task-C (nth (- k 1) tasks)))
       (setf Dk (task-D (nth (- k 1) tasks)))
       (if (not (< (RHS4 m tasks k) (+ (- Dk Ck) 1)))
	   (return-from LeeShin2014 'unknown)))
     (return-from LeeShin2014 'schedulable)))

;; BaekLee2020

#|
Lemma 8 Page 110 (9)
DOI: https://doi.org/10.1109/LES.2020.2966681
|#

(defun nk-val (k ts)
  (- k 1))

(defun RHS6 (m ts k)
  (prog (LO th)
     (setq th (- m (nk-val k ts)))
     (setq LO (sort (subseq ts k)
		    #'(lambda (t1 t2) (> (task-C t1) (task-C t2)))))
     (return-from RHS6 (if (> th (length LO)) "error"
			   (- (task-C (nth (- th 1) LO)) 1)))))

(defun BaekLee2020 (ts)
  (prog ((m (taskset-M ts))
	 (n (taskset-N ts))
	 (tasks (taskset-tasks ts))
	 Ck Dk nk)
  (do ((k 1 (+ k 1)))
      ((> k n))
    (setf nk (nk-val k tasks))
    (setf Ck (task-C (nth (- k 1) tasks)))
    (setf Dk (task-D (nth (- k 1) tasks)))
    (if (not
	 (cond ((<= nk (- m 1))
		(< (min (RHS4 m tasks k)
			(RHS6 m tasks k))
		   (+ (- Dk Ck) 1)))
	       ((<= m nk)
		(< (RHS4 m tasks k)
		   (+ (- Dk Ck) 1))) 
	       (T T)))
	(return-from BaekLee2020 'unknown)))
  (return-from BaekLee2020 'schedulable)))

; Library for generating and checking datasets

;; Auxiliary functions

;;; Hash table functions

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

;;; Random functions

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

;;; Uunifast algorithm

#|
Distributing total utilization into tasks
|#

(defun uunifast (n ut)
  (prog ((sum-u ut) (u nil) next-sum-u)
     (do ((i 1 (+ i 1)))
	 ((>= i n))
       (setf next-sum-u
	     (* sum-u
		(expt (random 1.0) (/ 1.0 (- n i)))))
       (setf u (cons (- sum-u next-sum-u) u))
       (setf sum-u next-sum-u))
     (setf u (cons sum-u u))
     (return u)))

;; Serializing datasets

#|
to Lisp files
|#

(defun print-dataset (dataset path name)
  (with-open-file
      (str
       (format nil "~A~A.lsp" path name)
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create)
    (format str "(list ")
    (dolist (taskset dataset)
      (format
       str
       "(make-taskset :M ~D :N ~D :U ~5,3F :UC ~5,3F :tasks (list "
       (taskset-M taskset)
       (taskset-N taskset)
       (taskset-U taskset)
       (taskset-UC taskset))
      (dolist (task (taskset-tasks taskset))
	(format
	 str
	 "(make-task :C ~D :D ~D :T ~D) "
	 (task-C task)
	 (task-D task)
	 (task-T task)))
      (format str ") ) "))
    (format str " )")))

;; Applying algorithms

(defun struct-ratio (str1 str2 list)
    (cond
      ((null list) nil)
      (T
       (let ((x (car list)))
	 (cons
	  (list x (gethash x str1) (gethash x str2) (cond
	    ((equal (float (gethash x str2)) 0.0) 0.0)
	    (T (float (/ (gethash x str1) (gethash x str2))))))
	(struct-ratio str1 str2 (cdr list)))))))

(defun apply-alg-to-dataset-uc (alg dataset uc-list prop)
  (prog ((res-num 0)
	 (res-map (make-hash-table))
	 (all-map (make-hash-table))
	 (t-num (get-internal-run-time))
	 UC)
     (dolist (UC uc-list)
       (setf (gethash UC res-map) 0)
       (setf (gethash UC all-map) 0))
     (dolist (taskset dataset)
       (setf UC (taskset-UC taskset))
       (if
	(equal (funcall alg taskset) prop)
	(prog () (setf (gethash UC res-map) (+ (gethash UC res-map) 1))
	   (setf res-num (+ res-num 1))))
       (setf (gethash UC all-map) (+ (gethash UC all-map) 1)))
     (return (list
	      (- (get-internal-run-time) t-num)
	      res-num
	      (struct-ratio res-map all-map uc-list)))))

(defun apply-alg-to-dataset (alg dataset prop)
  (prog ((res-num 0)
	 (t-num (get-internal-run-time)))
     (dolist (taskset dataset)
       (if
	(equal (funcall alg taskset) prop)
	(setf res-num (+ res-num 1))))
     (return (list
	      (- (get-internal-run-time) t-num)
	      res-num))))

(defun alg-diff (alg1 alg2 dataset prop)
  (prog ((diff nil))
     (dolist (taskset dataset)
       (if (and
	    (equal (funcall alg1 taskset) prop)
	    (not (equal (funcall alg2 taskset) prop)))
	   (setf diff (cons taskset diff))))
     (return diff)))

(defun alg-diff2 (alg1 alg2 dataset prop1 prop2)
  (prog ((diff nil))
     (dolist (taskset dataset)
       (if (and
	    (equal (funcall alg1 taskset) prop1)
	    (equal (funcall alg2 taskset) prop2))
	   (setf diff (cons taskset diff))))
     (return diff)))

;; Serializing algorithm outputs

#|
to csv files
|#

(defun print-algorithm-output-to-csv-uc-total (alg-output path alg-name parameter)
  (with-open-file
      (str
       (format nil "~A~A-output-uc-total.csv" path alg-name)
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create)
    (format str "Util. Class; ~A~%" parameter)
    (dolist (elem (nth 2 alg-output))
      (format str "~3,1F; ~,2F~%" (nth 0 elem) (nth 3 elem)))))

;; Dataset1

#|
1) T = D
2) monotonic
3) N = M + 1
|#

(defun strict-D (tasks)
  (prog ((D -1))
     (dolist (task tasks)
       (cond
	 ((< D (task-D task)) (setf D (task-D task)))
	 (T (setf (task-D task) (+ D 1)) (setf (task-T task) (+ D 1)) (setf D (+ D 1))))))
  tasks)
 
(defun generate-dataset1
    (u-list uc-list m-list C-max dataset-power)
  (prog ((dataset nil))
     (dolist (ut u-list)
       (dolist (m m-list)
	 (do ((k 1 (+ k 1)))
	     ((> k dataset-power))
	   (prog ((U 0) UC (tasks nil) (n (+ m 1)))
	      (do ((i 1 (+ i 1))
		   (ul-list (uunifast n ut))
		   Di Ci ULi)
		  ((> i n))
		(setf ULi (nth (- i 1) ul-list))
		(if (equal ULi 0.0) (return-from generate-dataset1 (list n ut ul-list)))
		(setf Ci (random-from-range 1 C-max))
		(setf Di (max (ceiling (/ Ci (* ULi m))) Ci))
		(if (equal Ci Di) (setf Di (+ Di 1)))
		(setf tasks (cons (make-task :C Ci :D Di :T Di) tasks)))
	      (setf tasks (strict-D (sort tasks
			    #'(lambda (t1 t2)
				(< (task-D t1) (task-D t2))))))
	      (dolist (task tasks)
		(setf U (+ U (/ (task-C task) (task-D task)))))
	      (setf U (/ U m))
	      (setf UC
		    (dolist (UC-elem uc-list)
		      (if (< U UC-elem) (return UC-elem))))
	      (setf dataset
		    (cons
		     (make-taskset
		      :M m
		      :N n
		      :U (float U)
		      :UC UC
		      :tasks tasks
		      )
		     dataset))))))
  (return dataset)))

;; Dataset2

(defun generate-dataset2
    (u-list uc-list m-list n-m-ratio C-max dataset-power)
  (prog ((dataset nil))
     (dolist (ut u-list)
       (dolist (m m-list)
	 (do ((n (+ m 1) (+ n 1)))
	     ((> n (* m n-m-ratio)))
	   (do ((k 1 (+ k 1)))
	       ((> k dataset-power))
	     (prog ((U 0) UC (tasks nil))
		(do ((i 1 (+ i 1)) (ul-list (uunifast n ut)) Di Ci ULi)
		    ((> i n))
		  (setf ULi (nth (- i 1) ul-list))
		  (if (equal ULi 0.0) (return-from generate-dataset2 (list n ut ul-list)))
		  (setf Ci (random-from-range 1 C-max))
		  (setf Di (max (ceiling (/ Ci (* ULi m))) Ci))
		  (if (equal Ci Di) (setf Di (+ Di 1)))
		  (setf U (+ U (/ Ci Di)))
		  (setf tasks (cons (make-task :C Ci :D Di :T Di) tasks)))
		(setf U (/ U m))
		(setf UC
		      (dolist (UC-elem uc-list)
			(if (< U UC-elem) (return UC-elem))))
		(setf dataset
		      (cons (make-taskset :M m :N n :U (float U) :UC UC :tasks tasks)
			    dataset)))))))
       (return dataset)))

;; Dataset3

(defun generate-dataset3
    (m-list n-m-ratio C-max dataset-power)
  (prog ((dataset nil))
       (dolist (m m-list)
	 (do ((n (+ m 1) (+ n 1)))
	     ((> n (* m n-m-ratio)))
	   (do ((k 1 (+ k 1)))
	       ((> k dataset-power))
	     (prog ((tasks nil))
		(do ((i 1 (+ i 1)) Di Ci ULi)
		    ((> i n))
		  (setf Ci (random-from-range 1 C-max))
		  (setf Di (random-from-range (+ Ci 1) (* 5 Ci)))
		  (setf tasks (cons (make-task :C Ci :D Di :T Di) tasks)))
		(setf dataset
		      (cons (make-taskset :M m :N n :U 0.0 :UC 0.0 :tasks tasks)
			    dataset))))))
       (return dataset)))

; Initializing

(setq u-list-val
      '(0.2 0.4 0.6 0.8 0.99))

(setq m-list-val
      '(1 2 3 4 5 6 7 8))

(setq uc-list-val '(0.2 0.4 0.6 0.8 1.0))

; Experiments

;; Experiment1

;;; Dataset Generating and Serializing

(setq dataset-val (generate-dataset1 u-list-val uc-list-val m-list-val 100 1000))

(print-dataset dataset-val "experiments/" "dataset1")

;;; Alg1's Output Generating, Handling and Serializing

(setq alg-output-val (apply-alg-to-dataset-uc #'Alg1 dataset-val uc-list-val 'schedulable))

(print-algorithm-output-to-csv-uc-total alg-output-val "experiments/" "Alg1" "Sched. Ratio")

;;; LeeShin2014's Output Generating, Handling and Serializing

(setq alg-output-val (apply-alg-to-dataset-uc #'LeeShin2014 qq uc-list-val 'schedulable))

(print-algorithm-output-to-csv-uc-total alg-output-val "experiments/" "LeeShin2014" "Sched. Ratio")

;;; BaekLee2020's Output Generating, Handling and Serializing

(setq alg-output-val (apply-alg-to-dataset-uc #'BaekLee2020 qq uc-list-val 'schedulable))

(print-algorithm-output-to-csv-uc-total alg-output-val "experiments/" "BaekLee2020" "Sched. Ratio")

;; Experiment2

(setq dataset-val (generate-dataset2 u-list-val uc-list-val m-list-val 2 100 100))

(print-dataset dataset-val "experiments/" "dataset2")

(setq alg-output-val (apply-alg-to-dataset-uc #'Alg2 dataset-val uc-list-val 'infeasible))

(print-algorithm-output-to-csv-uc-total alg-output-val "experiments/" "Alg2" "Infea. Ratio")


