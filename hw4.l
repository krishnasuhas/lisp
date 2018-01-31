;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defun all-length(input)(cond ((null input)0)((not (listp (car input)))     (+ 1 (all-length (cdr input))) )
(t(+ (all-length (car input))(all-length (cdr input))))))


(defun range(a)(let ((h (car a))(l (car a)))
(defun maxmin(b)(cond ((> (length b) 0)
(if (< h (car b)) (setq h (car b)))(if (> l (car b)) (setq l (car b)))
(maxmin (cdr b)))))(maxmin a)(list l h)))

(defun before(s e input)
(let ((taken 0)(result nil))

(defun minimise (s e input)(if (= (length input) 0) (return-from minimise nil))
(if (and (equal s (car input)) (= taken 0))(setq taken 1))
(if (= taken 1)(setq result (cons  (car input) result)))
(if (and (equal e (car input)) (= taken 1))(setf taken 10))(minimise s e (cdr input) ))

(minimise s e input)
(setq result (reverse result))
result))


(defun split-if(fun input)
(let ((list1 nil)(list2 nil))
(defun split(input)(if (= (length input) 0) (return-from split input))
(if (null (funcall fun (car input)) ) (setq list1 (cons (car input) list1)))
(if (funcall fun (car input)) (setq list2 (cons (car input) list2)))(split (cdr input)))
(split input)(list (reverse list1) (reverse list2))))


(defun group(input n)
(let ((count 0)(final nil)(sub nil))
(defun grp (input n)(if (= (length input) 0) (return-from grp input))
(setq sub (cons (car input) sub))(setq count (+ count 1))
(cond ((or (= count n) (= 1 (length input)))(setq final (cons (reverse sub) final))(setf sub nil)(setf count 0)))
(grp (cdr input) n))(grp input n)(reverse final)))


(defun mostn(fun input)(let ((vals nil)(pos nil)(final nil)(count 0)(maxval 0))

(defun valsfun(a)(if (= (length a) 0)(setq vals (reverse vals)))(cond ((> (length a) 0)(setq vals (cons (funcall fun (car a)) vals)) (valsfun (cdr a)))))

(defun maxi(b)(cond ((> (length b) 0)(if (< maxval (car b)) (setq maxval (car b)))(maxi (cdr b)))))

(defun posfun(c)(if (= (length c) 0)(setq pos (reverse pos)))(cond ((> (length c) 0)(if (= (funcall fun (car c)) maxval)(setq pos (cons count pos))) (setf count (+ count 1))(posfun (cdr c )))))

(defun sets(d)(cond ((= (length d) 0)(setq final (cons maxval (list (reverse final))))(setf final (reverse final))))(cond ((> (length d) 0)(setq final (cons (nth (car d) input) final)) (sets (cdr d)))))

(valsfun input)(setq maxval (car vals))(maxi vals)(posfun input)(sets pos)final))



(defun match(pattern assertion)
(if (or (not (null pattern))(not (null assertion)))
    (let ((flag 0)(samp nil)(samp2 nil)(count 0)(temp2 nil))

        (cond   ((or (equal (car pattern) (car assertion))(equal (car pattern) '?))(if (and (= (length pattern) 1)(= (length assertion) 1)) (return-from match T))(match (cdr pattern)(cdr assertion)))
                
                ((equal (car pattern) '!)(setq temp2 (cadr pattern) )(setq pattern (cdr pattern))(= count 0)
                (defun reset(asser)(cond ( (equal (car asser) temp2) (return-from reset nil))
                ((and (not (equal (car asser) temp2)) (not (null asser)))
                (setq pattern (cons (car asser) pattern))(setq count (+ count 1))(reset (cdr asser)))))
                (reset assertion)(cond ((> count 1)(setq samp (subseq pattern 0 count))(setq samp (reverse samp))
                (setq samp2 (subseq pattern count (length pattern)))(setq samp2 (append samp samp2))(setq pattern samp2)))
                
                (= count 0)(match pattern assertion))
                ((not(or(equal(car pattern)(car assertion))(equal (car pattern)'?)))(= flag 1)(return-from match nil)(return-from match nil))))))




(defun match-var (pat ass)
    (if (and (null pat) (null ass)) (return-from match-var T))
    (if (and (null pat) (not (null ass))) (return-from match-var nil))
    (if (and (null ass) (not (null pat))) (return-from match-var nil))
    (when (not (null ass))
        
        (when (atom (car pat))
            (if (equal (car pat) (car ass))
                (return-from match-var (match-var (cdr pat) (cdr ass)))
            )
        )
        (when (and (listp (car pat)) (equal (caar pat) '?))
            (set (cadar pat) (car ass))
            (return-from match-var (match-var (cdr pat) (cdr ass)))
        )
        (when (and (listp (car pat)) (equal (caar pat) '!))
            (setq temp nil)
            (loop 
                (when (listp (cadr pat))(return nil))
                (when (null ass) (return nil))
                (when (equal (cadr pat) (cadr ass)) (return T))
                (setq temp (cons (car ass) temp)) 
                (setq ass (cdr ass))
            )
            (setq temp (cons (car ass) temp))
            (when (equal (cadr ass) (car temp))
                 (setq ass (cdr ass))
                 (setq temp (cons (car ass) temp))
            )
            (set (cadar pat) (reverse temp))
            (return-from match-var (match-var (cdr pat) (cdr ass)))
        )
        
    )
)






















