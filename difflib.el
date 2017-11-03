(require 'cl-lib)
(require 'eieio)

(defvar difflib-pythonic-strings nil)

(defmacro difflib--alist-get (key alist &optional default)
  `(alist-get ,key ,alist ,default nil #'equal))

(defun difflib--calculate-ratio (matches length)
  (if (> length 0)
      (* 2.0 (/ (float matches) length))
    1.0))

(defclass difflib-sequence-matcher ()
  ((isjunk :initarg :isjunk
           :initform nil
           :type (or cons symbol)
           :documentation "A one-argument function that takes a sequence element and returns true if the element is junk. Nil means no element is considered junk.")
   (a :initarg :a
      :initform ""
      :type sequence
      :documentation "The first of two sequences to be compared. By default, an empty string.")
   (b :initarg :b
      :initform ""
      :type sequence
      :documentation "The second of two sequences to be compared. By default,an empty string.")
   (autojunk :initarg :autojunk
             :initform t
             :type boolean
             :documentation "Should be set to False to disable the \"automatic junk heuristic\" that treants popular elements as junk.")
   (b2j :initarg :b2j
        :initform nil
        :type list
        :documentation "For x in b, (alist-get x b2j) is ")
   (fullbcount :initarg :fullbcount
               :initform nil
               :type list
               :documentation "For x in b, the number of times x appears in b.")
   (matching-blocks :initarg :matching-blocks
                    :initform nil
                    :type list
                    :documentation "A list of (i j k) triples.")
   (opcodes :initarg :opcodes
            :initform nil
            :type list
            :documentation "A list of '(tag i1 i3 j1 j2).")
   (bjunk :initarg :bjunk
          :initform nil
          :type list
          :documentation "The items in b for which isjunk is t.")
   (bpopular :initarg :bpopular
             :initform nil
             :type list
             :documentation "Nonjunk items in b treated as junk by the heuristic (if used)."))
  "SequenceMatcher is a flexible class for comparing pairs of sequences of any
type, so long as the sequence elements are hashable. The basic algorithm
predates, and is a little fancier than, an algorithm published in the late
1980's by Ratcliff and Obershelp under the hyperbolic name \"gestalt pattern
matching\". The basic idea is to find the longest contiguous matching
subsequence that contains no \"junk\" elements (R-O doesn't address junk). The
same idea is then applied recursively to the pieces of the sequences to the
left and to the right of the matching subsequence. This does not yield minimal
edit sequences, but does tend to yield matches that \"look right\" to people.

SequenceMatcher tries to compute a \"human-friendly diff\" between two
sequences. Unlike e.g. UNIX(tm) diff, the fundamental notion is the longest
*contiguous* & junk-free matching subsequence. That's what catches peoples'
eyes. The Windows(tm) windiff has another interesting notion, pairing
up elements that appear uniquely in each sequence. That, and the method here,
appear to yield more intuitive difference reports than does diff. This method
appears to be the least vulnerable to synching up on blocks of \"junk lines\",
though (like blank lines in ordinary text files, or maybe \"<P>\" lines in HTML
files). That may be because this is the only method of the 3 that has a
*concept* of \"junk\" <wink>.")

(defmethod initialize-instance :after ((matcher difflib-sequence-matcher) &rest args)
  (difflib-set-seqs matcher (oref matcher :a) (oref matcher :b)))

(cl-defmethod difflib-set-seqs ((seq difflib-sequence-matcher) a b)
  (difflib-set-seq1 seq a)
  (difflib-set-seq2 seq b))

(cl-defmethod difflib-set-seq1 ((matcher difflib-sequence-matcher) seq)
  (oset matcher :a (if (and difflib-pythonic-strings (stringp seq))
                       (split-string seq "" 'omit-nulls)
                     seq))
  (oset matcher matching-blocks nil)
  (oset matcher opcodes nil))

(cl-defmethod difflib-set-seq2 ((matcher difflib-sequence-matcher) seq)
  (oset matcher :b (if (and difflib-pythonic-strings (stringp seq))
                       (split-string seq "" 'omit-nulls)
                     seq))
  (oset matcher matching-blocks nil)
  (oset matcher opcodes nil)
  (oset matcher fullbcount nil)
  (difflib--chain-b matcher))

(cl-defmethod difflib--chain-b ((matcher difflib-sequence-matcher))
  (cl-symbol-macrolet ((b (oref matcher :b))
                       (b2j (oref matcher :b2j))
                       (junk (oref matcher :bjunk))
                       (isjunk (oref matcher :isjunk))
                       (popular (oref matcher :bpopular)))
    (cl-loop
     for elt being the elements of b
     as i = 0 then (1+ i)
     do (cl-symbol-macrolet ((exists (difflib--alist-get elt b2j)))
          (when (not exists)
            (setf exists nil))
          (setf exists (append exists (list i)))))
    (when isjunk
      (cl-loop
       for (elt . val) in b2j
       if (funcall isjunk elt)
       do (unless (member elt junk)
            (push elt junk)))
      (cl-loop
       for elt in junk
       do (oset matcher b2j (delq (assoc elt b2j) b2j))))
    (let ((n (length b)))
      (when (and (oref matcher :autojunk) (>= n 200))
        (let ((ntest (1+ (/ n 100))))
          (cl-loop for (elt . idxs) in b2j
                   if (> (length idxs) ntest)
                   do (unless (member elt popular)
                        (push elt popular)))
          (cl-loop for elt in popular
                   do (oset matcher b2j (delq (assoc elt b2j) b2j))))))))

(cl-defmethod difflib-find-longest-match ((matcher difflib-sequence-matcher) alo ahi blo bhi)
  (cl-symbol-macrolet ((a (oref matcher :a))
                       (b (oref matcher :b))
                       (b2j (oref matcher :b2j))
                       (bjunk (oref matcher :bjunk)))
    (let ((besti alo)
          (bestj blo)
          (bestsize 0)
          j2len
          nothing)
      (cl-loop
       for i in (number-sequence alo (1- ahi))
       as newj2len = nil
       do (cl-loop
           named inner
           for j in (difflib--alist-get (elt a i) b2j nothing)
           do (cond ((< j blo) nil)
                    ((>= j bhi)
                     (cl-return-from inner))
                    (t (let ((k (1+ (difflib--alist-get (1- j) j2len 0))))
                         (setf (difflib--alist-get j newj2len) k)
                         (if (> k bestsize)
                             (setq besti (1+ (- i k))
                                   bestj (1+ (- j k))
                                   bestsize k))))))
       do (setq j2len newj2len))
      (while (and (> besti alo)
                  (> bestj blo)
                  (not (member (elt b (1- bestj))  bjunk))
                  (equal (elt a (1- besti))
                         (elt b (1- bestj))))
        (setq besti (1- besti)
              bestj (1- bestj)
              bestsize (1+ bestsize)))
      (while (and (< (+ besti bestsize) ahi)
                  (< (+ bestj bestsize) bhi)
                  (not (member (elt b (+ bestj bestsize)) bjunk))
                  (equal (elt a (+ besti bestsize))
                         (elt b (+ bestj bestsize))))
        (setq bestsize (1+ bestsize)))
      (while (and (> besti alo)
                  (> bestj blo)
                  (member (elt b (1- bestj)) bjunk)
                  (equal (elt a (1- besti))
                         (elt b (1- bestj))))
        (setq besti (1- besti)
              bestj (1- bestj)
              bestsize (1+ bestsize)))
      (while (and (< (+ besti bestsize) ahi)
                  (< (+ bestj bestsize) bhi)
                  (member (elt b (+ bestj bestsize)) bjunk)
                  (equal (elt a (+ besti bestsize))
                         (elt b (+ bestj bestsize))))
        (setq bestsize (1+ bestsize)))
      (list besti bestj bestsize))))

(cl-defmethod difflib-get-matching-blocks ((matcher difflib-sequence-matcher))
  (if (oref matcher :matching-blocks)
      (oref matcher :matching-blocks)
    (let* ((la (length (oref matcher :a)))
           (lb (length (oref matcher :b)))
           (queue `((0 ,la 0 ,lb)))
           matching-blocks)
      (while queue
        (cl-destructuring-bind (alo ahi blo bhi) (pop queue)
          (let ((x (difflib-find-longest-match matcher alo ahi blo bhi)))
            (cl-destructuring-bind (i j k) x
              (when (/= k 0)
                (push x matching-blocks)
                (when (and (< alo i) (< blo j))
                  (push (list alo i blo j) queue))
                (when (and (< (+ i k) ahi) (< (+ j k) bhi))
                  (push (list (+ i k) ahi (+ j k) bhi) queue)))))))
      (setq matching-blocks
            (sort matching-blocks ;; TODO: SORT BY WAT
                  (lambda (a b)
                    (if (= (car a) (car b))
                        (if (= (cadr a) (cadr b))
                            (< (caddr a) (caddr b))
                          (< (cadr a) (cadr b)))
                      (< (car a) (car b))))))
      (let ((i1 0)
            (j1 0)
            (k1 0)
            non-adjacent)
        (cl-loop for (i2 j2 k2) in matching-blocks
                 if (and (= (+ i1 k1) i2)
                         (= (+ i1 k1) j2))
                 do (setq k1 (+ k1 k2))
                 else
                 do (progn
                      (when k1
                        (push (list i1 j1 k1) non-adjacent))
                      (setq i1 i2
                            j1 j2
                            k1 k2)))
        (when k1
          (push (list i1 j1 k1) non-adjacent))
        (push (list la lb 0) non-adjacent)
        ;; (setq non-adjacent (reverse non-adjacent))
        (oset matcher :matching-blocks (reverse non-adjacent))))))

(cl-defmethod difflib-get-opcodes ((matcher difflib-sequence-matcher))
  (if (oref matcher :opcodes)
      (oref matcher :opcodes)
    (cl-symbol-macrolet ((answer (oref matcher :opcodes)))
      (let ((i 0)
            (j 0))
        (cl-loop
         for (ai bj size) in (difflib-get-matching-blocks matcher)
         as tag = ""
         do
         (progn (cond ((and (< i ai) (< j bj))
                       (setq tag "replace"))
                      ((< i ai)
                       (setq tag "delete"))
                      ((< j bj)
                       (setq tag "insert")))
                (when (not (string-empty-p tag))
                  (push (list tag i ai j bj) answer))
                (setq i (+ ai size)
                      j (+ bj size))
                (when (/= size 0)
                  (push (list "equal" ai i bj j) answer)))))
      (setf answer (reverse answer)))))

(cl-defmethod difflib-get-grouped-opcodes ((matcher difflib-sequence-matcher) &key (n 3))
  (cl-block grouped-opcodes
    (let ((codes (difflib-get-opcodes matcher))
          tag
          i2
          j1
          j2
          groups)
      (when (not codes)
        (setq codes `(("equal" 0 1 0 1))))
      (message "CODES: %S" codes)
      (let ((first (car codes)))
        (when (string= (car first) "equal")
          (setq tag (car first)
                i1 (cadr first)
                i2 (caddr first)
                j1 (nth 3 first)
                j2 (nth 4 first))
          (setcar codes (list tag (max i1 (- i2 n)) i2 (max j1 (- j2 n)) j2))))
      (message "CODES: %S" codes)
      (let ((last (car (last codes))))
        (when (string= (car last) "equal")
          (message "THIRDIF")
          (setq tag (car last)
                i1 (cadr last)
                i2 (caddr last)
                j1 (nth 3 last)
                j2 (nth 4 last))
          (setf (elt codes (1- (length codes))) (list tag i1 (min i2 (+ n i1)) j1 (min j2 (+ j1 n))))))
      (message "CODES: %S" codes)
      (let ((nn (* 2 n))
            group)
        (cl-loop
         for (tag i1 i2 j1 j2) in codes
         do (progn
              (when (and (string= tag "equal")
                         (> (- i2 i1) nn))
                (push (list tag i1 (min i2 (+ i1 n)) j1 (min j2 (+ j1 n)))
                      group)
                (push (reverse group) groups)
                (setq group '())
                (setq i1 (max i1 (- i2 n))
                      j1 (max j1 (- j2 n))))
              (push (list tag i1 i2 j1 j2) group)))
        (when (and group (not (= (length group) 1)) (string= (caar group) "equal"))
          (push (reverse group) groups))
        (reverse groups)))))

(cl-defmethod difflib-ratio ((matcher difflib-sequence-matcher))
  (let ((matches (apply '+ (mapcar (lambda (lst) (car (last lst)))
                                   (difflib-get-matching-blocks matcher)))))
    (difflib--calculate-ratio matches
                              (+
                               (length (oref matcher :a))
                               (length (oref matcher :b))))))

(cl-defmethod difflib-quick-ratio ((matcher difflib-sequence-matcher))
  (cl-symbol-macrolet ((fullbcount (oref matcher :fullbcount))
                       (b (oref matcher :b))
                       (a (oref matcher :a)))
    (when (not fullbcount)
      (cl-loop for elt being the elements of b
               do (setf (difflib-alist-get elt fullbcount)
                        (1+ (difflib-alist-get elt fullbcount 0)))))
    (let (avail
          numb
          (matches 0))
      (cl-loop for elt being the elements of a
               do (let ((availhas (difflib-alist-get elt avail)))
                    (if availhas
                        (setq numb availhas)
                      (setq numb (difflib-alist-get elt fullbcount 0)))
                    (setf (difflib-alist-get elt avail) (1- numb))
                    (when (> numb 0)
                      (setq matches (1+ matches)))))
      (difflib--calculate-ratio matches (+ (length a)
                                           (length b))))))

(cl-defmethod difflib-real-quick-ratio ((matcher difflib-sequence-matcher))
  (let ((la (length (oref matcher :a)))
        (lb (length (oref matcher :b))))
    (difflib--calculate-ratio (min la lb) (+ la lb))))

(cl-defun difflib-get-close-matches (word possibilities &key (n 3) (cutoff 0.6))
  (when (not (> n 0))
    (error "N must be > 0: %S" n))
  (when (not (<= 0.0 cutoff 1.0))
    (error "CUTOFF must be in [0.0, 1.0]: %S" cutoff))
  (let (result
        (s (difflib-sequence-matcher)))
    (difflib-set-seq2 s word)
    (cl-loop for x being the elements of possibilities
             do (difflib-set-seq1 s x)
             if (and (>= (difflib-real-quick-ratio s) cutoff)
                     (>= (difflib-quick-ratio s) cutoff)
                     (>= (difflib-ratio s) cutoff))
             do (push (cons x (difflib-ratio s)) result))
    (let ((res (cl-sort result #'> :key (lambda (cns) (cdr cns)))))
      (setq result (cl-subseq
                    res
                    0
                    (if (< (length res) n)
                        (length res)
                      n))))
    (mapcar (lambda (lst) (car lst)) result)))

(provide 'difflib)
