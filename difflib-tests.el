(require 'difflib)
(require 'cl-lib)

(ert-deftest difflib-test-sequence-matcher-example ()
  ;; SequenceMatcher docstring
  (let ((s (difflib-sequence-matcher
            :isjunk (lambda (x) (equal x (if difflib-pythonic-strings
                                             " "
                                           ?\s)))
            :a "private Thread currentThread;"
            :b "private volatile Thread currentThread;")))
    (should (equal (string-to-number (format "%.3f" (difflib-ratio s)))
                   0.866))
    (should (equal (difflib-get-matching-blocks s)
                   '((0 0 8)
                     (8 17 21)
                     (29 38 0))))
    (should (equal (difflib-get-opcodes s)
                   '(("equal" 0 8 0 8)
                     ("insert" 8 8 8 17)
                     ("equal" 8 29 17 38))))))
(ert-deftest difflib-test-set-seq-example ()
  (let ((s (difflib-sequence-matcher :a "abcd" :b "bcde")))
    (should (cl-equalp (difflib-ratio s)
                       0.75))
    (difflib-set-seq1 s "bcde")
    (should (cl-equalp (difflib-ratio s)
                       1.0))
    (difflib-set-seq1 s "abcd")
    (difflib-set-seq2 s "abcd")
    (should (cl-equalp (difflib-ratio s)
                       1.0))))

(ert-deftest difflib-test-find-longest-match-example ()
  (let ((s (difflib-sequence-matcher :a " abcd" :b "abcd abcd")))
    (should (equal (difflib-find-longest-match s 0 5 0 9)
                   '(0 4 5))))
  (let ((s (difflib-sequence-matcher
            :isjunk (lambda (x) (equal x (if difflib-pythonic-strings
                                             " "
                                           ?\s)))
            :a " abcd"
            :b "abcd abcd")))
    (should (equal (difflib-find-longest-match s 0 5 0 9)
                   '(1 0 4))))
  (let ((s (difflib-sequence-matcher :a "ab" :b "c")))
    (should (equal (difflib-find-longest-match s 0 2 0 1)
                   '(0 0 0)))))

(ert-deftest difflib-test-get-matching-blocks-example ()
  (let ((s (difflib-sequence-matcher :a "abxcd" :b "abcd")))
    (should (equal (difflib-get-matching-blocks s)
                   '((0 0 2)
                     (3 2 2)
                     (5 4 0))))))

(ert-deftest difflib-test-get-opcodes-example ()
  (let* ((a "qabxcd")
         (b "abycdf")
         (s (difflib-sequence-matcher :a a :b b))
         (opcodes (difflib-get-opcodes s)))
    (should
     (equal
      (cl-loop for (tag i1 i2 j1 j2) in opcodes
               collect
               (list tag i1 i2 (cl-subseq a i1 i2) j1 j2 (cl-subseq b j1 j2)))
      '(("delete" 0 1 "q" 0 0 "")
        ("equal" 1 3 "ab" 0 2 "ab")
        ("replace" 3 4 "x" 2 3 "y")
        ("equal" 4 6 "cd" 3 5 "cd")
        ("insert" 6 6 "" 5 6 "f"))))))

(ert-deftest difflib-test-get-grouped-opcodes-example ()
  (let* ((a (mapcar #'number-to-string (number-sequence 1 39)))
         (b '("1" "2" "3" "4" "5" "6" "7" "8" "i" "9" "10" "11"
              "12" "13" "14" "15" "16" "17" "18" "19" "20x" "21" "22"
              "28" "29" "30" "31" "32" "33" "34" "35y" "36" "37" "38" "39" )))
    ;; (push "i" (nthcdr 8 b))
    ;; (setf (elt b 20) (concat (elt b 20) "x"))
    ;; (setq b (append (cl-subseq b 0 23)
    ;;                 (cl-subseq b 28)))
    ;; (setf (elt b 30) (concat (elt b 30) "y"))
    ;; (message "BEE: %S" b)
    ;; (should
    ;;  (equal b
    ;;         '("1" "2" "3" "4" "5" "6" "7" "8" "i" "9" "10" "11"
    ;;           "12" "13" "14" "15" "16" "17" "18" "19" "20x" "21" "22"
    ;;           "28" "29" "30" "31" "32" "33" "34" "35y" "36" "37" "38" "39" )))
    (should
     (equal
      (difflib-get-grouped-opcodes (difflib-sequence-matcher :a a :b b))

      '((("equal" 5 8 5 8) ("insert" 8 8 8 9) ("equal" 8 11 9 12))
        (("equal" 16 19 17 20)
         ("replace" 19 20 20 21)
         ("equal" 20 22 21 23)
         ("delete" 22 27 23 23)
         ("equal" 27 30 23 26))
        (("equal" 31 34 27 30)
         ("replace" 34 35 30 31)
         ("equal" 35 38 31 34)))))))

(ert-deftest difflib-test-get-close-matches-example ()
  (should
   (equal (difflib-get-close-matches "appel" '("ape" "apple" "peach" "puppy"))
          '("apple" "ape")))
  (let ((python-keyword-list
         '("False" "None" "True" "and" "as" "assert" "break" "class" "continue"
           "def" "del" "elif" "else" "except" "finally" "for" "from" "global"
           "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
           "gaise" "return" "try" "while" "with" "yield")))
    (should
     (equal (difflib-get-close-matches "wheel" python-keyword-list)
            '("while")))
    (should
     (equal (difflib-get-close-matches "Apple" python-keyword-list)
            nil))
    (should
     (equal (difflib-get-close-matches "accept" python-keyword-list)
            '("except")))))


(ert-deftest difflib-test-one-insert ()
  (let ((sm (difflib-sequence-matcher :a (make-string 100 ?b)
                                      :b (concat "a"
                                                 (make-string 100 ?b)))))
    (should (equal (string-to-number (format "%.3f" (difflib-ratio sm)))
                   0.995))
    (should (equal (difflib-get-opcodes sm)
                   '(("insert" 0 0 0 1)
                     ("equal" 0 100 1 101))))
    (should (equal (oref sm :bpopular) nil)))
  (let ((sm (difflib-sequence-matcher :a (make-string 100 ?b)
                                      :b (concat (make-string 50 ?b)
                                                 "a"
                                                 (make-string 50 ?b)))))
    (should (equal (string-to-number (format "%.3f" (difflib-ratio sm)))
                   0.995))
    (should (equal (difflib-get-opcodes sm)
                   '(("equal" 0 50 0 50)
                     ("insert" 50 50 50 51)
                     ("equal" 50 100 51 101))))
    (should (equal (oref sm :bpopular) nil))))

(ert-deftest difflib-test-one-delete ()
  (let ((sm (difflib-sequence-matcher :a (concat (make-string 40 ?a)
                                                 "c"
                                                 (make-string 40 ?b))
                                      :b (concat (make-string 40 ?a)
                                                 (make-string 40 ?b)))))
    (should (equal (string-to-number (format "%.3f" (difflib-ratio sm)))
                   0.994))
    (should (equal (difflib-get-opcodes sm)
                   '(("equal" 0 40 0 40)
                     ("delete" 40 41 40 40)
                     ("equal" 41 81 40 80))))))

(ert-deftest difflib-test-bjunk ()
  (let ((sm (difflib-sequence-matcher
             :isjunk (lambda (x) (equal x (if difflib-pythonic-strings
                                              " "
                                            ?\s)))
             :a (concat (make-string 40 ?a)
                        (make-string 40 ?b))
             :b (concat (make-string 44 ?a)
                        (make-string 40 ?b)))))
    (should (equal (oref sm :bjunk) nil)))
  (let ((sm (difflib-sequence-matcher
             :isjunk (lambda (x) (equal x (if difflib-pythonic-strings
                                              " "
                                            ?\s)))
             :a (concat (make-string 40 ?a)
                        (make-string 40 ?b))
             :b (concat (make-string 44 ?a)
                        (make-string 40 ?b)
                        (make-string 20 ?\s)))))
    (should (equal (oref sm :bjunk) (if difflib-pythonic-strings
                                        '(" ")
                                      '(?\s)))))
  (let ((sm (difflib-sequence-matcher
             :isjunk (lambda (x) (member x
                                         (if difflib-pythonic-strings
                                             '(" " "b")
                                           '(?\s ?b))))
             :a (concat (make-string 40 ?a)
                        (make-string 40 ?b))
             :b (concat (make-string 44 ?a)
                        (make-string 40 ?b)
                        (make-string 20 ?\s)))))
    (should (equal (oref sm :bjunk) (if difflib-pythonic-strings
                                        '("b" " ")
                                      '(?b ?\s))))))

(ert-deftest difflib-test-autojunk ()
  (let ((seq1 (make-string 200 ?b))
        (seq2 (concat "a" (make-string 200 ?b))))
    (let ((sm (difflib-sequence-matcher :a seq1 :b seq2)))
      (should (cl-equalp (string-to-number (format "%.3f" (difflib-ratio sm)))
                         0))
      (should (equal (oref sm :bpopular) (if difflib-pythonic-strings
                                             '("b")
                                           '(?b)))))
    ;; Junk off
    (let ((sm (difflib-sequence-matcher :a seq1 :b seq2 :autojunk nil)))
      (should (cl-equalp (string-to-number (format "%.3f" (difflib-ratio sm)))
                         0.998))
      (should (equal (oref sm :bpopular) nil)))))

(ert-deftest difflib-test-ratio-for-null-seq ()
  (let ((s (difflib-sequence-matcher :a '() :b '())))
    (should (cl-equalp (difflib-ratio s) 1))
    (should (cl-equalp (difflib-quick-ratio s) 1))
    (should (cl-equalp (difflib-real-quick-ratio s) 1))))
