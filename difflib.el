;;; difflib.el --- Helpers for computing deltas between objects. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/dieggsy/difflib.el
;; Git-Repository: git://github.com/dieggsy/difflib.el
;; Created: 2017-10-28
;; Version: 0.1.0
;; Keywords: matching tools string
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides an emacs-lisp port of python's difflib.

;;; Code:
(require 'cl-lib)
(require 'eieio)

(defvar difflib-pythonic-strings nil
  "Treat chars in strings as single-char-length strings.")

(defmacro difflib--alist-get (key alist &optional default)
  "Equivalent to `(alist-get KEY ALIST DEFAULT nil #'equal)'."
  `(alist-get ,key ,alist ,default nil #'equal))

(defun difflib--calculate-ratio (matches length)
  "When (> LENGTH 0), return (* 2.0 (/ (float MATCHES) LENGTH))."
  (if (> length 0)
      (* 2.0 (/ (float matches) length))
    1.0))

;;;###autoload
(defclass difflib-sequence-matcher ()
  ((isjunk :initarg :isjunk
           :initform nil
           :type (or null function)
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
  "`difflib-sequence-matcher' is a flexible class for comparing pairs of
sequences of any type, so long as the sequence elements are hashable. The basic
algorithm predates, and is a little fancier than, an algorithm published in the
late 1980's by Ratcliff and Obershelp under the hyperbolic name \"gestalt
pattern matching\". The basic idea is to find the longest contiguous matching
subsequence that contains no \"junk\" elements (R-O doesn't address junk). The
same idea is then applied recursively to the pieces of the sequences to the
left and to the right of the matching subsequence. This does not yield minimal
edit sequences, but does tend to yield matches that \"look right\" to people.

SequenceMatcher tries to compute a \"human-friendly diff\" between two
sequences. Unlike e.g. UNIX(tm) diff, the fundamental notion is the longest
*contiguous* & junk-free matching subsequence. That's what catches peoples'
eyes. The Windows(tm) windiff has another interesting notion, pairing up
elements that appear uniquely in each sequence. That, and the method here,
appear to yield more intuitive difference reports than does diff. This method
appears to be the least vulnerable to synching up on blocks of \"junk lines\",
though (like blank lines in ordinary text files, or maybe \"<P>\" lines in HTML
files). That may be because this is the only method of the 3 that has a
*concept* of \"junk\" <wink>.

See the Differ class for a fancy human-friendly file differencer, which uses
SequenceMatcher both to compare sequences of lines, and to compare sequences of
characters within similar (near-matching) lines.

See also function `difflib-get-close-matches' in this module, which shows how
simple code building on SequenceMatcher can be used to do useful work.")

(cl-defmethod initialize-instance :after ((matcher difflib-sequence-matcher) &rest _args)
  "Construct a difflib-sequence-matcher."
  (difflib-set-seqs matcher (oref matcher :a) (oref matcher :b)))

(cl-defmethod difflib-set-seqs ((seq difflib-sequence-matcher) a b)
  "Set the two sequences to be compared to A and B."
  (difflib-set-seq1 seq a)
  (difflib-set-seq2 seq b))

(cl-defmethod difflib-set-seq1 ((matcher difflib-sequence-matcher) seq)
  "Set the first sequence to be compared to SEQ.

The second sequence to be compared is not changed.

difflib-sequence-matcher computes and caches detailed information about the
second sequence, so if you want to compare one sequence S against many
sequences, use .set_seq2(S) once and call .set_seq1(x) repeatedly for each of
the other sequences.

See also `difflib-set-seqs' and `difflib-set-seq2'."
  (oset matcher :a (if (and difflib-pythonic-strings (stringp seq))
                       (split-string seq "" 'omit-nulls)
                     seq))
  (oset matcher :matching-blocks nil)
  (oset matcher :opcodes nil))

(cl-defmethod difflib-set-seq2 ((matcher difflib-sequence-matcher) seq)
  "Set the second sequence to be compared to SEQ.

The first sequence to be compared is not changed.

difflib-sequence-matcher computes and caches detailed information about the
second sequence, so if you want to compare one sequence S against many
sequences, use .set_seq2(S) once and call .set_seq1(x) repeatedly for each of
the other sequences.

See also `difflib-set-seqs' and `difflib-set-seq1'.
"
  (oset matcher :b (if (and difflib-pythonic-strings (stringp seq))
                       (split-string seq "" 'omit-nulls)
                     seq))
  (oset matcher :matching-blocks nil)
  (oset matcher :opcodes nil)
  (oset matcher :fullbcount nil)
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
  "Find longest matching block in (cl-subseq a ALO AHI) and (cl-subseq b BLO BHI).

If slot :isjunk is not defined:

Return (i j k) surch that (cl-subseq a i (+ i k)) is equal to (cl-subseq b j (+ j k)), where
    (<= alo i (+ i k) ahi)
    (<= blo j (+ j k) bhi)
and for all (i' j' k') meeting those conditions,
    (>= k k')
    (<= i i')
    and (when (= i i') (<= j j'))

In other words, of all maximal matching blocks, return one that starts earliest
in a, and of all those maximal matching blocks that start earliest in a,return
the one that starts earliest in b.

If isjunk is defined, first the longest matching block is determined as above,
but with the additional restriction that no junk element appears in the block.
Then that block is extended as far as possible by matching (only) junk elements
on both sides. So the resulting block never matches on junk except as identical
junk happens to be adjacent to an \"interesting\" match."
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
  "Return list of triples describing matching subsequences.

Each triple is of the form (i j n), and means that
(equal (cl-subseq a i (+ i n)) (cl-subseq b j (+ j n))). The triples are
monotonically increasing in i and in j. It's also guaranteed that if (i j n)
and (i' j' n') are adjacent triples in the list, and the second is not the last
triple in the list, then (/= (+ i n) i') or (/= (+ j n) j'). IOW, adjacent
triples never describe adjacent equal blocks.

The last triple is a dummy, (list (length a) (length b) 0), and is the only
triple with (= n 0)."
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
                         (= (+ j1 k1) j2))
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
  "Return list of 5-tuples describing how to turn a into b.

Each tuple is of the form (tag i1 i2 j1 j2). The first tuple has (= i1 j1 0),
and remaining tuples have i1 equal to the i2 from the tuple preceding it, and
likewise for j1 equal to the previous j2.

The tags are strings, with these meanings:

'replace':  (cl-subseq a i1 i2) should be replaced by (cl-subseq b j1 j2)
 'delete':  (cl-subseq a i1 i2) should be deleted.
            Note that (= j1 j2) in this case.
 'insert':  (cl-subseq b j1 j3) should be inserted at (cl-subseq a i1 i1).
            Note that (= i1 i2) in this case.
 'equal':   (equal (cl-subseq a i1 i3) (cl-subseq b j1 j2))."
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
                (when (not (string= tag ""))
                  (push (list tag i ai j bj) answer))
                (setq i (+ ai size)
                      j (+ bj size))
                (when (/= size 0)
                  (push (list "equal" ai i bj j) answer)))))
      (setf answer (reverse answer)))))

(cl-defmethod difflib-get-grouped-opcodes ((matcher difflib-sequence-matcher) &key (n 3))
  "Isolate change clusters by eliminating ranges with no changes.

Return a generator of groups with up to n lines of context.
Each group is in the same format as returned by `difflib-get-opcodes'."
  (let ((codes (difflib-get-opcodes matcher))
        tag
        i1
        i2
        j1
        j2
        groups)
    (when (not codes)
      (setq codes `(("equal" 0 1 0 1))))
    (let ((first (car codes)))
      (when (string= (car first) "equal")
        (setq tag (car first)
              i1 (cadr first)
              i2 (caddr first)
              j1 (nth 3 first)
              j2 (nth 4 first))
        (setcar codes (list tag (max i1 (- i2 n)) i2 (max j1 (- j2 n)) j2))))
    (let ((last (car (last codes))))
      (when (string= (car last) "equal")
        (setq tag (car last)
              i1 (cadr last)
              i2 (caddr last)
              j1 (nth 3 last)
              j2 (nth 4 last))
        (setf (elt codes (1- (length codes)))
              (list tag i1 (min i2 (+ n i1)) j1 (min j2 (+ j1 n))))))
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
      (reverse groups))))

(cl-defmethod difflib-ratio ((matcher difflib-sequence-matcher))
  "Return a measure of the sequences' similarity.

This is (/ (* M 2) (float T)) where T is the total number of elements in both
sequences and M is the number of matches.
Note that this is 1 if the sequences are identical, and 0 if they have nothing
in common.

difflib-ratio is expensive to compute if you haven't already computed
`difflib-get-matching-blocks' or `difflib-get-opcodes',in which case you may
want to try `difflib-quick-ratio' or `difflib-real-quick-ratio' first to get an
upper bound."
  (let ((matches (apply '+ (mapcar (lambda (lst) (car (last lst)))
                                   (difflib-get-matching-blocks matcher)))))
    (difflib--calculate-ratio matches
                              (+
                               (length (oref matcher :a))
                               (length (oref matcher :b))))))

(cl-defmethod difflib-quick-ratio ((matcher difflib-sequence-matcher))
  "Return an upper bound on `difflib-ratio' relatively quickly.

This isn't defined beyond that it is an upper bound on `difflib-ratio', and is
faster to compute."
  (cl-symbol-macrolet ((fullbcount (oref matcher :fullbcount))
                       (b (oref matcher :b))
                       (a (oref matcher :a)))
    (when (not fullbcount)
      (cl-loop for elt being the elements of b
               do (setf (difflib--alist-get elt fullbcount)
                        (1+ (difflib--alist-get elt fullbcount 0)))))
    (let (avail
          numb
          (matches 0))
      (cl-loop for elt being the elements of a
               do (let ((availhas (difflib--alist-get elt avail)))
                    (if availhas
                        (setq numb availhas)
                      (setq numb (difflib--alist-get elt fullbcount 0)))
                    (setf (difflib--alist-get elt avail) (1- numb))
                    (when (> numb 0)
                      (setq matches (1+ matches)))))
      (difflib--calculate-ratio matches (+ (length a)
                                           (length b))))))

(cl-defmethod difflib-real-quick-ratio ((matcher difflib-sequence-matcher))
  "Return an upper bound on `difflib-ratio' very quickly.

This isn't defined beyond that it is an upper bound on .ratio(), and is faster
to compute than either `difflib-ratio' or `difflib-quick-ratio'."

  (let ((la (length (oref matcher :a)))
        (lb (length (oref matcher :b))))
    (difflib--calculate-ratio (min la lb) (+ la lb))))

;;;###autoload
(cl-defun difflib-get-close-matches (word possibilities &key (n 3) (cutoff 0.6))
  "Use SequenceMatcher to return list of the best \"good enough\" matches.

WORD is a sequence for which close matches are desired (typically a string).

POSSIBILITIES is a list of sequences against which to match word
(typically a list of strings).

Optional arg N (default 3) is the maximum number of close matches to return. N
must be > 0.

Optional arg CUTOFF (default 0.6) is a float in [0, 1]. Possibilities that
don't score at least that similar to word are ignored.

The best (no more than N) matches among the POSSIBILITES are returned in a
list, sorted by similarity score, most similar first."
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

(defun difflib--count-leading (line ch)
  (let ((i 0)
        (n (length line)))
    (while (and (< i n) (= (elt line i) ch))
      (setq i (1+ i)))
    i))

(defclass difflib-differ ()
  ((linejunk :initarg :linejunk
             :initform nil
             :type (or null function)
             :documentation "A function that should accept a single string argument,and return true iff the string is junk. The module-level function `difflib-is-line-junk-p' may be used to filter out lines without visible characters, except for at most one splat ('#').  It is recommended to leave linejunk nil; the underlying difflib-sequence-matcher class has an adaptive notion of \"noise\" lines that's better than any static definition the author has ever been able to craft.")
   (charjunk :initarg :charjunk
             :initform nil
             :type (or null function)
             :documentation "A function that should accept a string of length 1. The module-level function `difflib-is-character-junk-p' may be used to filter out whitespace characters (a blank or tab; **note**: bad idea to include newline in this!).  Use of `difflib-is-character-junk-p' is recommended."))
  "`difflib-differ' is a class for comparing sequences of lines of text,
and producing human-readable differences or deltas. The differ uses
`difflib-sequence-matcher' both to compare sequences of lines, and to compare
sequences of characters within similar (near-matching) lines.

Each line of a differ delta begins with a two-letter code:

    '- '    line unique to sequence 1
    '+ '    line unique to sequence 2
    '  '    line common to both sequences
    '? '    line not present in either input sequence

Lines beginning with '? ' attempt to guide the eye to intraline differences,
and were not present in either input sequence. These lines can be confusing if
the sequences contain tab characters.

Note that differ makes no claim to produce a *minimal* diff. To the contrary,
minimal diffs are often counter-intuitive, because they synch up anywhere
possible, sometimes accidental matches 100 pages apart. Restricting synch
points to contiguous matches preserves some notion of locality, at the
occasional cost of producing a longer diff.")

(cl-defmethod difflib-compare ((differ difflib-differ) a b)
  "Compare two sequences of lines; generate the resulting delta.

Each sequence must contain individual single-line strings ending with newlines.
The delta generated consists of newline- terminated strings."
  (let ((cruncher (difflib-sequence-matcher :isjunk (oref differ :linejunk) :a a :b b))
        g)
    (cl-loop for (tag alo ahi blo bhi) in (difflib-get-opcodes cruncher)
             do (pcase tag
                  ("replace"
                   (setq g (difflib--fancy-replace differ a alo ahi b blo bhi)))
                  ("delete" (setq g (difflib--dump "-" a alo ahi)))
                  ("insert" (setq g (difflib--dump "+" b blo bhi)))
                  ("equal" (setq g (difflib--dump " " a alo ahi)))
                  (_ (error "Unknown tag %s" tag)))
             append g)))

(cl-defmethod difflib--dump ((differ difflib-differ) tag x lo hi)
  (cl-loop for i in (number-sequence lo (1- hi))
           collect (format "%s %s" tag (elt x i))))

(cl-defmethod difflib--plain-replace ((differ difflib-differ) a alo ahi b blo bhi)
  (cl-assert (and (< alo ahi) (< blo bhi)))
  (let (first
        second)
    (if (< (- bhi blo) (- ahi alo))
        (progn
          (setq first (difflib--dump differ "+" b blo bhi))
          (setq second (difflib--dump differ "-" a alo ahi)))
      (setq first (difflib--dump differ "-" a alo ahi))
      (setq second (difflib--dump differ "+" b blo bhi)))))

(cl-defmethod difflib--fancy-replace ((differ difflib-differ) a alo ahi b blo bhi)
  (let ((best-ratio 0.74)
        (cutoff 0.75)
        (cruncher (difflib-sequence-matcher (oref differ :charjunk)))
        (outer-range (number-sequence blo (1- bhi)))
        (inner-range (number-sequence alo (1- ahi)))
        eqi
        eqj
        best-i
        best-j
        results)
    (cl-block fancy-replace
      (cl-loop for j in outer-range
               as bj = (elt b j)
               do (difflib-set-seq2 cruncher bj)
               do (cl-loop
                   for i in inner-range
                   as ai = (elt a i)
                   do (cl-block inner
                        (when (equal ai bj)
                          (when (not eqi)
                            (setq eqi i
                                  eqj j))
                          (cl-return-from inner))
                        (difflib-set-seq1 cruncher ai)
                        (when (and (> (difflib-real-quick-ratio cruncher)
                                      best-ratio)
                                   (> (difflib-quick-ratio cruncher)
                                      best-ratio)
                                   (> (difflib-ratio cruncher)
                                      best-ratio))
                          (setq best-ratio (difflib-ratio cruncher)
                                best-i i
                                best-j j)))))
      (if (< best-ratio cutoff)
          (progn
            (when (not eqi)
              (cl-return-from fancy-replace
                (difflib--plain-replace differ a alo ahi b blo bhi)))
            (setq best-i eqi
                  best-j eqj
                  best-ratio 1.0))
        (setq eqi nil))
      (setq results (append results (difflib--fancy-helper differ a alo best-i b blo best-j)))
      (let ((aelt (elt a best-i))
            (belt (elt b best-j)))
        (if (not eqi)
            (let ((atags "")
                  (btags ""))
              (difflib-set-seqs cruncher aelt belt)
              (cl-loop for (tag ai1 ai2 bj1 bj2) in (difflib-get-opcodes cruncher)
                       as la = (- ai2 ai1)
                       as lb = (- bj2 bj1)
                       do (pcase tag
                            ("replace"
                             (setq atags (concat atags (make-string la ?^)))
                             (setq btags (concat btags (make-string lb ?^))))
                            ("delete"
                             (setq atags (concat atags (make-string la ?-))))
                            ("insert"
                             (setq btags (concat btags (make-string lb ?+))))
                            ("equal"
                             (setq atags (concat atags (make-string  la ?\s)))
                             (setq btags (concat btags (make-string  lb ?\s))))
                            (_ (error "Unknown tag %s" tag))))
              (setq results (append results (difflib--qformat differ aelt belt atags btags))))
          (setq results (append results (list (concat "  " aelt)))))
        (setq results (append results (difflib--fancy-helper differ a (1+ best-i) ahi b (1+ best-j) bhi)))))))

(cl-defmethod difflib--fancy-helper ((differ difflib-differ) a alo ahi b blo bhi)
  (cond ((< alo ahi)
         (if (< blo bhi)
             (difflib--fancy-replace differ a alo ahi b blo bhi)
           (difflib--dump differ "-" a alo ahi)))
        ((< blo bhi)
         (difflib--dump differ "+" b blo bhi))))

(cl-defmethod difflib--qformat ((differ difflib-differ) aline bline atags btags)
  (let* (result
         (common (min (difflib--count-leading aline ?\t)
                      (difflib--count-leading bline ?\t)))
         (common (min common (difflib--count-leading (cl-subseq atags 0 common)
                                                     ?\s)))
         (common (min common (difflib--count-leading (cl-subseq btags 0 common)
                                                     ?\s)))
         (atags (string-trim-right (cl-subseq atags common)))
         (btags (string-trim-right (cl-subseq btags common))))
    (push (concat "- " aline) result)
    (message "ATAGS: %s" atags)
    (when (not (string= atags ""))
      (push (format "? %s%s\n"
                    (make-string common ?\t)
                    atags)
            result))
    (push (concat "+ " bline) result)
    (when (not (string= btags ""))
      (push (format "? %s%s\n"
                    (make-string common ?\t)
                    btags)
            result))
    (reverse result)))

(provide 'difflib)

;;; difflib.el ends here
