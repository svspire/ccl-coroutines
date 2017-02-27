;;; directory-structure-compare.lisp
;;; 15-Dec-2016 SVS
;;; requires ccl-coroutines-args.lisp

;;; Use directory-structure-compare to flag structural asimilarities below two given directories.

;;; Note that although the function directory-structure-compare does work, it has disadvantages:
;;; -- It's highly CCL-specific.
;;; -- It's very complicated because it uses coroutines instead of normal function calls. (I had
;;;     initially thought coroutines would be a great abstraction for writing this code. But coroutines
;;;     made it much more complicated than it should have been. The simpler example is
;;;     #'uniques-from-two-directories, which is both much easier to understand and 3x faster and more portable.)
;;; -- You really need to reference the directory-structure-compare-logic spreadsheet (which should be included
;;;     with this code) to follow what's going on.
;;; -- It serves mainly as an example for how to use coroutines, and as a test function for #'uniques-from-two-directories 
;;;    because #'directory-structure-compare and #'uniques-from-two-directories
;;;    should always produce the same results (possibly modulo how they handle error conditions like bad directories).

;;; The reasons coroutines are more complicated: Each coroutine (fringe1) performs a depth-first search
;;;  of a given directory. However, the resumer (the function that is the customer of the coroutine) needs to be
;;;  able to affect the traversal -- some subdirectories need to be skipped for efficiency reasons since descending
;;;  into them is pointless. Furthermore, the need to keep the two coroutines synchronized means a lot more
;;;  futzing around is necessary.

(in-package :cl-user)

#+DEBUG (defparameter *indent* 0)

(defun last-namestring (path)
  "Returns the last named thing in path"
  (if (directory-pathname-p path)
      (car (last (pathname-directory path)))
      (file-namestring path)))

(defun safer-file-data-size (path)
  "File-data-size will return nil on some native namestrings that contain characters like backslash (\) in
   their names. It will throw errors on namestrings that contain wildcards (*) in their names.
   This seems to solve the problem."
  (or (file-data-size (ccl::native-to-namestring
                       (typecase path
                         (string path)
                         (pathname (namestring path)))))
      0)) ; can still return nil if there's a permissions problem. Punt for now. We should report these eventually.

;;; Remember, arguments to sg-resume become returned-values from yield.
;;;  In the context of this coroutine, that's just the skip? parameter, which tells the coroutine
;;;  to NOT recurse into the PREVIOUS object it yielded. (If that object was a directory, this is meaningful. If it was
;;;  just a file, it's ignored.)
(defun fringe1 (tree exhausted) ; Ignores Unix dot files and symlinks.
  (labels ((fringe (path &optional last-in-subdirectory-p) ;; This should never yield nil. If given nil, it should immediately yield exhausted.
             (cond ((consp path)
                    (fringe (car path) (not (cdr path))) ; inform recursive call if we're at the end of this subdirectory
                    (when (cdr path) (fringe (cdr path))))
                   ((and path (directory-pathname-p path))
                    (let ((skip? (yield path last-in-subdirectory-p))) ; report the presence of the directory itself before we dig into it
                      ;; skip? tells us whether the resumer wants us to skip recursing into this directory
                      ;; (I'm a big fan of cursing. Recursing is even better.)
                      (unless skip?
                        (setf path (merge-pathnames
                                    (make-pathname :directory (pathname-directory path))
                                    ;; Call #'directory for a single level only.
                                    "*"))
                        (let ((direct-objects (directory path
                                                         :files t
                                                         :directories t
                                                         :all nil
                                                         :test (complement #'ccl::path-is-link) ; ignore symlinks
                                                         :directory-pathnames t
                                                         :follow-links nil)))
                          (cond (direct-objects
                                 (setf direct-objects (sort direct-objects #'string-lessp :key 'last-namestring))
                                 (fringe direct-objects))
                                (t (yield :empty t) ; if the subdirectory is empty
                                   ))))))
                   (path ; just a non-nil file. Since we know it's a file, the skip? returned value is meaningless.
                    (yield path last-in-subdirectory-p)))))
    (fringe tree)
    (yield exhausted) ; again, skip? returned value is meaningless here.
    ))
  
#|
(setf sg1 (make-sg "sg1"))
(sg-preset sg1 #'fringe1 f3 (setf done (cons nil nil)))
(sg-resume sg1)
(sg-resume sg1 t)
|#
(defun make-subpath-equal (nparents1 nparents2)
  "Return a function that compares pathnames below the parent node counts specified by nparents1 and nparents2.
  In other words, for comparison purposes, ignore the first nparents1 elements in path1 and the first
  nparents2 elements in path2."
  (lambda (path1 path2)
    (if (and (eq :empty path1)
             (eq :empty path2)) ; return true if both are empty; otherwise check them carefully
        (values t :bothempty)
        (and (pathnamep path1)
             (pathnamep path2)
             (let ((name1 (file-namestring path1))
                   (name2 (file-namestring path2))
                   (d1 (nthcdr nparents1 (pathname-directory path1))) ; last-namestring won't work here! Must take into account entire path below given parent
                   (d2 (nthcdr nparents2 (pathname-directory path2))))
               (cond ((and (directory-pathname-p path1)
                           (directory-pathname-p path2))
                      (values (equal d1 d2)  ; ignore file-namestrings & sizes if both paths are directories. (file-namestrings will be null strings in that case)
                              :directories))
                     
                     ((or (directory-pathname-p path1)
                          (directory-pathname-p path2)) ; only one is a directory
                      (values nil :mixed)) ; they don't match by definition
                     
                     (t ; we know they're both non-directories i.e. files
                      (values
                       (and (equal name1 name2) ; filenames are equal
                            (equal d1 d2)       ; and subpaths to each are equal
                            (eql (safer-file-data-size path1) (safer-file-data-size path2)))
                       :files))))))))


#|
Discussion of the logic herein.
NOTE THESE COMMENTS ARE OLD AND MIGHT NOT REFLECT CURRENT REALITY. THE directory-structure-compare-logic spreadsheet IS
THE DEFINITIVE REFERENCE.

DEFINITIONS:
object:       A file or directory.
pathname:     A fully-qualified path specification for an object in a file system.
lastname:     The most-specific part of a pathname. For files, this is the #'file-namestring of the path.
              For directories, it's #'last-namestring of the path (function defined herein).
file:         A pathname that references a file object. Always a leaf of a subtree. (We're ignoring symbolic links herein).
directory:    A pathname that references a container (not a file) that contains files and directories.
              Usually an interior node of a subtree, but a directory can be a leaf if it's empty.
subdirectory: A particular directory and its direct contents.
subtree:      A particular directory and its direct and indirect contents.
indirect:     What Unix calls the 'recursive' contents of a directory. We'd prefer to avoid the word 'recursive' in that context.
unique:       An object that appears at a given location in a subtree but not in the same location in another, similar subtree.
coroutine:    A generator of objects. A coroutine calls #'yield to report a result to the resumer.
resumer:      The main program, which initiates the coroutine, accepts result from it, and controls its behavior during its operation.
              'Resumer' is an odd word, but it's used in the coroutine literature to refer to more general ideas than just main
              programs (it could in general be another coroutine) but here the resumer is just the main program.
              The resumer calls #'sg-resume to tell the coroutine it wants another object.

First notice that we're traversing two subtrees, looking only for UNIQUE objects.
That is, files and directories at a given place in subtree1 that do not appear in subtree2,
or vice-versa.

Next, note that when we record a UNIQUE directory, we will NEVER bother to descend into it looking for more objects.
The fact that a given directory is UNIQUE is all we need report. The fact that objects contained within a unique
directory are also unique is certainly always true, but it's superfluous, so we won't bother to report it. Avoiding such
reportage reduces both clutter in the output report and the CPU time needed to traverse those directories.

Now to brass tacks.
We've structured this code with a resumer (main program) and two coroutines, which we're going to call sg1 and sg2.
(SG is short for 'stack-group' which is how coroutines were implemented in MCL a long time ago.)
sg1 and sg2 generate paths from two subtrees in depth-first order, subtree1 and subtree2 respectively.

A coroutine calls #'yield to report two values to the resumer: A path, and a flag that indicates if this path is the last
one (in alphabetic order) in the current subdirectory.

Conversely, the resumer calls #'sg-resume to tell the coroutine to produce the next path. But this call _itself_ takes
a flag called skip? which tells the coroutine whether it should descend into the last path it produced, or not. (These two
cases are represented by skip? = nil and skip? = t, respectively.) 'Descending into the last path' is only a meaningful
concept if the last path is a directory; if it's a file, the skip? flag is meaningless and the coroutine ignores it.
Thus the skip? parameter informs the coroutine about
how the resumer wants it to proceed. This is how the coroutines herein differ from 'dumb' pathname generators; their
behavior can be modified on the fly by the resumer.

In the diagrams below, the letters 'A', 'B', 'C' etc. refer to lastnames of objects. Lexicographic ordering
is important since we are sorting these.*
So the letter names are significant here in that 'A' can be assumed to be a lastname that occurs earlier in sorted order
than every other letter; 'B' is a name that sorts after 'A' but before 'C' etc.

*A given lisp implementation's #'directory function might return objects 
already sorted, but we don't depend on that. We sort them ourselves.

Pathnames will be produced by the coroutines _temporally_
in lexicographic sorted order. This is very important, because it allows us to make some simplifying assumptions about
whether objects are unique or not.

For example, if we initialize sg1 to subtree1 and sg2 to subtree2, then they will (presumably) traverse the two subtrees
in lockstep with one another.
If sg1 produces 'A' when sg2 produces 'A', we know immediately that this file named 'A' is not unique and should not be
reported (unless the sizes of the two files differ, in which case we do report both as unique). We also know that --
if 'A' represents a directory in both coroutines -- that we want both coroutines to descend into them both. (It's a given
herein that any time we report a directory as unique, we will NOT want the coroutine that produced it to descend into it,
but merely to produce the next object within the same subdirectory in which we found that directory.
Conversely, any directory that is NOT unique SHOULD be descended into by both coroutines, so we can search for unique
objects inside.)

But that 'lockstep' comment above is perilous. If the two subtrees differ in some way -- and we suspect that they DO
differ else this whole exercise is pointless -- what could 'lockstep' even mean?

It means that we make an effort to keep the two coroutines synchronized with each other as best we can. When they
fail to synchronize, that's EXACTLY the situation we're looking for, and we report it. After reporting an out-of-sync
(or uniqueness) condition, we attempt to re-synch the coroutines. Here's how:

CASE 0:
     sg1  sg2
     ---  ---
p1 -> A   A <- p2
      B   C 
      C   D

This diagram means that sg1 is currently traversing some given subdirectory, and it just produced A. p1 is the last
path produced by sg1, and it's equal to A. Likewise, sg2 is traversing a comparable subdirectory, and having produced an object
also named A, p2 is currently equal to that second A. (Note that the two As will always be different full pathnames, because they
started from different subtrees. But the lastnames of the two As will be the same.)

What do we do in this case? Not much. Since A appears in both subdirectories (and presumably both of these subdirectories
are at comparable places within their respective subtrees), they're not unique. But there is a subtlety: If both As are
files (not directories) and their sizes (or hashes) differ, then we'll treat them as if they are unique and we'll report
them both, along with their respective sizes or hashes for convenience.

If both As are directories, we don't care about their sizes or hashes since these concepts are ambiguous for directories,
so we'll treat them both as non-unique and we won't report them. We'll instruct both coroutines to descend into both in
a depth-first traversal.

What if one A is a directory and the other is a file? Then we'll treat them both as unique and record both. However, in this
case we will instruct the coroutine that produced the directory NOT to descend into that directory, but rather to skip descending
into it. We already know that directory is unique; no further information is to be gleaned by diving into it.

In all the subcases above, we instruct both coroutines to continue normally after we've handled the recording as necessary.
Neither generator needs to be paused, because we haven't really lost sync.

Now we need to look at the cases where we truly lose sync.


CASE 1:
     sg1  sg2
     ---  ---
      A   A
p1 -> B   C <- p2
      C   D

Oops. Now we're truly out of sync. sg1 produced B while sg2 produced C, because file B doesn't exist in sg2's subdirectory.
What do we do? Well, we have to decide which of p1 or p2 is 'unique'. They're not both unique! Or at least we can't know
they both are yet. Since both coroutines are producing files in lastname-lexicographic order, the earlier one in the mismatch
between p1 and p2 is the one we know to be unique.
If B was going to appear in the second list, it would have done so by now. Since it didn't, we
know it's not going to appear there, ever. So B is unique. But C might not be unique. (Looking at the last row above,
we know that in fact C is not unique, but our program doesn't have the luxury of foretelling the future so it cannot
see that last row just yet.) So our program will record B as a unique path, but it will not record C. Furthermore,
it will pause sg2 and resume only sg1 during the next loop iteration, in hopes that sg1 will produce a C some time
in the future.

One more wrinkle: If B is a directory, we will instruct sg1 to skip descending into it. From now on, we'll assume
that every time we say 'record X' we also imply 'and skip descending into X if X is a directory.'

Where this gets dicey is when we reach the end of a subdirectory.

CASE 2
      sg1  sg2
     ---  ---
      A   A
p1 -> B   C <- p2
          D
 
Here, B is the last object in sg1's current subdirectory. Here's what we must do:
      Record p1
      Record p2
      Exhaust sg2's current subdirectory
      Continue both coroutines

We haven't talked about exhausting a subdirectory yet. Exhausting a subdirectory means to
repeatedly produce values from a coroutine until the coroutine reports last-in-subdirectory.
Every object produced must be recorded (which again implies that that object is not descended into
if it's a directory).

Exhaustion here means that we record C, then produce D and record it too. We know that C and D are
unique because they occur lexicographically after B and there are no more
entries in the subdirectory where B occurred. We also know that sg2 cannot possibly produce B
within this same subdirectory later because its output is lexicographically ordered. (We also know
that no subdirectory can have two items named B because file systems don't allow two objects
with the same name in the same subdirectory.*)

*In Linux, our name matching and lexicographic ordering will be case-sensitive.

CASE 3
      sg1  sg2
     ---  ---
      A    A
p1 -> B    C <- p2
      C   

Action: Record p1.
        Suspend sg2 for the next iteration.

   This takes us here:
       sg1  sg2
     ---  ---
      A    A
      B    C <- p2
p1 -> C

This is basically just Case 0 again. The fact that we're at the end of each subdirectory is immaterial;
we won't record anything and we'll descend into both Cs (if they're directories) and we'll continue both
coroutines.

If both Cs above are files rather than directories, what happens when we call #'sg-resume on them? They
will continue to the next subdirectory in depth-first order. The coroutines 'know' what the depth-first
order of the file system is, and if there's another subdirectory, they'll go to it. If not, they'll
report 'done'.


If p1 or p2 is "unique" it only means that its lastname comes before the other one lexicographically.
[should probably rename this "prior" or something]

If neither p1 nor p2 is unique, it means their lastnames match. In which case they might still need
to be recorded in two cases:
1. They're both files and their file sizes differ, in which case we must record both, with their respective file sizes.
2. One is a directory and the other is a file, in which case we must record both.

Suspending and exhausting are operations that are solely for purposes of re-syncing, and re-syncing
is solely caused by lexicographic mismatches. If p1 and p2 have matching names, we never suspend or
exhaust.

Exhausting never happens if we're *already* exhausted in that subdirectory. In other words, if we're
already at the last-in-subdirectory object.

|#

(defconstant p1ubit  #b1000)
(defconstant p2ubit  #b0100)
(defconstant lis1bit #b0010)
(defconstant lis2bit #b0001)

(defmacro setbit (stateflags id)
  (let* ((bitname (intern (concatenate 'string (symbol-name id) "BIT") (symbol-package id))))
    `(when ,id (setf ,stateflags (logior ,stateflags ,bitname)))))

(defun pack-state (p1u p2u lis1 lis2)
  "p1u : p1 is unique
  p2u : p2 is unique
  lis1 : last-in-subdirectory-1
  lis2 :last-in-subdirectory-2"
  (let ((stateflags 0))
    (setbit stateflags p1u)
    (setbit stateflags p2u)
    (setbit stateflags lis1)
    (setbit stateflags lis2)
    stateflags))

(defun xor (x y)
  (or (and x (not y))
      (and y (not x))))

(defun directory-structure-compare (subtree1 subtree2)
  "Use this function when you care about structural similarity below two given directories.
  Returns two vectors: Items that are unique below path root subtree1 and those that are unique 
  below path root subtree2.
  (The number of individual comparisons performed is also returned but this is mostly for debugging).
  A 'Unique' item is one that exists at a given point below one root but not the other.
  Items that are not 'unique' (and thus not returned) include: 
  --Directories with the same name, at same position in the subtree.
  --Files with the       same name, at same position in the subtree, and of same size.
  Files that differ only in size are returned as a cons of (path . size-in-bytes), and are
  always returned in both vectors with similar paths but different size-in-bytes.
  ('similar path' means that their paths are identical only leafward from the subtree roots specified
  by subtree1 and subtree2).
  Items that appear BELOW unique directories are NOT reported, because they would add needless redundant
  clutter to the results and waste CPU time."
  (let ((compare-fn (make-subpath-equal (length (pathname-directory subtree1))
                                        (length (pathname-directory subtree2))))
        (unique1 (make-array 10 :adjustable t :fill-pointer 0))  ; unique things in first directory
        (unique2 (make-array 10 :adjustable t :fill-pointer 0))) ; unique things in second directory
    
    (let* ((sg1 (make-sg "sg1"))
           (sg2 (make-sg "sg2"))
           (done (cons nil nil))
           (next-sg1 t)
           (next-sg2 t)
           (skip-dive-sg1 nil)
           (skip-dive-sg2 nil)
           (p1 nil)
           (p2 nil)
           (numcomparisons 0) ; number of comparisons done
           (state nil)
           ;; State flags:
           (p1u nil) ; p1 unique
           (p2u nil) ; p2 unique
           (last-in-subdirectory-1 nil)
           (last-in-subdirectory-2 nil)
           )
      (macrolet ((%record (&rest args)
                   `(vector-push-extend ,@args)))
        (labels ((record (which &optional size-also)
                   "Record a unique entity, and instruct its coroutine not to dive into it on the next iteration.
           Don't record the special entity :empty. It's a flag for code elsewhere."
                   (ecase which
                     (1 (unless (eq :empty p1)
                          (if size-also
                              (%record (cons p1 (safer-file-data-size p1)) unique1)
                              (%record p1 unique1))
                          (setf skip-dive-sg1 t)))
                     (2 (unless (eq :empty p2)
                          (if size-also
                              (%record (cons p2 (safer-file-data-size p2)) unique2)
                              (%record p2 unique2))
                          (setf skip-dive-sg2 t)))))
                 (suspend (which)
                   (ecase which
                     (1 (setf next-sg1 nil))
                     (2 (setf next-sg2 nil))))
                 (next (which)
                   (ecase which
                     (1 (multiple-value-prog1 (sg-resume sg1 skip-dive-sg1)
                          (setf skip-dive-sg1 nil)))
                     (2 (multiple-value-prog1 (sg-resume sg2 skip-dive-sg2)
                          (setf skip-dive-sg2 nil)))))
                 (%exhaust-subdirectory (sg result-vector &optional (recursive-call? nil))
                   "Call sg repeatedly, collecting every result onto result-vector until last-in-subdirectory is true.
           Don't dive into any subdirectories found."
                   (declare (ignorable recursive-call?))
                   #+DEBUG (%record (format nil "~:[~;  ~]Calling %exhaust-subdirectory ~S" recursive-call? sg) result-vector) ; indent a bit if recursive call
                   (multiple-value-bind (path last-in-subdirectory) (sg-resume sg t)
                     (%record path result-vector)
                     (unless last-in-subdirectory
                       (%exhaust-subdirectory sg result-vector t))))
                 (exhaust-subdirectory (which)
                   (ecase which
                     (1 (%exhaust-subdirectory sg1 unique1)
                        (setf skip-dive-sg1 t))
                     (2 (%exhaust-subdirectory sg2 unique2)
                        (setf skip-dive-sg2 t)))))
          
          
          (sg-preset sg1 #'fringe1 subtree1 done)
          (sg-preset sg2 #'fringe1 subtree2 done)
          (let (match more-info)
            (loop
              (when next-sg1 (multiple-value-setq (p1 last-in-subdirectory-1) (next 1)))
              (when next-sg2 (multiple-value-setq (p2 last-in-subdirectory-2) (next 2)))
              #+DEBUG (format t "~%~v@Tp1=~S~:[ (same as last time)~;~] ~:[~;(last)~]" *indent* p1 next-sg1 last-in-subdirectory-1)
              #+DEBUG (format t "~%~v@Tp2=~S~:[ (same as last time)~;~] ~:[~;(last)~]~%" *indent* p2 next-sg2 last-in-subdirectory-2)
              
              (setf next-sg1 t next-sg2 t)
              
              ;; deal with the case where one or more coroutines is done
              (when (or (eq p1 done)
                        (eq p2 done))
                (unless (eq p2 done)
                  (record 2)
                  (loop while (progn (setf p2 (next 2))
                                (not (eq p2 done))) do
                    (record 2)))
                (unless (eq p1 done)
                  (record 1)
                  (loop while (progn (setf p1 (next 1))
                                (not (eq p1 done))) do
                    (record 1)))
                (process-kill sg1)
                (process-kill sg2)
                (return (values unique1 unique2 numcomparisons)))
              
              ;; okay, nobody's done yet
              (incf numcomparisons)
              (multiple-value-setq (match more-info) (funcall compare-fn p1 p2))
              #+DEBUG (format t "~v@T~:[MISMATCH~%~;~]" *indent* match)
              (cond (match ; match means we don't record the match.
                     ;;; At this point, either both of p1 and p2 are files, or they're both directories. We wouldn't be here if that wasn't the case.
                     ;;; Now we have to do some special processing iff exactly one of them is the last in its subdirectory.
                     ;;; (If neither is last, nothing special needs to be done, as the ordinary operation of this loop will handle it.
                     ;;;  Less obviously, if both are last, the ordinary operation of this loop will also handle it.)
                     
                     (when (xor last-in-subdirectory-1 last-in-subdirectory-2)
                       
                       ;; Several cases here:
                       ;; 1) Both are files with same name and same size. Don't record. Just exhaust the other subdirectory.
                       (cond ((eql :files more-info)
                              (if last-in-subdirectory-1
                                  (exhaust-subdirectory 2)
                                  (exhaust-subdirectory 1)))
                             ;; 2) Both are empty directories. Same drill.
                             ((eql :bothempty more-info)
                              (if last-in-subdirectory-1
                                  (exhaust-subdirectory 2)
                                  (exhaust-subdirectory 1)))
                             ;; 3) Both are non-empty directories. This is the fun case. (If exactly one was empty, it would be a non-match and we wouldn't be here.)
                             (t
                              (let (#+DEBUG (*indent* (+ *indent* 2))
                                    )
                                (multiple-value-bind (subunique1 subunique2)
                                                     (directory-structure-compare p1 p2)
                                  (map nil (lambda (item) (%record item unique1)) subunique1)
                                  (map nil (lambda (item) (%record item unique2)) subunique2)))
                              ; Don't dive into either, because we just did so with the recursive call
                              (setf skip-dive-sg1 t
                                    skip-dive-sg2 t)
                              (if last-in-subdirectory-1
                                  (exhaust-subdirectory 2)
                                  (exhaust-subdirectory 1))))))
                    
                    (t ; mismatch
                     ;; mismatch. See directory-structure-compare-logic spreadsheet.
                     ;; unique one is first lexicographically since we're sorting results from #'directory
                     (setf p1u nil
                           p2u nil)
                     
                     (if (or (eq :empty p1) ; if both were empty, it would have been called a match and we wouldn't be here
                             (eq :empty p2))
                         (setf p1u (eq :empty p1) ; call whichever one is empty the unique one
                               p2u (eq :empty p2))
                         (let ((p1name (last-namestring p1)) ; if neither is empty, check lexicographic order to determine uniqueness
                               (p2name (last-namestring p2)))
                           ; only one of these can be true at a time. They can both be false (when the names are equal), but not both true.
                           (setf p1u (string-lessp p1name p2name))
                           (setf p2u (string-lessp p2name p1name))))
                     
                     (setf state (pack-state p1u p2u last-in-subdirectory-1 last-in-subdirectory-2))
                     
                     (ecase state
                       ((#b0000 #b0001 #b0010 #b0011)
                        (if (and (eql :files more-info)
                                 (equal (file-namestring p1)
                                        (file-namestring p2)))
                            ;; we know their sizes don't match, so they're both unique. Increment both coroutines.
                            (progn
                              (record 1 t)
                              (record 2 t))
                            (progn
                              (record 1)
                              (record 2)))
                        (case state ; additional work if either is last
                          (#b0001
                           (exhaust-subdirectory 1))
                          (#b0010
                           (exhaust-subdirectory 2))))
                       
                       (#b0100 (suspend 1)
                               (record 2))
                       
                       (#b0101 (record 1)
                               (record 2)
                               (exhaust-subdirectory 1))
                       
                       (#b0110 (suspend 1)
                               (record 2))
                       
                       (#b0111 (record 1)
                               (record 2))
                       
                       (#b1000 (suspend 2)
                               (record 1))
                       
                       (#b1001 (suspend 2)
                               (record 1))
                       
                       (#b1010 (record 1)
                               (record 2)
                               (exhaust-subdirectory 2))
                       
                       (#b1011 (record 1)
                               (record 2))
                       
                       ; Cases with 11xx cannot exist. It's not possible for both p1 and p2 to be unique at the same time.
                       ))))))))))

#|
(setf dir1 #P"/<pathto>/repo1/") ; very important that these end with a slash
(setf dir2 #P"/<pathto>/repo2/")
(directory-structure-compare dir1 dir2)
|#
