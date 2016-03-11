;;; ccl-coroutines-mv.lisp
;;; 10-Mar-2016
;;; Improves on ccl-coroutines-sg.lisp by returning multiple values.

;;; Maintained by Shannon Spires <svs@bearlanding.com>
;;; Based on code by Gary Byers <gb@clozure.com>

;;; Basic coroutines for Clozure Common Lisp (OpenMCL) using fake stack-groups.
;;; Tested in CCL 1.11 and later
;;; Limitations of this version:
;;;   Funcalling a stack-group [coroutine] cannot accept parameters.
;;; License: MIT

#| Comments by SS:
This creates fake stack-groups in CCL to enable coroutines.
Each pseudo-stack-group is actually a native OS thread augmented by a semaphore.
(Early versions of this code used locks, but semaphores are handled by the OS 
in every implementation of CCL, while locks might not be. Trust me, you want the
OS scheduler to handle waiting, not userland Lisp code.)

Provisions are made herein to enable ordinary processes to have such semaphores
so that they can be the resumer of a specialized stack-group process. Thus there's
no need for the current process to itself be of the stack-group class just to allow
it to call a stack-group coroutine.

Advantages of this approach:
It makes it very easy to write coroutines in CCL without changing existing code or
using special continuation macros. Any function that accepts a function (e.g. the
Common Lisp mapping functions or the CCL directory function) can be instantly turned
into a generator or coroutine.

Disadvantages:
It's not particularly fast. The latency between when a coroutine is ready to return vs.
when the resumer sees the returned value(s) is/are dependent on the granularity of the operating
system's scheduler. That's not usually horrible, but it's not as fast as a native
stack-group would be.

The returned value(s) is/are stored in a slot of the producer stack-group process. It's atomically
protected such that the resumer won't see it half-written by the producer, but this
code doesn't support having more than one resumer for a given producer.

It's not possible in this code for the resumer to call the stack-group with a parameter.
Many coroutine algorithms require this. It's probably not too difficult to add. 
|#

(in-package :ccl)

(export '(sg-return
          make-sg
          funcall-sg
          sg-preset))

(defclass sg (process)
  ((%resumer :initform (vector nil))
   (value :initform nil)
   (run :initform (make-semaphore) :reader sg-semaphore)))

(define-condition sg-exhausted (error)
  ((sg :initarg :sg)
   (calling-thread :initarg :calling-thread))
  (:report (lambda (condition stream)
             (with-slots (sg calling-thread) condition
               (format stream "Attempt to funcall exhausted stack group ~s by ~s." sg calling-thread)))))

(defmethod sg-resumer ((sg sg))
  (with-slot-values (%resumer) sg
    (svref %resumer 0)))

(defmethod (setf sg-resumer) (new (sg sg))
  (with-slot-values (%resumer) sg
    (if  (and (or (null new)
                  (eq new *current-process*))
              (if new
                (conditional-store (svref %resumer 0) nil new)
                (conditional-store (svref %resumer 0) *current-process* nil)))
      new
      (error "Can't set ~s of ~s to ~s."
             'sg-resumer sg new))))

(defmethod sg-semaphore ((p process))
  (or (getf (process-plist p) :run)
      (setf (getf (process-plist p) :run) (make-semaphore))))

;; Only the resumer can access the stack group's return value.
;;; This clears the resumer after getting the values; that's enforced
;;; by the call to (SETF SG-RESUMER)
(defmethod sg-value ((sg sg))
  (multiple-value-prog1 (values-list (slot-value sg 'value))
    (setf (sg-resumer sg) nil)))

(defmethod (setf sg-value) (new (sg sg))
  (if (eq *current-process* sg)
    (setf (slot-value sg 'value) new)
    (error "~s can't set sg value of ~s." *current-process* sg)))

(defun make-sg (name)
  (make-process name :class 'sg))

(defun wait-to-run ()
  (wait-on-semaphore (sg-semaphore *current-process*)))

(defmethod sg-preset ((sg sg) function &rest args)
  (process-preset sg (lambda ()
                           (wait-to-run)
                           (multiple-value-call '%sg-return (apply function args))))
  (process-enable sg))

; could call this sg-resume
; can we enhance this to allow args to be passed?
(defmethod funcall-sg ((sg sg))
  (when (process-exhausted-p sg)
    (error 'sg-exhausted :sg sg :calling-thread *current-process*))
  (setf (sg-resumer sg) *current-process*)
  (signal-semaphore (sg-semaphore sg))
  (wait-to-run)
  (sg-value sg))

(defun %sg-return (&rest values)
  (let* ((self *current-process*)
         (resumer (sg-resumer self)))
    (setf (sg-value self) values)
    (signal-semaphore (sg-semaphore resumer))))

(defun sg-return (&rest values)
  "Returns value to resumer. Returned value from THIS function is always T."
  (apply #'%sg-return values)
  (wait-to-run))

(in-package :cl-user)

(defmacro yield (&rest values) ; similar name defined in ccl but fortunately not exported.
  "Conventional coroutine terminology."
  `(sg-return ,@values))

; Example 1. Samefringe.
#+EXAMPLE
(progn
  (defun fringe1 (tree exhausted)
    (labels ((fringe (tree)
               (if (atom tree)
                   (sg-return tree)
                   (progn
                     (fringe (car tree))
                     (when (cdr tree) (fringe (cdr tree)))))))
      (fringe tree)
      exhausted))
  
  (defun samefringe (tree1 tree2)
    (let* ((sg1 (make-sg "sg1"))
           (sg2 (make-sg "sg2"))
           (done (cons nil nil)))
      (sg-preset sg1 #'fringe1 tree1 done)
      (sg-preset sg2 #'fringe1 tree2 done)
      (loop
        (let* ((v1 (funcall-sg sg1))
               (v2 (funcall-sg sg2)))
          (cond ((not (eq v1 v2)) (return nil))
                ((eq v1 done) (return t)))))))
  
  (samefringe '(a b c) '(a (b c)))
  )

; Example 2. Create a generator by using mapc.
#+EXAMPLE
(progn
  (defun gen (items)
    (mapc (lambda (item) (yield item)) items)
    nil ; force last value to be nil
    )
  
  (defun make-generator (sg items)
    (sg-preset sg #'gen items)
    (lambda ()
      (funcall-sg sg)))
  )

; (setf g (make-generator (make-sg "bar") '(a b c d e f)))
; (funcall g) -> A
; (funcall g) -> B
; etc.
; Pay attention to when it returns nil. That means it's done. If you call it
;   after that, it'll throw a sg-exhausted error.

; Example 3. A quick-and-dirty directory generator. 
#+EXAMPLE
(progn
  (defun my-directory (dirpath)
    (directory dirpath :test #'(lambda (item) (yield item) nil))
    ; nil inside lambda above prevents #'directory from returning the found file as
    ;  one of its results. In fact, we want #'directory to return nil itself as a signal
    ;  that the generator is finished.
    )
  
  (defun directory-generator (dirpath)
    (let ((sg-dir (make-sg "dir")))
      (sg-preset sg-dir #'my-directory dirpath)
      #'(lambda ()
          (funcall-sg sg-dir))))
  )

; Example 4. A quick-and-dirty directory generator that returns multiple values.
;   Each returned entry consists of two values:
;    the pathname (as in example 3) and the file size.
#+EXAMPLE
(progn
  (defun my-directory (dirpath)
    (directory dirpath :test #'(lambda (item)
                                 (yield item (file-data-size item)) nil))
    ; nil inside lambda above prevents #'directory from returning the found file as
    ;  one of its results. In fact, we want #'directory to return nil itself as a signal
    ;  that the generator is finished.
    )
  
  (defun directory-generator (dirpath)
    (let ((sg-dir (make-sg "dir")))
      (sg-preset sg-dir #'my-directory dirpath)
      #'(lambda ()
          (funcall-sg sg-dir))))
  )

;(setf g (directory-generator "ccl:*"))
;(funcall g)
;(funcall g)
; etc.
; Pay attention to when it returns nil. That means it's done. If you call it
;   after that, it'll throw a sg-exhausted error.
