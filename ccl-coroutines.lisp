;;; ccl-coroutines.lisp
;;; Wednesday March 9, 2016
;;; Maintained by Shannon Spires <svs@bearlanding.com>
;;; Based on code by Gary Byers <gb@clozure.com>

;;; Coroutines for Clozure Common Lisp (OpenMCL)
;;; Tested in CCL 1.11 and later

;;; License: MIT

#|
FUNCALL-STACK-GROUP makes the calling thread be the resumer of the target stack group,
then waits until the target returns a value via STACK-GROUP-RETURN. I don't see how
it makes sense for a stack group to have more than one resumer at any time, so the
current version tries to enforce that. When the resumer picks up the value returned
by STACK-GROUP-RETURN, it clears the stack group's resumer so that it can be funcalled
by another thread. (A stack group's value can only be set by itself and can only be
read by the resumer.)

In the Symbolics API, the fundamental primitive for switching between stack groups
(and transferring values between them) was STACK-GROUP-RESUME, which took a stack
group and a value as arguments. (FUNCALL set the resumer and used STACK-GROUP-RESUME
to ... resume it; STACK-GROUP-RETURN used STACK-GROUP-RESUME to resume the resumer.
STACK-GROUP-RESUME didn't itself change the resumer. AFAIK, stack groups can FUNCALL
each other via FUNCALL-STACK-GROUP and obtain values returned by STACK-GROUP-RETURN in
this implementation; the only argument that I can think of for STACK-GROUP-RESUME is
that it could allow mutual recursion (A calls B which calls A, etc.), and I can think
of lots of fairly good reasons for not supporting it.

If a stack group dies prematurely then some state (in it and the resumer) would need
to be cleaned up. I haven't done anything about this (or even thought about it in any
detail.)
|#

(in-package :ccl)

(defclass stack-group (process)
  ((%resumer :initform (vector nil))
   (value :initform nil)
   (run :initform (make-semaphore) :reader stack-group-runsem)))

(define-condition stack-group-exhausted (error)
  ((stack-group :initarg :stack-group)
   (calling-thread :initarg :calling-thread))
  (:report (lambda (condition stream)
             (with-slots (stack-group calling-thread) condition
               (format stream "Attempt to funcall exhausted stack group ~s by ~s." stack-group calling-thread)))))

(defmethod stack-group-resumer ((sg stack-group))
  (with-slot-values (%resumer) sg
    (svref %resumer 0)))

(defmethod (setf stack-group-resumer) (new (sg stack-group))
  (with-slot-values (%resumer) sg
    (if  (and (or (null new)
                  (eq new *current-process*))
              (if new
                (conditional-store (svref %resumer 0) nil new)
                (conditional-store (svref %resumer 0) *current-process* nil)))
      new
      (error "Can't set ~s of ~s to ~s."
             'stack-group-resumer sg new))))

(defmethod stack-group-runsem ((p process))
  (or (getf (process-plist p) :run)
      (setf (getf (process-plist p) :run) (make-semaphore))))

;; Only the resumer can access the stack group's return value.
;;; This clears the resumer after getting the value; that's enforced
;;; by the call to (SETF STACK-GROUP-RESUMER)
(defmethod stack-group-value ((sg stack-group))
  (prog1 (slot-value sg 'value)
    (setf (stack-group-resumer sg) nil)))

(defmethod (setf stack-group-value) (new (sg stack-group))
  (if (eq *current-process* sg)
    (setf (slot-value sg 'value) new)
    (error "~s can't set stack-group value of ~s." *current-process* sg)))

(defun make-stack-group (name)
  (make-process name :class 'stack-group))

(defun wait-to-run ()
  (wait-on-semaphore (stack-group-runsem *current-process*)))

(defmethod stack-group-preset ((sg stack-group) function &rest args)
  (process-preset sg (lambda ()
                           (wait-to-run)
                           (%stack-group-return (apply function args))))
  (process-enable sg))

; could call this stack-group-resume
; can we enhance this to allow args to be passed?
(defmethod funcall-stack-group ((sg stack-group))
  (when (process-exhausted-p sg)
    (error 'stack-group-exhausted :stack-group sg :calling-thread *current-process*))
  (setf (stack-group-resumer sg) *current-process*)
  (signal-semaphore (stack-group-runsem sg))
  (wait-to-run)
  (stack-group-value sg))

(defun %stack-group-return (value)
  (let* ((self *current-process*)
         (resumer (stack-group-resumer self)))
    (setf (stack-group-value self) value)
    (signal-semaphore (stack-group-runsem resumer))))

(defun stack-group-return (value)
  (%stack-group-return value)
  (wait-to-run))

(defmacro yield (value)
  "Conventional coroutine terminology"
  `(stack-group-return ,value))

#| Example 1. Samefringe.
(defun fringe1 (tree exhausted)
  (labels ((fringe (tree)
             (if (atom tree)
               (stack-group-return tree)
               (progn
                 (fringe (car tree))
                 (when (cdr tree) (fringe (cdr tree)))))))
    (fringe tree)
    exhausted))

(defun samefringe (tree1 tree2)
  (let* ((sg1 (make-stack-group "sg1"))
         (sg2 (make-stack-group "sg2"))
         (done (cons nil nil)))
    (stack-group-preset sg1 #'fringe1 tree1 done)
    (stack-group-preset sg2 #'fringe1 tree2 done)
    (loop
      (let* ((v1 (funcall-stack-group sg1))
             (v2 (funcall-stack-group sg2)))
        (cond ((not (eq v1 v2)) (return nil))
              ((eq v1 done) (return t)))))))

(samefringe '(a b c) '(a (b c)))

|#

#| Example 2. Convert mapcar into a generator.
(defun gen (items)
  (mapcar (lambda (item) (yield item)) items))

(defun make-generator (sg items)
  (stack-group-preset sg #'gen items)
  (lambda ()
    (funcall-stack-group sg)))

; (setf c (make-generator (make-stack-group "bar") '(a b c d e f)))
; (funcall c) -> A
; (funcall c) -> B
; etc.

|#

#| Example 3. A quick-and-dirty directory generator. 

(defun my-directory (dirpath)
  (directory dirpath :test #'(lambda (item) (yield item) nil)))

(defun directory-generator (dirpath)
  (let ((sg-dir (make-stack-group "dir")))
    (stack-group-preset sg-dir #'my-directory dirpath)
    #'(lambda ()
        (funcall-stack-group sg-dir))))

;(setf g (directory-generator "ccl:*"))
;(funcall g)
;(funcall g)
; etc.

|#