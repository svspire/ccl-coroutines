;;; ccl-coroutines-basic.lisp
;;; Wednesday March 9, 2016
;;; Maintained by Shannon Spires <svs@bearlanding.com>
;;; Based on code by Gary Byers <gb@clozure.com>

;;; Basic coroutines for Clozure Common Lisp (OpenMCL) using fake stack-groups.
;;; Tested in CCL 1.11 and later
;;; Limitations of this version:
;;;   Yield can only return single values.
;;;   Funcalling a stack-group [coroutine] cannot accept parameters.
;;;   Probably too verbose. The name "stack-group" everywhere is cumbersome.
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
when the resumer sees the returned value is dependent on the granularity of the operating
system's scheduler. That's not usually horrible, but it's not as fast as a native
stack-group would be.

The returned value is stored in a slot of the producer stack-group process. It's atomically
protected such that the resumer won't see it half-written by the producer, but this
code doesn't support having more than one resumer for a given producer, or the return
of multiple values from a stack-group (although multiple values could probably be supported 
fairly easily).

It's not possible in this code for the resumer to call the stack-group with a parameter.
Many coroutine algorithms require this. It's probably not too difficult to add. 
|#

#| Comments by gb:
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
to be cleaned up. There may be more work to do here.
|#

(in-package :ccl)

(export '(stack-group-return
          make-stack-group
          funcall-stack-group
          stack-group-preset))

(defclass stack-group (process)
  ((%resumer :initform (vector nil))
   (value :initform nil)
   (run :initform (make-semaphore) :reader stack-group-semaphore)))

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

(defmethod stack-group-semaphore ((p process))
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
  (wait-on-semaphore (stack-group-semaphore *current-process*)))

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
  (signal-semaphore (stack-group-semaphore sg))
  (wait-to-run)
  (stack-group-value sg))

(defun %stack-group-return (value)
  (let* ((self *current-process*)
         (resumer (stack-group-resumer self)))
    (setf (stack-group-value self) value)
    (signal-semaphore (stack-group-semaphore resumer))))

(defun stack-group-return (value)
  "Returns value to resumer. Returned value from THIS function is always T."
  (%stack-group-return value)
  (wait-to-run))

(in-package :cl-user)

(defmacro yield (value) ; similar name defined in ccl but fortunately not exported.
  "Conventional coroutine terminology."
  `(stack-group-return ,value))

; Example 1. Samefringe.
#+EXAMPLE
(progn
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
  )

; Example 2. Create a generator by using mapc.
#+EXAMPLE
(progn
  (defun gen (items)
    (mapc (lambda (item) (yield item)) items)
    nil ; force last value to be nil
    )
  
  (defun make-generator (sg items)
    (stack-group-preset sg #'gen items)
    (lambda ()
      (funcall-stack-group sg)))
  )

; (setf g (make-generator (make-stack-group "bar") '(a b c d e f)))
; (funcall g) -> A
; (funcall g) -> B
; etc.
; Pay attention to when it returns nil. That means it's done. If you call it
;   after that, it'll throw a stack-group-exhausted error.

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
    (let ((sg-dir (make-stack-group "dir")))
      (stack-group-preset sg-dir #'my-directory dirpath)
      #'(lambda ()
          (funcall-stack-group sg-dir))))
  )

;(setf g (directory-generator "ccl:*"))
;(funcall g)
;(funcall g)
; etc.
; Pay attention to when it returns nil. That means it's done. If you call it
;   after that, it'll throw a stack-group-exhausted error.
