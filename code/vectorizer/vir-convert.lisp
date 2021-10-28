(in-package #:sb-simd-internals)

;;; This file contains the machinery for turning a loop body into a
;;; directed acyclic graph of VIR nodes.  This is essentially a variation
;;; of EVAL for a very limited subset of Common Lisp, except that we don't
;;; actually compute a result, but assemble data flow graph nodes.
;;;
;;; Some notes about the conversion process:
;;;
;;; - We represent the lexical environment as alists whose entries are of
;;;   the form (VARIABLE VIR-NODE DECLARED-TYPE)
;;;
;;; - We automatically merge common subexpressions.
;;;
;;; - The vectorizer assumes that calculations of array indices never
;;;   overflow.  This allows us to represent index calculations as symbolic
;;;   expressions of their incoming VIR nodes.
;;;
;;; - The vectorizer assumes that loads and stores never reference the same
;;;   memory locations.  (We try to emit warnings, though)

(defgeneric vir-convert (form lexenv))

(defgeneric vir-convert-special-form (operator rest lexenv))

(defgeneric vir-convert-function-form (function arguments lexenv))

(defgeneric vir-convert-funcall (function-record arguments lexenv))

(defmethod vir-convert (form lexenv)
  (if (atom form)
      (if (symbolp form)
          (let ((entry (assoc form lexenv)))
            (if (consp entry)
                (second entry)
                (make-vir-ref form)))
          (make-vir-constant form))
      (let ((operator (first form)))
        (if (and (symbolp operator)
                 (special-operator-p operator))
            (vir-convert-special-form operator (rest form) lexenv)
            (vir-convert-function-form operator (rest form) lexenv)))))

;;; Special Forms

(defmethod vir-convert-special-form (operator rest lexenv)
  (vectorizer-error
   "Cannot vectorize code containing the ~S special operator."
   operator))

(defmethod vir-convert-special-form ((_ (eql 'progn)) rest lexenv)
  (vir-convert-progn rest lexenv))

(defun vir-convert-progn (forms lexenv)
  (if (null forms)
      (make-vir-constant 'nil)
      (let ((n (length forms)))
        (loop for form in (subseq forms 0 (1- n)) do
          (vir-convert form lexenv))
        (vir-convert (elt forms (1- n)) lexenv))))

(defmethod vir-convert-special-form ((_ (eql 'locally)) rest lexenv)
  (multiple-value-bind (body-forms declarations) (vir-parse-body rest)
    ;; TODO handle declarations.
    (vir-convert-progn body-forms lexenv)))

(defmethod vir-convert-special-form ((_ (eql 'the)) rest lexenv)
  (unless (= 2 (length rest))
    (vectorizer-error "Malformed THE form: ~S" `(the ,@rest)))
  ;; TODO check the type.
  (vir-convert (second rest) lexenv))

(defmethod vir-convert-special-form ((_ (eql 'let)) rest lexenv)
  (when (or (atom rest) (not (listp (first rest))))
    (vectorizer-error "Malformed LET form: ~S" `(let ,@rest)))
  (let ((new-lexenv lexenv))
    (loop for (variable form) in (mapcar #'vir-canonicalize-binding (first rest)) do
      (let ((vir (vir-convert form lexenv)))
        (push (list variable vir t) new-lexenv)))
    (multiple-value-bind (body-forms declarations) (vir-parse-body (rest rest))
      ;; TODO handle declarations.
      (vir-convert-progn body-forms new-lexenv))))

(defmethod vir-convert-special-form ((_ (eql 'let*)) rest lexenv)
  (when (or (atom rest) (not (listp (first rest))))
    (vectorizer-error "Malformed LET* form: ~S" `(let ,@rest)))
  (loop for (variable form) in (mapcar #'vir-canonicalize-binding (first rest)) do
    (let ((vir (vir-convert form lexenv)))
      (push (list variable vir t) lexenv)))
  (multiple-value-bind (body-forms declarations) (vir-parse-body (rest rest))
    ;; TODO handle declarations.
    (vir-convert-progn body-forms lexenv)))

(defmethod vir-convert-special-form ((_ (eql 'function)) rest lexenv)
  (vectorizer-error
   "Cannot vectorize the FUNCTION special form in this position."))

(defun vir-parse-body (body)
  (let ((body body)
        (declarations '()))
    ;; Process declarations
    (loop for item = (first body) until (or (atom item) (not (eq (first item) 'declare))) do
      (pop body)
      (loop for declaration-specifier in (rest item) do
        (unless (consp declaration-specifier)
          (vectorizer-error
           "Malformed declaration specifier: ~S"
           declaration-specifier))
        (push declaration-specifier declarations)))
    (values body (reverse declarations))))

(defun vir-canonicalize-binding (binding)
  (typecase binding
    (symbol
     (list binding nil))
    ((cons symbol (cons t null))
     binding)
    (otherwise
     (vectorizer-error "Malformed binding: ~S" binding))))

;;; Function Forms

(defmethod vir-convert-function-form (operator rest lexenv)
  (vectorizer-error
   "Cannot vectorize function forms starting with ~S."
   operator))

(defmethod vir-convert-function-form ((symbol symbol) rest lexenv)
  (vir-convert-function-form 'funcall `(#',symbol ,@rest) lexenv))

(defmethod vir-convert-function-form ((cons cons) rest lexenv)
  (unless (lambda-expression-p cons)
    (return-from vir-convert-function-form (call-next-method)))
  (vir-convert-function-form 'funcall `(#',cons ,@rest) lexenv))

(defmethod vir-convert-function-form ((_ (eql 'funcall)) rest lexenv)
  (flet ((convert (x) (vir-convert x lexenv)))
    (when (null rest)
      (vectorizer-error "FUNCALL requires at least one argument."))
    (let ((function-form (first rest))
          (arguments (rest rest)))
      (unless (and (consp function-form)
                   (= (list-length function-form) 2)
                   (member (first function-form) '(function quote)))
        (vectorizer-error
         "Calling ~S is too complicated for the vectorizer."
         function-form))
      (let ((callee (second function-form)))
        (cond
          ;; Process a LAMBDA expression as the equivalent LET expression.
          ((and (eq (first function-form) 'function)
                (lambda-expression-p callee))
           (let ((lambda-list (second callee))
                 (body (rest (rest callee))))
             (unless (null (intersection lambda-list lambda-list-keywords))
               (vectorizer-error
                "Cannot vectorize the lambda list ~S."
                lambda-list))
             (unless (= (length lambda-list) (length rest))
               (vectorizer-error
                "Wrong number of arguments in ~S."
                (list* 'funcall rest)))
             (convert `(let ,(mapcar #'list lambda-list rest) ,@body))))
          ;; Special case certain function calls that we only allow within
          ;; index calculations.
          ((eq callee '+)
           (apply #'vir-index+ (mapcar #'convert arguments)))
          ((eq callee '-)
           (unless (plusp (length arguments))
             (vectorizer-error "The function - expects at least one argument"))
           (apply #'vir-index- (mapcar #'convert arguments)))
          ((eq callee '*)
           (apply #'vir-index* (mapcar #'convert arguments)))
          ((eq callee '1+)
           (unless (= 1 (length arguments))
             (vectorizer-error "The function 1+ expects a single argument."))
           (vir-index+ (convert (first arguments)) (make-vir-constant 1)))
          ((eq callee '1-)
           (unless (= 1 (length arguments))
             (vectorizer-error "The function 1- expects a single argument."))
           (vir-index- (convert (first arguments)) (make-vir-constant 1)))
          (t
           (let ((function-record (find-function-record callee nil)))
             (unless (scalar-function-record-p function-record)
               (vectorizer-error "Cannot vectorize calls to the function ~S." callee))
             (vir-funcall function-record (mapcar #'convert arguments)))))))))
