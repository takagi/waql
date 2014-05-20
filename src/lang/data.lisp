#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.data)

;;
;;  Syntax:
;;
;;    WAQL-SYMBOL-P object => boolean
;;
;;  Arguments and Values:
;;
;;    object --- an object.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun waql-symbol-p (object)
  (and (symbolp object)
       (not (eq object t))
       (not (eq object nil))
       (not (underscore-notation-p object))))

;;
;;  Syntax:
;;
;;    PERCENT-SYMBOL-P symbol => boolean
;;
;;  Arguments and Values:
;;
;;    symbol --- a WAQL symbol.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if SYMBOL is not a WAQL symbol.
;;
(defun percent-symbol-p (symbol)
  (unless (waql-symbol-p symbol)
    (error 'type-error :datum symbol :expected-type 'waql-symbol))
  (string= (symbol-name symbol) "%" :end1 1))

;;
;;  Syntax:
;;
;;    PERCENT-SYMBOL symbol count => new-symbol
;;
;;  Arguments and Values:
;;
;;    symbol --- a WAQL symbol.
;;    count --- an integer.
;;    new-symbol --- a WAQL symbol.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if SYMBOL is not a WAQL symbol
;;    or underscore notation.
;;
;;    Signals an error of type type-error if COUNT is not an integer.
;;
(defun percent-symbol (symbol count)
  (unless (or (waql-symbol-p symbol)
              (underscore-notation-p symbol))
    (error 'type-error :datum symbol :expected-type 'waql-symbol))
  (unless (integerp count)
    (error 'type-error :datum count :expected-type 'integer))
  (let ((symbol-package (symbol-package symbol))
        (symbol-name (symbol-name symbol))
        (count-str (princ-to-string count)))
    (let ((symbol1 (format-symbol symbol-package
                                  "%~A~A" symbol-name count-str)))
      (setf (get symbol1 'original-symbol) symbol)
      symbol1)))

;;
;;  Syntax:
;;
;;    ORIGINAL-SYMBOL symbol => original-symbol
;;
;;  Arguments and Values:
;;
;;    symbol --- a WAQL symbol.
;;    original-symbol --- a WAQL symbol.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun original-symbol (symbol)
  (get symbol 'original-symbol))

;;
;;  Syntax:
;;
;;    UNDERSCORE-NOTATION-P symbol => boolean
;;
;;  Arguments and Values:
;;
;;    symbol --- a WAQL symbol.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun underscore-notation-p (object)
  (string= "_" (symbol-name object)))

;;
;;  Syntax:
;;
;;    SCOPED-SYMBOL symbol scope => new-symbol
;;
;;  Arguments and Values:
;;
;;    symbol --- a WAQL symbol.
;;    scope --- a WAQL symbol.
;;    new-symbol --- a WAQL symbol.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if SYMBOL is not a WAQL symbol.
;;
;;    Signals an error of type type-error if SCOPE is not a WAQL symbol.
;;
;;    Signals an error of type simple-error if SYMBOL and SCOPE are not in
;;    a same package.
;;
(defun scoped-symbol (symbol scope)
  (unless (waql-symbol-p symbol)
    (error 'type-error :datum symbol :expected-type 'waql-symbol))
  (when scope
    (unless (waql-symbol-p scope)
      (error 'type-error :datum scope :expected-type 'waql-symbol))
    (unless (eq (symbol-package symbol)
                (symbol-package scope))
      (error "The symbols ~S and ~S are not in a same package."
             symbol scope)))
  (if scope
      (let ((symbol-package (symbol-package symbol)))
        (format-symbol symbol-package "~A.~A" scope symbol))
      symbol))

;;
;;  Syntax:
;;
;;    SCOPING-SYMBOL symbol => new-symbol
;;
;;  Arguments and Values:
;;
;;    symbol --- a WAQL symbol.
;;    new-symbol --- a WAQL symbol.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if SYMBOL is not a WAQL symbol.
;;
(defvar *scoping-count* 1)

(defun scoping-symbol (symbol)
  (prog1 (format-symbol (symbol-package symbol)
                        "%~A~A" symbol *scoping-count*)
    (incf *scoping-count*)))

;;
;;  Syntax:
;;
;;    WAQL-BOOLEAN-P object => boolean
;;
;;  Arguments and Values:
;;
;;    object --- an object.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun waql-boolean-p (object)
  (or (eq object t)
      (eq object nil)))

;;
;;  Syntax:
;;
;;    WAQL-INTEGER-P object => boolean
;;
;;  Arguments and Values:
;;
;;    object --- an object.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun waql-integer-p (object)
  (integerp object))

;;
;;  Syntax:
;;
;;    WAQL-STRING-P object => boolean
;;
;;  Arguments and Values:
;;
;;    object --- an object.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun waql-string-p (object)
  (stringp object))

;;
;;  Syntax:
;;
;;    TIME+ time interval => new-time
;;
;;  Arguments and Values:
;;
;;    time --- a time.
;;    interval --- an interval.
;;    new-time --- a time.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to INTERVAL-AMOUNT function, INTERVAL-UNIT function and
;;    LOCAL-TIME:TIMESTAMP+ function.
;;
(defun time+ (time interval)
  (let ((amount (interval-amount interval))
        (unit   (interval-unit interval)))
    (timestamp+ time amount unit)))

;;
;;  Syntax:
;;
;;    TIME- time interval => new-time
;;
;;  Arguments and Values:
;;
;;    time --- a time.
;;    interval --- an interval.
;;    new-time --- a time.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to INTERVAL-AMOUNT function, INTERVAL-UNIT function and
;;    LOCAL-TIME:TIMESTAMP- function.
;;
(defun time- (time interval)
  (let ((amount (interval-amount interval))
        (unit   (interval-unit interval)))
    (timestamp- time amount unit)))

;;
;;  Syntax:
;;
;;    INTERVAL-UNIT-P object => boolean
;;
;;  Arguments and Values:
;;
;;    object --- an object.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun interval-unit-p (object)
  (and (member object '(:minute :hour :day :month :year))
       t))

;;
;;  Syntax:
;;
;;    MAKE-INTERVAL amount unit => interval
;;
;;  Arguments and Values:
;;
;;    amount --- an integer.
;;    unit --- an interval unit.
;;    interval --- an interval.
;;
;;  Description:
;;
;;    Returns an interval object with AMOUNT in UNIT.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if AMOUNT is not an integer.
;;
;;    Signals an error of type simple-error if UNIT is not an interval
;;    unit.
;;
(defstruct (interval (:constructor %make-interval))
  (amount :amount :read-only t)
  (unit :unit :read-only t))

(defun make-interval (amount unit)
  (unless (integerp amount)
    (error 'type-error :datum amount :expected-type 'integer))
  (unless (interval-unit-p unit)
    (error "The value ~S is not an interval unit." unit))
  (%make-interval :amount amount :unit unit))

;;
;;  Syntax:
;;
;;    MINUTES amount => interval
;;
;;  Arguments and Values:
;;
;;    amount --- an integer.
;;    interval --- an interval.
;;
;;  Description:
;;
;;    Returns an interval object with AMOUNT in unit :minute.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to MAKE-INTERVAL function.
;;
(defun minutes (amount)
  (make-interval amount :minute))

;;
;;  Syntax:
;;
;;    HOURS amount => interval
;;
;;  Arguments and Values:
;;
;;    amount --- an integer.
;;    interval --- an interval.
;;
;;  Description:
;;
;;    Returns an interval object with AMOUNT in unit :hour.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to MAKE-INTERVAL function.
;;
(defun hours (amount)
  (make-interval amount :hour))

;;
;;  Syntax:
;;
;;    DAYS amount => interval
;;
;;  Arguments and Values:
;;
;;    amount --- an integer.
;;    interval --- an interval.
;;
;;  Description:
;;
;;    Returns an interval object with AMOUNT in unit :day.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to MAKE-INTERVAL function.
;;
(defun days (amount)
  (make-interval amount :day))

;;
;;  Syntax:
;;
;;    WEEKS amount => interval
;;
;;  Arguments and Values:
;;
;;    amount --- an integer.
;;    interval --- an interval.
;;
;;  Description:
;;
;;    Returns an interval object with AMOUNT multiplied by seven in unit
;;    :day.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to MAKE-INTERVAL function.
;;
(defun weeks (amount)
  (make-interval (* 7 amount) :day))

;;
;;  Syntax:
;;
;;    MONTHS amount => interval
;;
;;  Arguments and Values:
;;
;;    amount --- an integer.
;;    interval --- an interval.
;;
;;  Description:
;;
;;    Returns an interval object with AMOUNT in unit :month.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to MAKE-INTERVAL function.
;;
(defun months (amount)
  (make-interval amount :month))

;;
;;  Syntax:
;;
;;    YEARS amount => interval
;;
;;  Arguments and Values:
;;
;;    amount --- an integer.
;;    interval --- an interval.
;;
;;  Description:
;;
;;    Returns an interval object with AMOUNT in unit :year.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to MAKE-INTERVAL function.
;;
(defun years (amount)
  (make-interval amount :year))

;;
;;  Syntax:
;;
;;    TUPLE element* => tuple
;;
;;  Arguments and Values:
;;
;;    elements --- objects.
;;    tuple --- a tuple.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defstruct (tuple (:constructor %make-tuple)
                  (:conc-name %tuple-)
                  (:print-function print-tuple))
  (elements :elements :read-only t))

(defun tuple (&rest elements)
  (%make-tuple :elements elements))

(defun print-tuple (tuple stream depth)
  (declare (ignore depth))
  (format stream "#S(TUPLE ~{~S~^ ~})" (%tuple-elements tuple)))


;;
;;  Syntax:
;;
;;    TUPLE-REF tuple i &key accept-nil-p => object
;;
;;  Arguments and Values:
;;
;;    tuple --- a tuple or nil.
;;    i --- a non-negative integer.
;;    accept-nil-p --- a generalized boolean. The default is false.
;;    object --- an object.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    When ACCEPT-NIL-P is true, signals an error of type-error if TUPLE
;;    is not a tuple nor nil. Otherwise, siglans an error of type-error
;;    if TUPLE is not a tuple.
;;
;;    Signals an error of type-error if I is not a non-negative integer.
;;
;;    Signals an error of simple-error if I is equal or larger than
;;    dimension of TUPLE.
;;
(defun tuple-ref (tuple i &key accept-nil-p)
  (when tuple
    (unless (< i (tuple-dim tuple))
      (error "The value ~S is equal or larger than dimension of ~S"
             i tuple)))
  (if accept-nil-p
      (when tuple
        (nth i (%tuple-elements tuple)))
      (nth i (%tuple-elements tuple))))

;;
;;  Syntax:
;;
;;    TUPLE-DIM tuple => dim
;;
;;  Arguments and Values:
;;
;;    tuple --- a tuple.
;;    dim --- a non-negative integer.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if TUPLE is not a tuple.
;;
(defun tuple-dim (tuple)
  (length (%tuple-elements tuple)))

;;
;;  Syntax:
;;
;;    EMPTY-RELATION-INDEX => empty-relation-index
;;
;;  Arguments and Values:
;;
;;    empty-relaion-index --- a relation index.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defstruct (relation-index (:constructor %make-relation-index)
                           (:conc-name %relation-index-))
  (body :body :read-only t))

(defun empty-relation-index ()
  (let ((body (make-hash-table :test #'equalp)))
    (%make-relation-index :body body)))

;;
;;  Syntax:
;;
;;    ADD-RELATION-INDEX relation-index key tuple => index
;;
;;  Arguments and Values:
;;
;;    relation-index --- a relation index.
;;    key --- an object.
;;    tuple --- a tuple.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if INDEX is not a relation index.
;;
;;    Signals an error of type type-error if TUPLE is not a tuple.
;;
(defun add-relation-index (relation-index key tuple)
  (unless (tuple-p tuple)
    (error 'type-error :datum tuple :expected-type 'tuple))
  (symbol-macrolet ((body (%relation-index-body relation-index))
                    (place (gethash key body)))
    (setf place (cons tuple place)))
  relation-index)

;;
;;  Syntax:
;;
;;    LOOKUP-RELATION-INDEX relation-index key => tuple
;;
;;  Arguments and Values:
;;
;;    relation-index --- a relation index.
;;    key --- an object.
;;    tuple --- a tuple.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if INDEX is not a relation index.
;;
(defun lookup-relation-index (relation-index key)
  (gethash key (%relation-index-body relation-index)))

;;
;;  Syntax:
;;
;;    EMPTY-RELATION => empty-relation
;;
;;  Arguments and Values:
;;
;;    empty-relation --- a relation.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defstruct (relation (:constructor %make-relation)
                     (:conc-name %relation-))
  (body :body :read-only t)
  (indices :indices))

(defun %relation-index (relation i)
  (nth i (%relation-indices relation)))

(defun empty-relation ()
  (let ((body (make-hash-table :test #'equalp)))
    (%make-relation :body body :indices nil)))

;;
;;  Syntax:
;;
;;    RELATION->LIST relation => tuple-list
;;
;;  Arguments and Values:
;;
;;    relation --- a relation.
;;    tuple-list --- a list of tuples.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if RELATION is not a relation.
;;
(defun relation->list (relation)
  (hash-table-keys (%relation-body relation)))

;;
;;  Syntax:
;;
;;    RELATION-COUNT relation => count
;;
;;  Arguments and Values:
;;
;;    relation --- a relation.
;;    count --- an integer.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if RELATION is not a relation.
;;
(defun relation-count (relation)
  (hash-table-count (%relation-body relation)))

;;
;;  Syntax:
;;
;;    RELATION-EXISTS relation => boolean
;;
;;  Arguments and Values:
;;
;;    relation --- a relation.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if RELATION is not a relation.
;;
(defun relation-exists (relation)
  (> (relation-count relation) 0))

;;
;;  Syntax:
;;
;;    RELATION-MEMBER relation tuple => boolean
;;
;;  Arguments and Values:
;;
;;    relation --- a relation.
;;    tuple --- a tuple.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if RELATION is not a relation.
;;
;;    Signals an error of type type-error if TUPLE is not a tuple.
;;
(defun relation-member (relation tuple)
  (unless (tuple-p tuple)
    (error 'type-error :datum tuple :expected-type 'tuple))
  (gethash tuple (%relation-body relation)))

;;
;;  Syntax:
;;
;;    RELATION-ADJOIN tuple relation => relation
;;
;;  Arguments and Values:
;;
;;    relation --- a relation.
;;    tuple --- a tuple.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if RELATION is not a relation.
;;
;;    Signals an error of type type-error if TUPLE is not a tuple.
;;
;;    Signals an error of type simple-error if TUPLE has different
;;    attributes than ones in RELATION.
;;
(defun relation-adjoin (tuple relation)
  (unless (tuple-p tuple)
    (error 'type-error :datum tuple :expected-type 'tuple))
  (symbol-macrolet ((body (%relation-body relation))
                    (indices (%relation-indices relation)))
    ;; if tuple does not exist in body
    (unless (gethash tuple body)
      ;; add tuple to body as key
      (setf (gethash tuple body) t)
      ;; add tuple to each index
      (let ((dim (tuple-dim tuple)))
        ;; if no indices, prepare them
        (unless indices
          (setf indices (loop repeat dim
                           collect (empty-relation-index))))
        ;; actually add tuple to each index
        (loop for i from 0 below dim
           do (let ((val (tuple-ref tuple i))
                    (relation-index (nth i indices)))
                (add-relation-index relation-index val tuple)))))
    relation))

;;
;;  Syntax:
;;
;;    RELATION-ADJOIN-ALL tuple-list relation => relation
;;
;;  Arguments and Values:
;;
;;    tuple-list --- a list of tuples.
;;    relation --- a relation.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if TUPLE-LIST does not a list.
;;
(defun relation-adjoin-all (tuple-list relation)
  (reduce #'relation-adjoin tuple-list
          :initial-value relation
          :from-end t))

;;
;;  Syntax:
;;
;;    RELATION-INDEX-LOOKUP relation lookup-keys => tuple-list
;;
;;  Arguments and Values:
;;
;;    relation --- a relation.
;;    lookup-keys --- a list of lookup keys.
;;    tuple-list --- a list of tuples.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if RELATION is not a relation.
;;
;;    Signals an error of type type-error if LOOKUP-KEYS is not a list.
;;
;;    Signals an error of type simple-error if any of LOOKUP-KEYS are
;;    invalid.
;;
(defun relation-index-lookup (relation lookup-keys)
  (cond
    ((not (relation-exists relation))
     nil)
    ((null lookup-keys)
     (relation->list relation))
    (t
     (%relation-index-lookup relation lookup-keys))))

(defun %relation-index-lookup (relation lookup-keys)
  (labels ((aux (i-val)
             (destructuring-bind (i . val) i-val
               (let ((relation-index (%relation-index relation i)))
                 (lookup-relation-index relation-index val)))))
    (let ((i-vals (mapcar #'lookup-key-relative lookup-keys)))
      (let ((results (mapcar #'aux i-vals)))
        (minimize results :key #'length)))))

(defun valid-lookup-key (lookup-key)
  (single (remove nil lookup-key)))

(defun lookup-key-relative (lookup-key)
  (unless (valid-lookup-key lookup-key)
    (error "The value ~S is invalid as lookup-key." lookup-key))
  (loop for element in lookup-key
        for i from 0
     when element
     return (cons i element)))

;;
;;  Syntax:
;;
;;    (ITERATE:ITER (FOR-TUPLE var-list IN-RELATION relation USING key)
;;                  (COLLECT-RELATION tuple))
;;
;;  Arguments and Values:
;;
;;    var-list --- a list of symbols.
;;    relation --- a relation.
;;    key --- a list of objects.
;;    tuple --- a tuple.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;

;; driver for relation
(defmacro for-tuple (vars IN-RELATION relation &optional USING keys)
  (unless (eq IN-RELATION 'in-relation)
    (error "Invalid IN-RELATION clause."))
  (when USING
    (unless (eq USING 'using)
      (error "Invalid USING clause.")))
  `(iterate:for ,vars in (mapcar #'%tuple-elements
                                 (relation-index-lookup ,relation ,keys))))

;; gatherer for relation
(defmacro collect-relation (expr)
  `(iterate:reducing ,expr
                  by (flip #'relation-adjoin)
       initial-value (empty-relation)))
