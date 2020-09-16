;;;; Deque class


;;; Node class to create doubly linked lists
(defclass node ()
  ((content
    :initarg :content
    :accessor content)
   (prev
    :initform nil
    :accessor prev)
   (next
    :initform nil
    :accessor next)))

(defun make-node (content &key prev next)
  "Creates a new node, doubly linked to nodes prev and next. Returns the new node"
  (let ((n (make-instance 'node :content content)))
    (if prev (setf (next prev) n (prev n) prev))
    (if next (setf (prev next) n (next n) next))
    (values n)))

(defun copy-node (node)
  "Returns a copy of node"
  (make-node (content node) :prev (prev node) :next (next node)))

(defun bind-nodes (a b)
  "Bind nodes a and b, placing a after b"
  (setf (next a) b (prev b) a))

(defmethod print-object ((obj node) stream)
  "Prints a node object and its content. Output has the format:

<NODE content sole|first|middle|last>

  The descriptors mean:
  
  * sole   - the node is not linked to other nodes
  * first  - the node is the first in a list
  * middle - the node is in the middle of a list
  * last   - the node is the last in a list" 
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((content content)
                     (next next)
                     (prev prev))
        obj
      (format stream "~a ~:[~:[sole~;first~]~;~:[last~;middle~]~]" content prev next))))

(defun print-list (lst &key from-end)  
  "Prints out the items of a linked list in separate lines"
  (let ((direction (if from-end 'prev 'next)))
    (loop for i = lst then (slot-value i direction)
          while i do (pprint i))))

(defmacro do-linked-list ((var lst &key from-end) &body body)
  (let ((i (gensym)))
    "Iterates over lst in either direction"
    `(loop for ,i = ,lst
           then (,(if from-end 'prev 'next) ,i)
           while ,i
           do (let ((,var (content ,i))) (progn ,@body)))))

(defun make-linked-list (lst)
  "Creates a doubly linked list from a common list. Returns
pointers to the first and last elements in the list and the
number of nodes in the list."
  (if lst 
      (loop with 1st = (make-node (car lst))
            for i in lst
            for j = 1st then (make-node i :prev j)
            counting t into n
            finally (return (values 1st j n)))
      (values nil nil 0)))


;;; Deque class

(defclass deque ()
  ((element-count
    :initarg :element-count
    :accessor element-count)
   (first-element
    :initform nil
    :accessor first-element)
   (last-element
    :initform nil
    :accessor last-element)))

(defmethod print-object ((obj deque) stream)
    "Prints a deque object. Output has the format:

<DEQUE :elements <element-count> :contents (first ... last)>"
    (print-unreadable-object (obj stream :type t)    
      (with-accessors ((first first-element)
                       (last last-element)
                       (c element-count)
                       (p pointer))
          obj
        (format stream "~[empty~:;:elements ~:*~d :content ~:*(~[~;~a~;~a ~a~:;~a ... ~a~])~]"
                c
                (if first (content first))
                (if last (content last))))))


(defun make-deque (&optional lst)
  "Constructor for deque object. Takes a list as argument and returns a deque
with the same elements in order."
  (multiple-value-bind (first last n)
      (make-linked-list lst)
    (let ((d (make-instance 'deque :element-count n)))
      (setf (first-element d) first
            (last-element d) last)
      (values d))))


;;; Ancillary functions for pop and append functions
(declaim (inline add-first-element remove-single-element))

(defmethod add-first-element ((obj deque) element)
  "Adds one element to an empty deque"
  (let ((new-node (make-node element)))
    (setf (element-count obj) 1
          (first-element obj) new-node 
          (last-element obj) new-node)))

(defmethod remove-single-element ((obj deque))
  "Empties a deque containing one element"
  (setf (element-count obj) 0
        (first-element obj) nil
        (last-element obj) nil))

(defmethod empty-deque-p ((obj deque))
  "Tests whether a deque is empty"
  (zerop (element-count obj)))

(defmethod append-element ((obj deque) element)
  "Add one element to the end of a deque. Return the enlarged deque."
  (if (empty-deque-p obj)
      (add-first-element obj element)
      (progn (make-node element :prev (last-element obj))
             (incf (element-count obj))
             (setf (last-element obj)
                   (next (last-element obj)))))
  (values obj))


;;; Functions for appending, prepending and removing elements from
;;; either end of the deque.
(defmethod prepend-element ((obj deque) element)
  "Add one element to the start of a deque. Return the enlarged deque."
  (if (zerop (element-count obj))
      (add-first-element obj element)
      (progn (make-node element :next (first-element obj))
             (incf (element-count obj))
             (setf (first-element obj)
                    (prev (first-element obj)))))
  (values obj))


(defmethod pop-last ((obj deque))
  "Remove one element from the end of a deque. Return the shortened deque."
  (let ((result (unless (zerop (element-count obj))
                  (content (last-element obj)))))
    (case (element-count obj)
      (0
       (values nil nil))
      (1
       (remove-single-element obj)
       (values result t))
      (otherwise
       (setf (last-element obj) (prev (last-element obj))
             (next (last-element obj)) nil)
       (decf (element-count obj))
       (values result t)))))

(defmethod pop-first ((obj deque))
  "Remove one element from the start of a deque. Return the shortened deque."
  (let ((result (unless (zerop (element-count obj))
                  (content (first-element obj)))))
    (case (element-count obj)
      (0
       (values nil nil))
      (1
       (remove-single-element obj)
       (values result t))
      (otherwise
       (setf (first-element obj) (next (first-element obj))
             (prev (first-element obj)) nil)
       (decf (element-count obj))
       (values result t)))))

(defmethod insert-element ((obj deque) content position)
  "Inserts an element containing 'content' in position 'position' (zero offset).
Returns the resulting deque."
  (cond ((zerop position)
         (prepend-element obj content))
        ((= position (element-count obj))
           (append-element obj content))
        (t
         (loop repeat position
               for j = (first-element obj) then (next j)
               finally (progn (make-node content :prev j :next (next j))
                              (incf (element-count obj))))))
  (values obj))

(defmethod nth-element ((obj deque) n &key from-end  &aux (c (element-count obj)))
  "Returns the nth element of a deque. If from-end is non-nil, returns the nth element before last."
  (assert (<= n c)
          ()
          "Index out of range. Position ~d requested, but deque has only ~d elements" n c)
  (loop with d = (if from-end 'prev 'next)
        repeat (1+ n)
        for k = (slot-value obj (if from-end 'last-element 'first-element))
        then (slot-value k d)
        finally (return (content k))))

(defmethod change-nth-element ((obj deque) pos value &key from-end &aux (c (element-count obj)))
  "Changes the value of the 'pos' element in a deque to 'value'.
If 'from-end' is T, the deque is traversed in reverse order."
  (assert (<= pos c)
          ()
          "Index out of range. Position ~d requested, but deque has only ~d elements" pos c)
  (loop with d = (if from-end 'prev 'next)
        repeat (1+ pos)
        for k = (slot-value obj (if from-end 'last-element 'first-element))
        then (slot-value k d)
        finally (return (setf (content k) value))))

(define-setf-expander nth-element (obj n &key from-end)
  "Makes individual elements of a deque setf-able using the change-nth-element function."
  (let ((input (gensym)))
    (values '()
            '()
            `(,input)
            `(progn (change-nth-element ,obj ,n ,input :from-end ,from-end) ,input)
            `(nth-element obj pos &key from-end))))


(defmacro do-deque ((var deque &key from-end) &body body)
  "Executes the closure 'body' for each element of a deque. If from-end is t,
iterates over the deque in reverse order."
  `(do-linked-list (,var 
                    ,@(if from-end `((last-element ,deque) :from-end t)
                          `((first-element ,deque))))
     ,@body))

(defmethod find-element ((obj deque) element)
  "Finds the first occurrence of element in a deque, scanning it from
start to end. Returns the element if successful, nil otherwise"
  (let ((i (first-element obj)))
    (block nil
      (tagbody
       ::loop
         (if (eq (content i) element) (return-from nil (content i)))
         (setf i (next i))
         (if (null i) (return-from nil nil))
         (go ::loop)))))


(defmethod find-element-pos ((obj deque) element)
  "Finds the position of element in a deque, scanning it from start to end.
Returns the element if successful, nil otherwise"
  (let ((i (first-element obj)) (pos 0))
    (block nil
      (tagbody
       ::loop
         (if (eq (content i) element) (return-from nil pos))
         (setf i (next i) pos (1+ pos))
         (if (null i) (return-from nil nil))
         (go ::loop)))))
