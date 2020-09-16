;;;; Tests for cl-deque.lisp (REPL output)

CL-USER> (make-deque '(1))
#<DEQUE :elements 1 :content (1)>
CL-USER> (make-deque '(1 2))
#<DEQUE :elements 2 :content (1 2)>
CL-USER> (make-deque '(1 2 3))
#<DEQUE :elements 3 :content (1 ... 3)>
CL-USER> (make-deque '(1 2 3 4))
#<DEQUE :elements 4 :content (1 ... 4)>
CL-USER> (make-deque '(1 2 3 4 5))
#<DEQUE :elements 5 :content (1 ... 5)>
CL-USER> (defvar v (make-deque '(1 2 3 4 5 6)))
V
CL-USER> v
#<DEQUE :elements 6 :content (1 ... 6)>
CL-USER> (append-element v 7)
#<DEQUE :elements 7 :content (1 ... 7)>
CL-USER> v
#<DEQUE :elements 7 :content (1 ... 7)>
CL-USER> (prepend-element v 0)
#<DEQUE :elements 8 :content (0 ... 7)>
CL-USER> (pop-first v)
0
T
CL-USER> v
#<DEQUE :elements 7 :content (1 ... 7)>
CL-USER> (pop-last v)
7
T
CL-USER> v
#<DEQUE :elements 6 :content (1 ... 6)>
CL-USER> (do-deque (p v) (format t "~d~%" p))
1
2
3
4
5
6
NIL
CL-USER> (do-deque (p v :reverse t) (format t "~d~%" p))
6
5
4
3
2
1
NIL
CL-USER> (nth-element v 0)
1
CL-USER> (nth-element v 0 :reverse t)
6
CL-USER> (setf (nth-element v 0) 1000) => #(1000 2 3 4 5 6)
1000
CL-USER> (setf (nth-element v 0 :reverse t) 6000) => #(1000 2 3 4 5 6000)
6000
CL-USER> (find-element v 2)
2
CL-USER> (find-element v 6000)
6000
CL-USER> (find-element-pos v 2)
1
