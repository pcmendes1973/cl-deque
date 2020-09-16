# cl-deque
Deque structure in Common Lisp, along the lines of the similar class in C++, plus a few `Lisp`-ian twists.

The main deque operations are available, including push and pop on both ends, an iterate macro (`do-deque`),
search (element or position of an element), and check-if-null. Individual elements of the deque are also setfable.
