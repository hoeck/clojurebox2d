
;  #^{:doc "Iterator macro
;  implements a simple and extendable iteration facility on top of clojure loop"}

(ns hoeck.iterate
  (:use clojure.walk
        clojure.contrib.macro-utils
        clojure.contrib.except
        clojure.test))

(defn context-walk
  "Recursively performs applications of f to forms and subforms until
  the results do not change anymore. Does not apply f to quoted forms:
  (context-walk _ (quote a)) -> (quote a)."
  [f form]
  (let [quoted? #(and (seq? %) (= (first %) 'quote))]
    (walk #(if (quoted? %) % (context-walk f (f %)))
          #(if (quoted? %)
             %
             (let [r (f %)]
               (if (= r %)
                 %
                 (context-walk f r))))
          form)))

(defmulti
;"This function is called by the iter macro at expansion-time, and dispatches
;  on the first arg of a given expression.
;  Returns a hashmap of :loop-binding, :while, :recur-form and :bind-form which 
;  values are used to build the final loop clause.
;  See the definition of `iter-for in' for a sample implementation.
;  Must return nil if the given expression is not a iter-driver."
  iter-driver
  (fn [[driver-name & args]] (symbol (name driver-name)))
  :default nil)

;; default iter driver, returns nil, -> form is not an iteration-driver expression
(defmethod iter-driver nil [& args] nil)

;; special driver, only creates invariant in the loop body to use in the recur form.
;; only allowed (but not enforced) to use after other driver definitions
;; (because ordering of invariants is important in recur calls)
(defmethod iter-driver 'with [[driver-name bind-name initial-value]]
  {:loop-binding [bind-name initial-value]})

;; special driver for creating let-bindings around the whole iter clause
;; best used as the first clause in an iter expression
(defmethod iter-driver 'let [[driver-name let-name let-value & more]]
  ;; iter-let only allows one binding, not wrapped in a vector
  ;; to not confuse it with the clojure let
  (if more 
    nil ;; nil means that this is not a driver-expression
    {:outer-let [let-name let-value]}))

(defmacro iter*
  "simple iteration right here."
  [& args]
  ;; syntax:
  ;; iter-driver{1..many}
  ;; with-clause{0..1}
  ;; iter-body{1} <- adheres to recur rules
  ;; finally-clause{0..1} .. evaluated when there are no more items left
  (let [drivers (take-while identity (map iter-driver args))

        ;; the body, and finally-clause; if you branch in body, a finally clause does not work
        [body finally] (drop-while iter-driver args)

        let-bindings (apply vector (mapcat :bind-form drivers)) ;; inner-let
        loop-bindings (apply vector (mapcat :loop-binding drivers)) ;; loop invariants
        while-clauses (remove nil? (map :while drivers)) ;; loop as long as those hold
        recur-forms (remove nil? (map :recur-form drivers)) ;; auto-inserted recurs        
        outer-let (apply vector (mapcat :outer-let drivers)) ;; a let around the whole loop

        ;; 'with only produces loop-bindings, in case one calls recur without additional arguments,
        ;; provide identiy values
        optional-recur-forms (drop (count recur-forms) (map first (partition 2 loop-bindings)))]
    `(let ~outer-let
       (loop ~loop-bindings         
         (if (and ~@while-clauses)
           (let ~let-bindings             
             ~(let [my-recur (gensym 'recur)]
                ;; use unique my-recur symbol to only replace recur's once
                (context-walk
                 #(if (and (seq? %) (= (first %) my-recur))
                    (concat '(recur) (rest %))
                    %)
                 (context-walk
                  #(if (and (seq? %) (= (first %) 'recur))
                     (let [given-forms (rest %)
                           forms-to-insert (concat recur-forms
                                                   given-forms
                                                   (drop (count given-forms) optional-recur-forms))]
                       (cons my-recur forms-to-insert))
                     %)
                  body))))
           ~finally)))))

;; macros around iteration
;; to express more comples operations like collect, return-if

(defmulti iter-macro
  (fn [expr]
    (if (seq? expr)
      (first expr)
      nil))
  :default nil)

;; default: return just the form, no expansion
(defmethod iter-macro nil [form]
  {:body form})

;; expands toplevel iter* body forms according to iter-macro
(defmacro iter
  "Iteration macro."
  [& args]
  (let [drivers (take-while iter-driver args)
        body-expansions (map iter-macro (drop-while iter-driver args))
        body (map :body body-expansions)
        finally (remove nil? (map :finally body-expansions))]
    `(iter* ~@(concat drivers (mapcat :drivers body-expansions))
            ~@body
            ~@finally)))

;; iteration: for

(defmulti
;  "Multimethod for implementing different kinds of for loops.
;  Dispatches on the 'for-keyword', which is the 3rd argument
;  in a for clause: (for x _in_ ...)."
  iter-for
  (fn [[driver-name bind-expr for-key & args]]
    for-key)
  :default nil)

(defmethod iter-for nil [args]
  (throwf "unknown iter-for macro `%s' in `%s'" (first args) args))

(defmethod iter-driver 'for
  [for-def] (iter-for for-def))

;; sequence-for
(defmethod iter-for 'in
;; iteration in seqs using first and next
;; (for [x & r]
;;  in (range 0 9)
;;  by (drop 2 r)
;;  while (seq %)
  [for-def]
  (let [argm (apply hash-map for-def)

        seq-name (gensym 'for)
        replace-with-seqname (fn [form] 
                               (context-walk (fn [x] (if (= x '%) seq-name x))
                                             form))
        ;; mandatory 
        for (argm 'for)              
        in `(seq ~(argm 'in)) ;; wrap in a seq

        ;; optional
        by (replace-with-seqname (argm 'by '(next %)))
        while (replace-with-seqname (argm 'while '(seq %)))]
    {:loop-binding [seq-name in]
     :bind-form (if (symbol? for)
                  [for `(first ~seq-name)]
                  [for seq-name])
     :recur-form by
     :while while
     :outer-let nil}))

(deftest test-sequence-for
  (is (= (iter (for x in (range 0 9))
               (with a 0)
               (recur (+ a x))
               a)
         36) "seq iteration")
  (is (= (iter (for x in [1 2 3])
               (with a [])
               (recur (conj a x))
               a)) "seq collection"))

;; for c-like linked list datastructures, where calling .next
;; returns the next element of the collection
(defmethod iter-for 'call
  [for-def] ;; (iter (for x call .next on shape))
  (let [argm (apply hash-map for-def)
        for (argm 'for) ;; must be a symbol
        call (argm 'call) ;; a clojure method name: .foo
        on (argm 'on) ;; expression yielding an object
        while (argm 'while for)]
    {:loop-binding [for on]
     :bind-form nil
     :recur-form `(~call ~for)
     :while while}))

(deftest test-c-style-for
  (is (= (iter (for x call next on (range 0 9))
               (with a 0)
               (recur (+ a (first x)))
               a)
         36) "c-style-for"))

;; iterate over an array, binding each element to the name of (for _name_ ..)
(defmethod iter-for 'in-array
  ;; for x
  ;; in-array a
  [for-def]
  (let [argm (apply hash-map for-def)        
        array (gensym 'array) ;; the array itself
        idx (gensym 'idx) ;; the index to step through
        for (argm 'for) ;; the name of each array element
        in (argm 'in-array)] ;; the expr yielding an array
    {:outer-let [array in]
     :loop-binding [idx `(int 0)]
     :while `(< ~idx (alength ~array))
     :bind-form [for `(aget ~array ~idx)]
     :recur-form `(unchecked-inc ~idx)}))

(deftest test-array-for 
  (is (= (iter (for x in-array (into-array (range 0 9)))
               (with a 0)
               (recur (+ a x))
               a)
         36) "array-for"))

;; bind, in each iteration, for to as
(defmethod iter-for 'as
  ;; for y
  ;; as (.someMethod x)
  [for-def]
  (let [argm (apply hash-map for-def)]
    {:bind-form [(argm 'for) (argm 'as)]}))

(deftest test-for-as
  (is (= (iter (for x in (range 0 9))
               (for d as (* 2 x))
               (with a 0)
               (recur (+ a d))
               a)
         72) "for-as"))

;; iteration macro definitions

;; in each iteration, collect expr by conj onto a vector
(defmethod iter-macro 'collect [[mname expr]]
  (let [collect-name (gensym 'collect)]
    {:drivers `((~'with ~collect-name []))
     :body `(recur (conj ~collect-name ~expr))
     :finally collect-name}))

(deftest test-collect
  (is (= (iter (for x in (range 0 9))
               (collect (* 2 x)))
         (map (partial * 2) (range 0 9)))))

;; in each iteration, check for test, if true collect expr
(defmethod iter-macro 'collect-if [[name test expr]]
  (let [_expr (iter-macro (list 'collect expr))]
    {:drivers (:drivers _expr)
     :body `(if ~test 
              ~(:body _expr)
              (recur))
     :finally (:finally _expr)}))

(deftest test-collect-if
  (is (= (iter (for x in (range 0 9))
               (collect-if (odd? x) (* 2 x)))
         (map (partial * 2) (filter odd? (range 0 9))))))

;; in each iteration, check wether test holds, if so return immediatly
;; with ret from the loop
(defmethod iter-macro 'return-if [[name test ret]]
  {:body `(if ~test ~ret (recur))})

(deftest test-return-if
  (is (= (iter (for x in (range 0 9))
               (return-if (= x 4) x))
         4)))

(comment ;; more examples

  (iter (for x in '(1 2 3))
        (for y as (* 2 x))
        (collect-if (and (even? x) (even? y)) [x y]))

  (iter (for x in '(1 2 3))
        (return-if (even? x) x))

  (iter (for x in [1 2 3])
        (collect-if (odd? x) x))

  (iter (for x call next on '(1 2 3))
        (with a [])
        (recur (conj a x))
        a)

  (iter (for x in [1 2 3])
        (with sum 0)
        (recur (if (odd? x)
                 (+ sum x)
                 sum))
        sum)

  (iter (for x in [1 2 3])
        (with sum 0)
        (with bla 0)
        (recur (if (odd? x)
                 (+ sum x)
                 sum)
               (+ bla x))
        [sum bla])
  
  (iter (let a 1)
        (for i in '(1 2 3))
        (do (println i a)
            (recur))
        a)

  (iter (for x in-array (make-array (type 9) 3))
        (do (println x)
            (recur))))


;;   comparison: iter vs. plain loop
;;
;; one line:
;;(iter (for shape call .next on (query *world*)) (collect (jbox->clojurebox shape)))
;;(loop [shape (query *world*) bodies []] (if shape (recur (.next shape) (conj bodies (jbox->clojurebox shape))) bodies))
;;
;; iter: 81 chars (70% of loop)
;;       2 levels of nesting
;;       1 temporary binding: shape
;; loop: 117 chars (144% of iter)
;;       2 levels of nesting
;;       2 tmp bindings: shape, bodies
;;
;; pretty printed:
;;
;; (iter (for shape call .next on (query *world*)) 
;;       (collect (jbox->clojurebox shape)))
;;
;; (loop [shape (query *world*)
;;        bodies []]
;;   (if shape 
;;     (recur (.next shape)
;;            (conj bodies (jbox->clojurebox shape)))
;;     bodies))

