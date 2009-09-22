
(ns hoeck.clojurebox2d.utils
  (:use clojure.contrib.pprint
        clojure.contrib.macro-utils
        clojure.walk
        clojure.contrib.def        
        hoeck.clojurebox2d.jbox2d
        hoeck.iterate)
  (:import (java.util TreeMap)           
           (java.util.concurrent ConcurrentLinkedQueue ArrayBlockingQueue)
           (java.util.concurrent TimeUnit)
           (org.jbox2d.collision.shapes Shape)
           (org.jbox2d.dynamics Body World ContactListener)
           (org.jbox2d.dynamics.contacts ContactPoint)))

;; utils

(defmacro at-tick
  "wrap body in a conditional, so that it only executes every nth-tick + offset tick.
Ex: (at-tick 10 2 (foo)) <=> (when (== (rem tick 10) 2) (foo))."
  [nth-tick offset & body]
  `(when (== (rem ~'tick ~nth-tick) ~offset)
     ~@body))


;; structmaps as body userdata

(defstruct #^{:private :true} body-userdata-struct :draw-fn :type :name :destroyed)

(defn make-body-userdata
  "Create Body Userdata, defaults to a structmap: body-userdata-struct."
  [& initargs]
  (apply struct-map body-userdata-struct initargs))

(defn set-body-userdata-ctor
  "Set body userdata constructor (make-body-userdata)"
  [f]
  (alter-var-root #'make-body-userdata (constantly f)))

(defmacro body-userdata
  "Expand to code which calls the function f with the 
  bodys userdata (a structmap) and any given additional args.
  If no function is given, just return the bodys userdata."
  ([body] `(.getUserData ~body))
  ([body f] `(-> ~body .getUserData ~f))
  ([body f & args] `(-> ~body .getUserData (~f ~@args))))

(defmacro alter-body-userdata
  "apply f to the bodies userdata and additional args, store the result
  back into the bodies userdata slot."
  [body f & args]
  `(.setUserData ~body (body-userdata ~body ~f ~@args)))


;; basic world step

(defvar- step-world-ptime (atom (/ 1 60)))

(defn set-step-ptime
  "set the stepping time in milliseconds"
  [ptime]
  (swap! step-world-ptime (constantly ptime)))

(defn step-world 
  "jbox2d-loop: calculate the next physics world state. defaults to 17ms (60Hz).
  Change it with `set-step-ptime'."
  [tick]
  ;; ptime, iterations
  (.step *world* (max 0 @step-world-ptime) 10))


;; global tick

(def current-tick (atom -1))

(defn write-tick
  "jbox2d-loop: write the current tick to the current-tick atom."
  [tick]
  (swap! current-tick (constantly tick)))


;; tick-timed events

(defvar timed-events (atom (sorted-map)))

(defn mm-assoc
  "MultiMap assoc. Like assoc but wraps every value in a vec to
  allow more than one value per key."
  ([map key val] 
     (assoc map key (if-let [e (find map key)] 
                      (conj (clojure.core/val e) val)
                      [val])))
  ([map key val & kvs]
     (let [ret (mm-assoc map key val)]
       (if kvs
         (recur ret (first kvs) (second kvs) (nnext kvs))
         ret))))

(defn schedule-event
  "Execute the given function at scheduled-tick."
  [scheduled-tick f]
  (swap! timed-events mm-assoc scheduled-tick f))

(defn add-event*
  "Add a function f to the event queue, to be executed in n delay-ticks.
Without the given delay-ticks arg, set delay-ticks to 0.
f should take tick as the only parameter."
  ([f] (schedule-event @current-tick f))
  ([delay-ticks f] (schedule-event (+ @current-tick delay-ticks) f)))

(defmacro add-event
  "Same as add-event* but without the fn boilerplate."
  [delay-ticks & body]
  `(add-event* ~delay-ticks
               (fn [tick#]
                 ~@body)))

(defn reset-events
  "removes any pending events (useful after restarting the physics engine)
  and sets current-tick to 0."
  []
  (write-tick 0)
  (swap! timed-events #(reduce dissoc % (keys %))))

(defn run-timed-events
  "jbox2d-loop: function which triggers tick-timed events."
  [tick]
  (let [q @timed-events
        events (take-while #(<= (key %) tick) q)
        event-fns (apply concat (map val events))]
    (dorun (map #(% tick) event-fns))
    (apply swap! timed-events dissoc (map key events))))


;; running functions 

;;(defvar- jbox2d-tasks-queue (ConcurrentLinkedQueue.))
;;(defn- add-task [task-fn] (.add jbox2d-tasks-queue task-fn))
(defn- add-task [task-fn] (add-event* task-fn))

(defn with-jbox2d*
  "execute f in the jbox2d thread, wait for its result and return it.  f is
  called with one arg: tick.  Waits no more than 10
  seconds for the result and throws an Exception on timeout.  If f throws an
  exception, it is catched in the jbox2d thread and is re-thrown in the
  calling thread."
  [f]
  (let [result-queue (ArrayBlockingQueue. 1)]
    (add-task (fn [tick]
                (let [task-result (try (f tick)
                                       ;; don't interrupt the jbox2d thread
                                       (catch Exception e e))]
                  (if (isa? (class task-result) Exception)
                    (.add result-queue task-result)
                    (.add result-queue [task-result]))))) ;; wrap result in vector, to be able to return nil
    (if-let [ret (.poll result-queue 10 (TimeUnit/SECONDS))]
      (if (vector? ret)
        (first ret)
        (throw ret))
      (throw (Exception. "with-jbox2d* timed out.")))))

(defmacro with-jbox2d
  "Execute body within the jbox2ds thread.
  Captures 'tick'. See the documentation of
  with-jbox2d* for more info."
  [& body]
  `(with-jbox2d* (fn [~'tick] ~@body)))

(defmacro with-jbox2d-a 
  "Executes body in the jbox2d threadCaptures 'tick'. In contrast to with-world,
  does not wait for the result of body."
  [& body]
  `(add-event* (fn [~'tick] ~@body)))

;;(defn jbox2d-tasks
;;  "jbox2d-loop: runs one-time tasks on world, e.g. adding bodies.  Use the
;;  with-jbox2d macro to safely execute tasks within the jbox2d thread."
;;  [tick]
;;  (doseq [t (take-while identity (repeatedly #(.poll #^ConcurrentLinkedQueue jbox2d-tasks-queue)))]
;;    (t tick)))

;; contact-events

(defn contact-dispatch [body0, body1, _, _]
  [(body-userdata body0 :type)
   (body-userdata body1 :type)])

(defmulti contact
  contact-dispatch
  :default nil)

(defmethod contact nil [b0 b1 t cp])

(defn call-contact-multimethod [contact-point contact-keyword]
  (let [b0 (.getBody (.shape1 #^ContactPoint contact-point))
        b1 (.getBody (.shape2 #^ContactPoint contact-point))
        t0 (body-userdata b0 :type)
        t1 (body-userdata b1 :type)]
    (if (and t0 t1)
      (if (< 0 (.compareTo t0 t1))
        (contact b1 b0 contact-keyword contact-point)
        (contact b0 b1 contact-keyword contact-point))
      (contact b0 b1 contact-keyword contact-point))))

(defn register-contact-multimethod
  "Register the contact multimethod with the jbox2d simulation so that
 the contact multimethod is called for each contact between two bodies."
  [world]
  (let [cl (proxy [ContactListener] []
             (add [cp] (call-contact-multimethod cp :add))
             (persist [cp] (call-contact-multimethod cp :persist))
             (remove [cp] (call-contact-multimethod cp :remove))
             (result [_]))]
    (.setContactListener world cl)))


;; central gravity generating device

(defn central-gravity
  "Apply a force directed to the point p to all dynamic bodies in *world*.
  p defaults to [0 0]."
  ([mass] (central-gravity [0 0] mass))
  ([p mass]
     (iter (let p-vec (vec2 p))
           (let min-length-squared Float/MIN_VALUE) ;; minimal distance between grav-center and body, below this, no force is applied
           (for #^Body body call .getNext on (.getBodyList *world*))
           (do (when (not (.isStatic body))
                 (let [r-vec (.sub p-vec (.getWorldCenter body));; gravity-force vector (from body to mass)
                       ;; simple gravity function: f = (/ (* m1 m2) (sqr r))
                       r2 (max min-length-squared (.lengthSquared r-vec))
                       gravity-force (if (< r2 min-length-squared)
                                       0;; do not apply gforce
                                       (/ (* mass (.getMass body)) r2))
                       r-norm (doto r-vec (.normalize)) ;; alters r-vec
                       force-vec (.mul r-norm gravity-force)]
                   (.applyForce body
                                force-vec
                                (.getWorldCenter body))))
               (recur)))))


;; unlimited space illusion

(defn wrap-world [[x0 y0 x1 y1]]
  (iter (for #^Body b call .getNext on (.getBodyList *world*))
        (do (let [pos (.getPosition b)
                  px (.x pos)
                  py (.y pos)
                  newx (cond (< px x0) x1
                             (< x1 px) x0)
                  newy (cond (< py y0) y1
                             (< y1 py) y0)]
              (when (or newx newy)
                (.setXForm b
                           (vec2 (or newx px) (or newy py))
                           (.getAngle b)))
              (recur)))))

;; destroying bodies
;; uses body userdata to decide whether body should be destroyed
;; or has already been destroyed

(defn destroy-body [world body]
  (when (not (body-userdata body :destroyed)) (.destroyBody *world* body)))