
(ns hoeck.clojurebox2d.statemachine)

(defn- make-state-method-argvec
  "(make-state-method-argvec [sm :a :b msg-arg]) -> [{:keys [a b]} state-machine msg1234 msg-arg])"
  [argvec]
  (let [default-argvec ['state-machine (gensym 'msg)]
        [argkeys msg-args] (split-with keyword? argvec)]
    (vec (concat `[{:keys [~@(map #(symbol (name %)) argkeys)] :as ~'state-map}]
                 default-argvec
                 msg-args))))

(defmacro defsmethod
  "Define a state machine transition method which is triggered on the
  given message-key signal.
  Captures `state-machine' and binds it to the state-machine function
  we're running in.
  state-method-args is a simplified parameter list, a number of keys to which
  current state-map values are bound followed by optional arguments from the
  message."
  [name message-key state-method-args & body]
  `(defmethod ~name ~message-key ~(make-state-method-argvec state-method-args)     
     ~@body))

(defmacro def-state-machine [name]
  `(defmulti ~name (fn [state# state-machine# msg# & msg-args#] msg#)))

(defn make-state-machine 
  ([state-machine-fn]
     (let [state-agent (agent {})]
       (fn state-machine
         ([] state-agent)
         ([msg]
            (send state-agent
                  #(try (merge % (state-machine-fn % state-machine msg))
                        (catch Exception e
                          (throw (Exception. (format "Exception when calling state function with: %s" msg)
                                             e))))))
         ([msg arg]
            (send state-agent 
                  #(try (merge % (state-machine-fn % state-machine msg arg))
                        (catch Exception e 
                          (throw (Exception. (format "Exception when calling state function with: %s %s" msg arg)
                                             e))))))
         ([msg arg & more-args]
            (send state-agent 
                  #(try (merge % (apply state-machine-fn % state-machine msg arg more-args))
                        (catch Exception e
                          (throw (Exception. (format "Exception when calling state function with: %s %s" msg (cons arg more-args)))))))))))
  ([state-machine-fn & init]
     (let [smfn (make-state-machine state-machine-fn)]
       (apply smfn init)
       smfn)))

(defn make-debugging-state-machine  
  [debug-atom state-machine-fn & rest]
  (swap! debug-atom (constantly []))
  (apply make-state-machine #(do (swap! debug-atom conj %&) 
                                 (apply state-machine-fn %&))
         rest))

(defn print-statemachine-errors [sm]
  (let [a (sm)]
    (when-let [ae (agent-errors a)]
      (.printStackTrace (first ae)))))

(comment 
  ;; example
    
  (def-state-machine example-state)
  
  (defsmethod example-state :reset []
    {:state :init
     :hp 10
     :bullets 10})

  (defsmethod example-state :shoot [:bullets]
    {:bullets (dec bullets)})

  (let [sm (make-state-machine example-state :reset)]
    [@(sm)
     (sm :shoot)
     @(sm)])
)
