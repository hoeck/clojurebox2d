
(ns hoeck.clojurebox2d.statemachine)

;; functions and macros to define and use state machines

(defn- make-state-method-argvec
  "(make-state-method-argvec [sm :a :b msg-arg]) -> [{:keys [a b]} current-state-machine msg1234 msg-arg])"
  [argvec]
  (let [default-argvec ['current-state-machine (gensym 'msg)]
        [argkeys msg-args] (split-with keyword? argvec)]
    (vec (concat `[{:keys [~@(map #(symbol (name %)) argkeys)] :as ~'state-map}]
                 default-argvec
                 msg-args))))

(defmacro defsmethod
  "Define a state machine transition method which is triggered on the
  given message-key.
  Captures `current-state-machine' and binds it to the state-machine function
  we're running in.
  `state-method-args' is a simplified parameter list, a number of keys to which
  current state-map values are bound followed by optional arguments from the
  message.
  The return value of defsmethod is merged with the current state of the state-machine, so
  it should be either nil or a hashmap."
  [name message-key state-method-args & body]
  `(defmethod ~name ~message-key ~(make-state-method-argvec state-method-args)     
     ~@body))

(defmacro def-state-machine
  "Define name to be a state-machine function. Use defsmethod to define
  state-machine-functions and make-state-machine to generate a state-machine."
  [name]
  `(defmulti ~name (fn [state# state-machine# msg# & msg-args#] msg#)))

(defn make-state-machine
  "Makes a state machine from the given state-machine-function
  (a multimethod declared with def-state-machine).
    (let [sm (make-state-machine sfn)]
      (sm) -> returns the state-agent
      (sm :msg args ...)) -> sends sfn to agent: (send sfn state this msg args ..), 
                             merges back the results.
  Additional params to make-state-machine are messages that are executed right after
  the creatoin of the state machine."
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

(defn throw-statemachine-errors
  "Throw the first errors of the statemachine sm in the current Thread or
  return nil."
  [sm]
  (let [a (sm)]
    (when-let [ae (agent-errors a)]
      (throw (first ae)))))

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
