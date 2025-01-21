(ns mysyx.core
  "Agent coordination for state managment")

(defn assign
  "Assign an agent a value.
  Any asignment triggers dependencies to be `::stale`
  - If the assignment itself is for `::stale`..
  Then this also triggers `::stale` dependencies
  - If it was already `::stale`,
  then the dependencies should already be `::stale` as well.
  So it doesn't need to propogate
  - If it's assigning to the previous value,
  Also don't propogate anything"
  [myagent
   assignment]
  (if (not= @myagent    ;; don't run if there is no update
            assignment) ;; (ex: already `::stale`)
    (do (send myagent   ;; otherwise first make `::stale`
              (fn dummy-func1
                [_]
                ::stale))
        (await myagent) ;; wait for `::stale` to propogate
        (send myagent   ;; set to new value and values will propogate (TODO: Should wait after this??)
              (fn dummy-func2
                [_]
                assignment)))))

(defn rule
  "The rule updates one agent `myagent`
  It tracks all the dependencies (agents)
  If any of them turn `::stale` then something upstream is being updated
  So we need to set `myagent` to be `::stale` too
  TODO: Add a 4-arg overload with a `str` to print on trigger
  TODO: Multiple rules can update one agent.
  - When you update an agent's rule it should remove the previous one"
  [myagent
   tracked-agents-vec
   mission] ;; TODO: Check non-zero amount of tracked agents
  (let [action (fn [call-key  ;; unique key 
                    my-agent  ;; the agent     ;
                    old-stat  ;; old-state
                    new-stat ];; new-state
                 ;; TODO  optionally pass these to the supplied `mission`?
                 (if (->> tracked-agents-vec
                          (some #(-> %
                                     deref
                                     (= ::stale))))
                   (if (not= old-stat
                             ::stale)
                     (send myagent
                           (fn dummy-func3
                             [_]
                             ::stale))
                     ;; else - already `::stale` so it's propogated
                     )
                   ;; else - all inputs are updated - so re-eval
                   (send myagent
                         mission
                         (->> tracked-agents-vec
                              (mapv deref)))))]
    (run! #(let [random-key (keyword (str (rand)))]
             (do (println (str "ADDING.."
                               "\nwatch func:\t"
                               action
                               "\nwith key:\t"
                               random-key
                               "\nto agent:\t"
                               %
                               "\nwho's value is:"
                               (deref %)))
                 (add-watch %
                            random-key
                            action)))
          tracked-agents-vec)))
