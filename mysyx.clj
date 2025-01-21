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
    (do (println (str "SET STALE: "
                      myagent
                      "\n and propogate...\n"))
        (send myagent   ;; otherwise first make `::stale`
              (fn dummy-func1
                [_]
                ::stale))
        (await myagent) ;; wait for `::stale` to propogate
        (println (str "SENDING:"
                      "\nfunc/val:\t"
                      assignment
                      "\nto agent:\t"
                      myagent
                      \newline))
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
                 (println (str "ACTION"
                               "\ntriggering on agent: "
                               myagent
                               "\nrunning function: "
                               mission
                               "\nfrom state:"
                               old-stat
                               "\nto  state:"
                               new-stat
                               \newline))
                 (if (->> tracked-agents-vec
                          (some #(-> %
                                     deref
                                     (= ::stale))))
                   (do (println "At least one dependency is stale!")
                       (if (not= old-stat
                                 ::stale)
                         (do (println (str "SET DEP STALE: "
                                           myagent))
                             (send myagent
                                   (fn dummy-func3
                                     [_]
                                     ::stale)))
                         ;; else - already `::stale` so it's propogated
                         ))
                   ;; else - all inputs are updated - so reeval
                   (do (println (str "Sending off func: "
                                     mission
                                     "\nwith the num args: "
                                     (count tracked-agents-vec)))
                       (send myagent
                             mission
                             (->> tracked-agents-vec
                                  (mapv deref))))))]
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

;; Orbital Parameters Example
(use 'clojure.math)
(add-libs {'generateme/fastmath {:mvn/version "3.0.0-alpha1"}
           'kxygk/quickthing    {:local/root "../quickthing"}})
(use '[fastmath.core :only (radians)])
(require 'thi.ng.geom.viz.core)
(require 'quickthing)

;; Obliquity
(def obliq-median-deg
  (agent (radians 23.320556)))

(def obliq-freqs
  (agent [{:amplitude -2462.22
           :freq-arcs 31.609970 ;; change per year in arcseconds
           :phase-deg 251.9}
          {:amplitude -857.32
           :freq-arcs 32.620499
           :phase-deg 280.83}
          {:amplitude -629.32
           :freq-arcs 24.172195
           :phase-deg 128.30}]))

(def time-steps
  (agent (range 0
                800000
                1000)))

(def obliq-epsilons
  (agent nil))

(defn sum-of-freqs
  [current-agent-state
   [constant-factor
    freq-factors
    time-steps]]
  (println (str "Calculating new polynomial values:"
                "\ncurrent-agent-state"
                current-agent-state
                "\nconstant-factor"
                constant-factor
                "\nfreq-factors"
                freq-factors
                "\ntime-steps count"
                (count time-steps)
                \newline))
  (let [in-rads (->> freq-factors
                     (mapv (fn [{:keys [amplitude
                                        freq-arcs
                                        phase-deg]}]
                             {:amplitude amplitude
                              :freq--rad (radians (/ freq-arcs
                                                     3600))
                              :phase-rad (radians phase-deg)})))]
    (mapv vector
          time-steps
          (->> time-steps
               (mapv (fn [time-step]
                       (->> in-rads
                            (mapv (fn [{:keys [amplitude
                                               freq--rad
                                               phase-rad]}]
                                    (* amplitude
                                       (cos (+ phase-rad
                                               (* time-step
                                                  freq--rad))))))
                            (reduce +))))
               (mapv (partial +
                              constant-factor))))))
#_
(calc-obliq-vals nil
                 @obliq-median-deg
                 @obliq-freqs
                 @time-steps)

(rule obliq-epsilons
      [obliq-median-deg
       obliq-freqs
       time-steps]
      sum-of-freqs)

(assign obliq-median-deg
        (radians 23.320555))

(identity @obliq-freqs)
;; => [{:amplitude -2462.22, :freq-arcs 31.60997, :phase-deg 251.9}
;;     {:amplitude -857.32, :freq-arcs 32.620499, :phase-deg 280.83}
;;     {:amplitude -629.32, :freq-arcs 24.172195, :phase-deg 128.3}]
(identity @obliq-median-deg)
;; => 23.320556
(identity @obliq-epsilons)
;; => nil


(defn draw-svg-line
  [current-agent-state
   [xy-pair-seq
    svg-filename]]
  (println (str "Current Agent State: "
                current-agent-state
                "Huge Data Vec: "
                xy-pair-seq))
  (let [plot (-> xy-pair-seq
                 quickthing/no-axis
                 (update :data
                         #(into %
                                (quickthing/dashed-line xy-pair-seq))))]
    (let [plot-xml (-> plot
                       (thi.ng.geom.viz.core/svg-plot2d-cartesian)
                       quickthing/svg-wrap
                       quickthing/svg2xml)]
      (spit svg-filename
            plot-xml)
      plot)))
#_
(draw-svg-line nil
               [@obliq-epsilons])
  
(def obliq-svg-filename
  (agent "obliquity.svg"))

(def obliq-svg
  (agent nil))

(rule obliq-svg
      [obliq-epsilons
       obliq-svg-filename]
      draw-svg-line)
#_#_
(assign obliq-median-deg
        28.320555)
(assign obliq-freqs
        [{:amplitude -2462.22
          :freq-arcs 31.609970 ;; change per year in arcseconds
          :phase-deg 251.9}
         {:amplitude -857.32
          :freq-arcs 32.620499
          :phase-deg 280.83}
         {:amplitude -629.32
          :freq-arcs 24.172195
          :phase-deg 128.30}])


;; Eccentricity

(def eccen-median-deg
  (agent 0.028707))

(def eccen-freqs
  (agent [{:amplitude 0.01102940
           :freq-arcs 3.3138886
           :phase-deg 165.16}
          {:amplitude -0.00873296
           :freq-arcs 13.650058
           :phase-deg 279.68}
          {:amplitude -0.00749255
           :freq-arcs 10.511172
           :phase-deg 114.51}]))

(def eccen-e
  (agent nil))

(rule eccen-e
      [eccen-median-deg
       eccen-freqs
       time-steps]
      sum-of-freqs)
#_
(sum-of-freqs nil
                 [@eccen-median-deg
                  @eccen-freqs
                  @time-steps])

(def eccen-svg-filename
  (agent "eccen.svg"))

(def eccen-svg
  (agent nil))

(rule eccen-svg
      [eccen-e
       eccen-svg-filename]
      draw-svg-line)


(assign eccen-median-deg
        0.028708)


;; Precession of the Equinox

(def pr-eq-median-deg
  (agent 0.1))

(def pr-eq-freqs
  (agent [{:amplitude 0.0186080
           :freq-arcs 54.646484
           :phase-deg 32.01}
          {:amplitude 0.0162752
           :freq-arcs 57.785370
           :phase-deg 197.18}
          {:amplitude -0.0130066
           :freq-arcs 68.296539
           :phase-deg 311.69}]))

(def pr-eq-esinw
  (agent nil))

(rule pr-eq-esinw
      [pr-eq-median-deg
       pr-eq-freqs
       time-steps]
      sum-of-freqs)
#_
(sum-of-freqs nil
                 [@eccen-median-deg
                  @eccen-freqs
                  @time-steps])

(def pr-eq-svg-filename
  (agent "pr-eq.svg"))

(def pr-eq-svg
  (agent nil))

(rule pr-eq-svg
      [pr-eq-esinw
       pr-eq-svg-filename]
      draw-svg-line)

(assign pr-eq-median-deg
        0.0)


;; Combined plot to show how one acts as an envolope of another

(def composite-filename
  (agent nil))

(defn
  two-line-plot
  [current-state
   [xy-pair-A-seq
    xy-pair-B-seq
    svg-filename]]
  (let [plot (-> xy-pair-A-seq
                 quickthing/no-axis
                 (update :data
                         #(into %
                                (quickthing/dashed-line xy-pair-A-seq)))
                 (update :data
                         #(into %
                                (quickthing/dashed-line xy-pair-B-seq
                                                        {:attribs {:stroke "red"}}))))]
    (let [plot-xml (-> plot
                       (thi.ng.geom.viz.core/svg-plot2d-cartesian)
                       quickthing/svg-wrap
                       quickthing/svg2xml)]
      (spit svg-filename
            plot-xml)
      plot)))

(def composite-plot
  (agent nil))

(rule composite-plot
      [pr-eq-esinw
       eccen-e
       composite-filename]
      two-line-plot)
       
(assign composite-filename
        "composite.svg")


(assign time-steps
        (range 0
               800000
               1000))

(assign eccen-freqs
        [{:amplitude 0.01102940
          :freq-arcs 3.3138886
          :phase-deg 165.16}
         {:amplitude -0.00873296
          :freq-arcs 13.650058
          :phase-deg 279.68}
         {:amplitude -0.00749255
          :freq-arcs 10.511172
          :phase-deg 114.51}
         {:amplitude 0.00672394
          :freq-arcs 13.013341
          :phase-deg 291.57}
         {:amplitude 0.00581229
          :freq-arcs 9.874455
          :phase-deg 126.41}
         {:amplitude -0.00470066
          :freq-arcs 0.636717
          :phase-deg 348.10}])


(assign pr-eq-freqs
        [{:amplitude 0.0186080
          :freq-arcs 54.646484
          :phase-deg 32.01}
         {:amplitude 0.0162752
          :freq-arcs 57.785370
          :phase-deg 197.18}
         {:amplitude -0.0130066
          :freq-arcs 68.296539
          :phase-deg 311.69}
         {:amplitude 0.0098883
          :freq-arcs 67.659821
          :phase-deg 323.59}])

(defn recall
  "WIP
  Unclear how to hang and wait for a `::stale` to change.
  Currently just spins the main thread...
  which is not great
  ..
  You could `await` the main thread.
  But you may end up waiting for unrelated work to finish.
  Also not great"
  [myagent]
  (let [myagent-value (deref myagent)]
    (if (= ::stale
           myagent-value)
      (do (await myagent)
          (recur myagent))
      myagent-value)))
