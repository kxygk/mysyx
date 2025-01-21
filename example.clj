(ns mysyx.example)

(using mysyx.core)



(def time-steps
  (agent (range 0
                800000
                1000)))
                
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

(defn sum-of-freqs
  [current-agent-state
   [constant-factor
    freq-factors
    time-steps]]
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

(def eccen-e
  (agent nil))

(rule eccen-e
      [eccen-median-deg
       eccen-freqs
       time-steps]
      sum-of-freqs)

(defn draw-svg-line
  [current-agent-state
   [xy-pair-seq
    svg-filename]]
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


(def eccen-svg-filename
  (agent "eccen.svg"))

(def eccen-svg
  (agent nil))

(rule eccen-svg
      [eccen-e
       eccen-svg-filename]
      draw-svg-line)

#_
(assign eccen-svg-filenam
        "eccentricity.svg")
        
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
               100))


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
         {:amplitude -0.00672394
          :freq-arcs 13.013341
          :phase-deg 291.57}
         {:amplitude -0.00581229
          :freq-arcs 9.874455
          :phase-deg 126.41}
         {:amplitude -0.00470066
          :freq-arcs 0.636717
          :phase-deg 348.10}]))
