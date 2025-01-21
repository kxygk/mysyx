(ns mysyx.example)

(use 'mysyx.core)

#_
(add-libs {'generateme/fastmath {:mvn/version "3.0.0-alpha1"}
           'kxygk/quickthing    {:local/root "../quickthing"}})
(use '[fastmath.core :only (radians)])
(require 'thi.ng.geom.viz.core)
(require 'quickthing)


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
  [constant-factor
   freq-factors
   time-steps]
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
                                       (clojure.math/cos (+ phase-rad
                                               (* time-step
                                                  freq--rad))))))
                            (reduce +))))
               (mapv (partial +
                              constant-factor))))))
#_
(sum-of-freqs @eccen-median-deg
              @eccen-freqs
              @time-steps)

(def eccen-e
  (agent nil))

(rule eccen-e
      [eccen-median-deg
       eccen-freqs
       time-steps]
      sum-of-freqs)

(defn draw-svg-line
  [xy-pair-seq
   svg-filename]
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
(assign eccen-svg-filename
        "eccentricity.svg")

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
(sum-of-freqs @eccen-median-deg
              @eccen-freqs
              @time-steps)

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



(def composite-filename
  (agent nil))

(defn two-line-plot
  [xy-pair-A-seq
   xy-pair-B-seq
   svg-filename]
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

#_
(assign time-steps
        (range 0
               800000
               100))

#_
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
          :phase-deg 348.10}])

#_
(assign time-steps
        (range 0
               800000
               1000))

#_
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

#_
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
