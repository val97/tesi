(ns bayes-net
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit 
         [inference :only [collect-by]]]))

(defquery burglar-bayes-net [alarm radio]
  (let [earthquake (sample (flip 0.8))

        burglar (cond (and (= alarm true )
                           (= radio false)) 
                         (sample (flip 0.8))
                         (and (= radio true)
                              (= alarm true))
                         (sample (flip 0.2)))
        radio-dist (cond (= earthquake true) 
                             (flip 0.8)
                             (= earthquake false) 
                             (flip 0.2))
        alarm-dist (cond 
                         (and (= burglar true) 
                              (= earthquake true))  								  				       
                         (flip 0.99)
                         (and (= burglar false) 
                              (= earthquake false))
                         (flip 0.01)
                     	 (or  (= burglar true) 
                              (= earthquake true))
                         (flip 0.99))]
    (observe radio-dist radio)
    (observe alarm-dist alarm)

    burglar))

(->> (doquery :smc burglar-bayes-net [true false] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
