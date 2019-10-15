;; gorilla-repl.fileformat = 1

;; @@
(ns bayes-net-raining
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit 
         [inference :only [collect-by]]]))


(defquery raining-b-net [t-wet-grass j-wet-grass]
  (let [raining(cond(and (= t-wet-grass true)
                         (= j-wet-grass true))
                    	 (sample(flip 0.8))
                     (or(= t-wet-grass false)
                        (= j-wet-grass false))
                      (sample(flip 0.01)))

        sprinkler (sample (flip 0.3))
        jack (cond (= raining true)
             		(flip 0.9)
                   (= raining false)
                   	(flip 0.2))
        tracey(cond(and (= raining true) 
                       	(= sprinkler true))
                        (flip 0.99)
                   (and (= raining false) 
                        (= sprinkler false))
                        (flip 0.02)
                   (or  (= raining true) 
                        (= sprinkler true))
                        (flip 0.99))]
    (observe tracey t-wet-grass)
    (observe jack j-wet-grass)

    sprinkler))


(->> (doquery :smc raining-b-net [true false] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net-raining/raining-b-net</span>","value":"#'bayes-net-raining/raining-b-net"}],"value":"[nil,#'bayes-net-raining/raining-b-net]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"f2beeaec-db1c-4c94-9685-a07ff26183d9","values":[{"x":true,"y":0.9517590249580054},{"x":false,"y":0.048240975041994705}]}],"marks":[{"type":"rect","from":{"data":"f2beeaec-db1c-4c94-9685-a07ff26183d9"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"f2beeaec-db1c-4c94-9685-a07ff26183d9","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"f2beeaec-db1c-4c94-9685-a07ff26183d9","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"f2beeaec-db1c-4c94-9685-a07ff26183d9\", :values ({:x true, :y 0.9517590249580054} {:x false, :y 0.048240975041994705})}], :marks [{:type \"rect\", :from {:data \"f2beeaec-db1c-4c94-9685-a07ff26183d9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f2beeaec-db1c-4c94-9685-a07ff26183d9\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f2beeaec-db1c-4c94-9685-a07ff26183d9\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[nil,#'bayes-net-raining/raining-b-net],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"f2beeaec-db1c-4c94-9685-a07ff26183d9\", :values ({:x true, :y 0.9517590249580054} {:x false, :y 0.048240975041994705})}], :marks [{:type \"rect\", :from {:data \"f2beeaec-db1c-4c94-9685-a07ff26183d9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f2beeaec-db1c-4c94-9685-a07ff26183d9\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f2beeaec-db1c-4c94-9685-a07ff26183d9\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; @@


















;; @@
