;; gorilla-repl.fileformat = 1

;; @@
;;;One morning Tracey leaves her house and realises that her grass is wet. Is it due to overnight rain or did she forget to turn off the sprinkler last night? Next she notices that the grass of her neighbour, Jack, is also wet. What's the probability that it has rained

(ns bayes-net-raining
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit
         [inference :only [collect-by]]]))


(defquery raining-b-net [t-wet-grass j-wet-grass]
  (let [raining  (sample(flip 0.4))

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

    raining))


(->> (doquery :smc raining-b-net [true false] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net-raining/raining-b-net</span>","value":"#'bayes-net-raining/raining-b-net"}],"value":"[nil,#'bayes-net-raining/raining-b-net]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"858dcb09-6272-4fab-a9f5-02c62c40b950","values":[{"x":false,"y":0.7873175655535822},{"x":true,"y":0.2126824344464177}]}],"marks":[{"type":"rect","from":{"data":"858dcb09-6272-4fab-a9f5-02c62c40b950"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"858dcb09-6272-4fab-a9f5-02c62c40b950","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"858dcb09-6272-4fab-a9f5-02c62c40b950","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"858dcb09-6272-4fab-a9f5-02c62c40b950\", :values ({:x false, :y 0.7873175655535822} {:x true, :y 0.2126824344464177})}], :marks [{:type \"rect\", :from {:data \"858dcb09-6272-4fab-a9f5-02c62c40b950\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"858dcb09-6272-4fab-a9f5-02c62c40b950\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"858dcb09-6272-4fab-a9f5-02c62c40b950\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[nil,#'bayes-net-raining/raining-b-net],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"858dcb09-6272-4fab-a9f5-02c62c40b950\", :values ({:x false, :y 0.7873175655535822} {:x true, :y 0.2126824344464177})}], :marks [{:type \"rect\", :from {:data \"858dcb09-6272-4fab-a9f5-02c62c40b950\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"858dcb09-6272-4fab-a9f5-02c62c40b950\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"858dcb09-6272-4fab-a9f5-02c62c40b950\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; @@


















;; @@
