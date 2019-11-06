;; gorilla-repl.fileformat = 1

;; @@
(ns bayes-net
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit 
         [inference :only [collect-by]]]))

(defquery burglar-bayes-net [alarm radio]
  (let [earthquake (sample (flip 0.4))

        burglar    (sample (flip 0.2)))
        radio-dist (cond (= earthquake true) 
                             (flip 0.7)
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
                         (flip 0.70))]
    (observe radio-dist radio)
    (observe alarm-dist alarm)

    burglar))
;using Lightweight Metropolis-Hastings inference function

(->> (doquery :lmh burglar-bayes-net [true true] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/burglar-bayes-net</span>","value":"#'bayes-net/burglar-bayes-net"}],"value":"[nil,#'bayes-net/burglar-bayes-net]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"0ec7f51a-3447-42b5-83c6-85fb44cd8266","values":[{"x":false,"y":0.6892000000000004},{"x":true,"y":0.31079999999999963}]}],"marks":[{"type":"rect","from":{"data":"0ec7f51a-3447-42b5-83c6-85fb44cd8266"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"0ec7f51a-3447-42b5-83c6-85fb44cd8266","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"0ec7f51a-3447-42b5-83c6-85fb44cd8266","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\", :values ({:x false, :y 0.6892000000000004} {:x true, :y 0.31079999999999963})}], :marks [{:type \"rect\", :from {:data \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[nil,#'bayes-net/burglar-bayes-net],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\", :values ({:x false, :y 0.6892000000000004} {:x true, :y 0.31079999999999963})}], :marks [{:type \"rect\", :from {:data \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0ec7f51a-3447-42b5-83c6-85fb44cd8266\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; @@

;; @@
