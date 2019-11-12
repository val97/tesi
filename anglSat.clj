
(ns sat-net 

    (:require [gorilla-plot.core :as plot] 

        [anglican.stat :as s])

    (:use clojure.repl

        [anglican core runtime emit 

        [inference :only [collect-by]]]))

(defquery sat-bayes-net []

     (let [
 	u4 (sample(flip 0.5))
	u3 (sample(flip 0.5))
	u2 (sample(flip 0.5))
	u1 (sample(flip 0.5))
	c0 (cond(or 
		(= u1 true)
		(= u2 true)
		(= u3 true)
		)
	(sample(flip 1.0))) 
	c1 (cond(or 
		(= u1 false)
		(= u2 false)
		(= u3 true)
		)
	(sample(flip 1.0))) 
	c2 (cond(or 
		(= u2 true)
		(= u3 false)
		(= u4 true)
		)
	(sample(flip 1.0))) 
	d0 (cond 
		(= c0 true)
	
	(sample(flip 1.0))) 
	d1 (cond(and 
		(= d0 true)
		(= c1 true)
		)
	(sample(flip 1.0))) 
	y (cond(and 
		(= d1 true)
		(= c2 true)
		)
	(sample(flip 1.0))) 
]y))(->> (doquery :lmh sat-bayes-net [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))