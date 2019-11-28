
(ns anglsat.core
(:require
        [anglican.stat :as s])
(:use [anglican.core :refer [doquery]]

       [anglican runtime emit
       [inference :only [collect-by] ]]))
    (defm getPrior[u1 u2 u3 val1 val2 val3]
        (let[
            c (cond(or
		          (= u1 val1)
		          (= u2 val2)
		          (= u3 val3))
	          (sample(flip 1.0))
              (and(not= u1 val1)
                  (not= u2 val2)
                  (not= u3 val3))
    	      (sample(flip 0.0)))]c))
    (defm getY[u1 u2]
        (let[
            c (cond(and
                  (= u1 true)
                  (= u2 true))

              (sample(flip 1.0))
              (or(= u1 false)
                  (= u2 false))
              (sample(flip 0.0)))]c))
(defquery anglsat []

     (let [
 	u3 (sample(flip 0.5))
	u2 (sample(flip 0.5))
	u1 (sample(flip 0.5))
	c0 (getPrior u1 u2 u3 true true true)
	c1 (getPrior u1 u2 u3 true true false)
	c2 (getPrior u1 u2 u3 true false true)
	c3 (getPrior u1 u2 u3 true false false)
	c4 (getPrior u1 u2 u3 false true true)
	c5 (getPrior u1 u2 u3 false true false)
	c6 (getPrior u1 u2 u3 false false true)
	c7 (getPrior u1 u2 u3 false false false)
	d0(cond(= c0 true)
(sample(flip 1.0))
(= c0 false)
(sample(flip 0.0)))
	d1 (getY d0 c1 )
	d2 (getY d1 c2 )
	d3 (getY d2 c3 )
	d4 (getY d3 c4 )
	d5 (getY d4 c5 )
	d6 (getY d5 c6 )
	y (getY d6 c7 )
]y))(defn -main [& args]
  (println
   (->> (doquery :lmh anglsat []
                 :number-of-particles 1000)
        (take 10000)
        (map #(vector
               (:result %)
               (:log-weight %)))
 (s/empirical-distribution)
        )))
