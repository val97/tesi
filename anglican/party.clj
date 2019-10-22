;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
;;;Consider the decision problem as to whether or not to go ahead with a fund-raising garden party.If we go ahead with the party and it subsequently rains, then we will lose money (since very few people will show up); on the other hand, if we don’t go ahead with the party and it doesn’t rain we’re free to go and do something else fun.
;;;An extension of the Party problem is that if we decide not to go ahead with the party, we have the opportunity to visit a friend. However, we’re not sure if this friend will be in. The question is should we still go ahead with the party?

;;IN THIS EXAMPLE THERE IS NOT INFERENCE
;; @@

;; @@
(ns party-or-not
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit 
         [inference :only [collect-by]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defquery party[]
  (let [ rain-prior (categorical {:rain 0.6
                                  :norain 0.4}) 
         rain-state (sample rain-prior)
         ]
        
    rain-state))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/party</span>","value":"#'party-or-not/party"}
;; <=

;; @@
(defn utility 
  [action rain-state]
  (if (= action :doParty) 
    (get  {:rain -100 
           :norain 500}
          rain-state)
   
    (get  {:rain 0   
           :norain 50}   
          rain-state)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/utility</span>","value":"#'party-or-not/utility"}
;; <=

;; @@
(defn posterior-estimate [N ] 
  (->> (doquery :smc party [] 
                :number-of-particles 100)
     (take N)
     (map #(vector 
             (:result %)
             (:log-weight %)))
     (s/empirical-distribution)))

;;;defining the probability that it's been raining
(def N 5000)
(posterior-estimate N )


(defn expected-utility-of-action [posterior action]
  (reduce + (map #(* (utility action (key %)) 
                     (val %)) 
                 posterior)))

(expected-utility-of-action (posterior-estimate N ) :dontParty)


;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/posterior-estimate</span>","value":"#'party-or-not/posterior-estimate"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/N</span>","value":"#'party-or-not/N"}],"value":"[#'party-or-not/posterior-estimate,#'party-or-not/N]"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:rain</span>","value":":rain"},{"type":"html","content":"<span class='clj-double'>0.5921999999999613</span>","value":"0.5921999999999613"}],"value":"[:rain 0.5921999999999613]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:norain</span>","value":":norain"},{"type":"html","content":"<span class='clj-double'>0.40779999999998157</span>","value":"0.40779999999998157"}],"value":"[:norain 0.40779999999998157]"}],"value":"{:rain 0.5921999999999613, :norain 0.40779999999998157}"}],"value":"[[#'party-or-not/posterior-estimate,#'party-or-not/N],{:rain 0.5921999999999613, :norain 0.40779999999998157}]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility-of-action</span>","value":"#'party-or-not/expected-utility-of-action"}],"value":"[[[#'party-or-not/posterior-estimate,#'party-or-not/N],{:rain 0.5921999999999613, :norain 0.40779999999998157}],#'party-or-not/expected-utility-of-action]"},{"type":"html","content":"<span class='clj-double'>20.049999999999116</span>","value":"20.049999999999116"}],"value":"[[[[#'party-or-not/posterior-estimate,#'party-or-not/N],{:rain 0.5921999999999613, :norain 0.40779999999998157}],#'party-or-not/expected-utility-of-action],20.049999999999116]"}
;; <=

;; @@
(defn expected-utility [N actions ]
  (let [posterior (posterior-estimate N )]
  (apply 
                        assoc 
                        {}
                        (interleave 
                          actions 
                          (map 
                            (partial 
                              expected-utility-of-action 
                              posterior)
                            actions)))))

(expected-utility N [:doParty :dontParty]  )

(key (apply max-key val (set( expected-utility N [:doParty :dontParty]) )))
(key (apply max-key val {:a 3 :b 7 :c 9}))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility</span>","value":"#'party-or-not/expected-utility"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:doParty</span>","value":":doParty"},{"type":"html","content":"<span class='clj-double'>141.31999999999502</span>","value":"141.31999999999502"}],"value":"[:doParty 141.31999999999502]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontParty</span>","value":":dontParty"},{"type":"html","content":"<span class='clj-double'>20.109999999999108</span>","value":"20.109999999999108"}],"value":"[:dontParty 20.109999999999108]"}],"value":"{:doParty 141.31999999999502, :dontParty 20.109999999999108}"}],"value":"[#'party-or-not/expected-utility,{:doParty 141.31999999999502, :dontParty 20.109999999999108}]"},{"type":"html","content":"<span class='clj-keyword'>:doParty</span>","value":":doParty"}],"value":"[[#'party-or-not/expected-utility,{:doParty 141.31999999999502, :dontParty 20.109999999999108}],:doParty]"},{"type":"html","content":"<span class='clj-keyword'>:c</span>","value":":c"}],"value":"[[[#'party-or-not/expected-utility,{:doParty 141.31999999999502, :dontParty 20.109999999999108}],:doParty],:c]"}
;; <=

;; @@
(defn make-rational-policy 
  [N actions]
  (let [expected ( expected-utility N actions)]
    (key (apply max-key val(set expected)))
   
   ))

(make-rational-policy N [:doParty :dontParty] )
(set( expected-utility N [:doParty :dontParty]))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/make-rational-policy</span>","value":"#'party-or-not/make-rational-policy"},{"type":"html","content":"<span class='clj-keyword'>:doParty</span>","value":":doParty"}],"value":"[#'party-or-not/make-rational-policy,:doParty]"},{"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontParty</span>","value":":dontParty"},{"type":"html","content":"<span class='clj-double'>19.789999999999143</span>","value":"19.789999999999143"}],"value":"[:dontParty 19.789999999999143]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:doParty</span>","value":":doParty"},{"type":"html","content":"<span class='clj-double'>137.47999999999544</span>","value":"137.47999999999544"}],"value":"[:doParty 137.47999999999544]"}],"value":"#{[:dontParty 19.789999999999143] [:doParty 137.47999999999544]}"}],"value":"[[#'party-or-not/make-rational-policy,:doParty],#{[:dontParty 19.789999999999143] [:doParty 137.47999999999544]}]"}
;; <=

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@
