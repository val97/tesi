;; gorilla-repl.fileformat = 1

;; @@
;;;Consider the decision problem as to whether or not to go ahead with a fund-raising garden party.If we go ahead with the party and it subsequently rains, then we will lose money (since very few people will show up); on the other hand, if we don’t go ahead with the party and it doesn’t rain we’re free to go and do something else fun.
;;;An extension of the Party problem is that if we decide not to go ahead with the party, we have the opportunity to visit a friend. However, we’re not sure if this friend will be in. The question is should we still go ahead with the party?
(ns party-or-not
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit 
         [inference :only [collect-by]]]))
(defquery party-rain[]
  (let [ rain-prior (categorical {:rain 0.6
                                  :norain 0.4}) 
         rain-state (sample rain-prior)]
        
    rain-state))




(defquery party-friend[rain]
  (let [  
         rain-prior (cond(= rain true)
                         (flip 0.99)
                         (= rain false)
                         (flip 0.6)) 
         rain-state (sample rain-prior)
         friend-state (if(= rain-state true)
                        (sample (categorical
                           		{:in 0.8
                                 :out 0.2}))
                        (sample (categorical
                                  {:in 0.1
                                 :out 0.9})))
    ]
		;friend-state (sample friend-prior)
    (observe rain-prior rain)
    friend-state))
(defn utility-rain
  [action rain-state]
  (if (= action :doParty) 
    (get  {:rain -100 
           :norain 500}
          rain-state)
   
    (get  {:rain 0   
           :norain 50}   
          rain-state)))

(defn utility-friend
  [action friend-state]
  (if(= action :doVisit)
    (get  {:in -200 
           :out -100}
          friend-state)
  
    (get  {:in 0   
           :out 0}   
          friend-state)))
(defn posterior-estimate-rain [N] 
  (->> (doquery :smc party-rain [] 
                :number-of-particles 100)
     (take N)
     (map #(vector 
             (:result %)
             (:log-weight %)))
     (s/empirical-distribution)))

;;;defining the probability that it's been raining knowing that the party is in process
(def N 5000)
(def prob-rain (posterior-estimate-rain N ))
prob-rain

(defn posterior-estimate-friend [N rain] 
  (->> (doquery :smc party-friend [rain] 
                :number-of-particles 100)
     (take N)
     (map #(vector 
             (:result %)
             (:log-weight %)))
     (s/empirical-distribution)))

;;;defining the probability that it's been raining knowing that the party is in process
(def N 5000)
(def prob-friend (posterior-estimate-friend N true ))

prob-friend



(defn expected-utility-of-single-action [posterior action]
  
  (if(or (= action :doParty)(= action :dontParty)) 
  (reduce + (map #(* (utility-rain action (key %)) 
                     (val %)) 
                 posterior))
  (reduce + (map #(* (utility-friend action (key %)) 
                     (val %)) 
                 posterior)))
  )


(defn expected-utility-rain [N actions]
  (let [posterior (posterior-estimate-rain N )]
  (apply 
                        assoc 
                        {}
                        (interleave 
                          actions 
                          (map 
                            (partial 
                              expected-utility-of-single-action 
                              posterior)
                            actions)))))

(defn expected-utility-friends [N actions observation]
  (let [posterior (posterior-estimate-friend N observation)]
  (apply 
                        assoc 
                        {}
                        (interleave 
                          actions 
                          (map 
                            (partial 
                              expected-utility-of-single-action 
                              posterior)
                            actions))))
    )
  

(expected-utility-friends  N [:doVisit :dontVisit] false)
;(map (partial expected-utility N [:doVisit :dontVisit]) [false true])
(expected-utility-rain N [:doParty :dontParty]) 

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/party-rain</span>","value":"#'party-or-not/party-rain"}],"value":"[nil,#'party-or-not/party-rain]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/party-friend</span>","value":"#'party-or-not/party-friend"}],"value":"[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/utility-rain</span>","value":"#'party-or-not/utility-rain"}],"value":"[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/utility-friend</span>","value":"#'party-or-not/utility-friend"}],"value":"[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/posterior-estimate-rain</span>","value":"#'party-or-not/posterior-estimate-rain"}],"value":"[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/N</span>","value":"#'party-or-not/N"}],"value":"[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/prob-rain</span>","value":"#'party-or-not/prob-rain"}],"value":"[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain]"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:norain</span>","value":":norain"},{"type":"html","content":"<span class='clj-double'>0.3967999999999828</span>","value":"0.3967999999999828"}],"value":"[:norain 0.3967999999999828]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:rain</span>","value":":rain"},{"type":"html","content":"<span class='clj-double'>0.6031999999999601</span>","value":"0.6031999999999601"}],"value":"[:rain 0.6031999999999601]"}],"value":"{:norain 0.3967999999999828, :rain 0.6031999999999601}"}],"value":"[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/posterior-estimate-friend</span>","value":"#'party-or-not/posterior-estimate-friend"}],"value":"[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/N</span>","value":"#'party-or-not/N"}],"value":"[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/prob-friend</span>","value":"#'party-or-not/prob-friend"}],"value":"[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/prob-friend]"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:in</span>","value":":in"},{"type":"html","content":"<span class='clj-double'>0.7917999999999393</span>","value":"0.7917999999999393"}],"value":"[:in 0.7917999999999393]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:out</span>","value":":out"},{"type":"html","content":"<span class='clj-double'>0.2081999999999978</span>","value":"0.2081999999999978"}],"value":"[:out 0.2081999999999978]"}],"value":"{:in 0.7917999999999393, :out 0.2081999999999978}"}],"value":"[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/prob-friend],{:in 0.7917999999999393, :out 0.2081999999999978}]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility-of-single-action</span>","value":"#'party-or-not/expected-utility-of-single-action"}],"value":"[[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/prob-friend],{:in 0.7917999999999393, :out 0.2081999999999978}],#'party-or-not/expected-utility-of-single-action]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility-rain</span>","value":"#'party-or-not/expected-utility-rain"}],"value":"[[[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/prob-friend],{:in 0.7917999999999393, :out 0.2081999999999978}],#'party-or-not/expected-utility-of-single-action],#'party-or-not/expected-utility-rain]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility-friends</span>","value":"#'party-or-not/expected-utility-friends"}],"value":"[[[[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/prob-friend],{:in 0.7917999999999393, :out 0.2081999999999978}],#'party-or-not/expected-utility-of-single-action],#'party-or-not/expected-utility-rain],#'party-or-not/expected-utility-friends]"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:doVisit</span>","value":":doVisit"},{"type":"html","content":"<span class='clj-double'>-151.47999999999178</span>","value":"-151.47999999999178"}],"value":"[:doVisit -151.47999999999178]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontVisit</span>","value":":dontVisit"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"[:dontVisit 0.0]"}],"value":"{:doVisit -151.47999999999178, :dontVisit 0.0}"}],"value":"[[[[[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/prob-friend],{:in 0.7917999999999393, :out 0.2081999999999978}],#'party-or-not/expected-utility-of-single-action],#'party-or-not/expected-utility-rain],#'party-or-not/expected-utility-friends],{:doVisit -151.47999999999178, :dontVisit 0.0}]"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:doParty</span>","value":":doParty"},{"type":"html","content":"<span class='clj-double'>141.31999999999502</span>","value":"141.31999999999502"}],"value":"[:doParty 141.31999999999502]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontParty</span>","value":":dontParty"},{"type":"html","content":"<span class='clj-double'>20.109999999999108</span>","value":"20.109999999999108"}],"value":"[:dontParty 20.109999999999108]"}],"value":"{:doParty 141.31999999999502, :dontParty 20.109999999999108}"}],"value":"[[[[[[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:norain 0.3967999999999828, :rain 0.6031999999999601}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/prob-friend],{:in 0.7917999999999393, :out 0.2081999999999978}],#'party-or-not/expected-utility-of-single-action],#'party-or-not/expected-utility-rain],#'party-or-not/expected-utility-friends],{:doVisit -151.47999999999178, :dontVisit 0.0}],{:doParty 141.31999999999502, :dontParty 20.109999999999108}]"}
;; <=

;; @@

;; @@

;; @@

;; @@
