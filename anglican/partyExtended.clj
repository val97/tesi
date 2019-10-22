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

;viene definito il modello per la calcolare la probabilità che piova(variabile indip.) e che il nostro amico sia a casa(var dipendente dalla pioggia)
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
    (observe rain-prior rain)			; viene fatta inferenza conoscendo lo stato di rain 
    friend-state))

;Vengono definite le utility per ogni tipo di azione
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

; P(rain)
(defn posterior-estimate-rain [N] 
  (->> (doquery :smc party-rain [] 
                :number-of-particles 100)
     (take N)
     (map #(vector 
             (:result %)
             (:log-weight %)))
     (s/empirical-distribution)))

(def N 5000)
(def prob-rain (posterior-estimate-rain N ))
prob-rain

;P(Friend in|rain) and P(friend out|rain)
(defn posterior-estimate-friend [N rain] 
  (->> (doquery :smc party-friend [rain] 
                :number-of-particles 100)
     (take N)
     (map #(vector 
             (:result %)
             (:log-weight %)))
     (s/empirical-distribution)))

(def N 5000)
(def friend (map(partial posterior-estimate-friend N )[true false]))
friend
(def prob-friend ( posterior-estimate-friend N true ))



;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/party-rain</span>","value":"#'party-or-not/party-rain"}],"value":"[nil,#'party-or-not/party-rain]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/party-friend</span>","value":"#'party-or-not/party-friend"}],"value":"[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/utility-rain</span>","value":"#'party-or-not/utility-rain"}],"value":"[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/utility-friend</span>","value":"#'party-or-not/utility-friend"}],"value":"[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/posterior-estimate-rain</span>","value":"#'party-or-not/posterior-estimate-rain"}],"value":"[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/N</span>","value":"#'party-or-not/N"}],"value":"[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/prob-rain</span>","value":"#'party-or-not/prob-rain"}],"value":"[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain]"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:rain</span>","value":":rain"},{"type":"html","content":"<span class='clj-double'>0.6065999999999597</span>","value":"0.6065999999999597"}],"value":"[:rain 0.6065999999999597]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:norain</span>","value":":norain"},{"type":"html","content":"<span class='clj-double'>0.39339999999998315</span>","value":"0.39339999999998315"}],"value":"[:norain 0.39339999999998315]"}],"value":"{:rain 0.6065999999999597, :norain 0.39339999999998315}"}],"value":"[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:rain 0.6065999999999597, :norain 0.39339999999998315}]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/posterior-estimate-friend</span>","value":"#'party-or-not/posterior-estimate-friend"}],"value":"[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:rain 0.6065999999999597, :norain 0.39339999999998315}],#'party-or-not/posterior-estimate-friend]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/N</span>","value":"#'party-or-not/N"}],"value":"[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:rain 0.6065999999999597, :norain 0.39339999999998315}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/friend</span>","value":"#'party-or-not/friend"}],"value":"[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:rain 0.6065999999999597, :norain 0.39339999999998315}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/friend]"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:in</span>","value":":in"},{"type":"html","content":"<span class='clj-double'>0.8005999999999384</span>","value":"0.8005999999999384"}],"value":"[:in 0.8005999999999384]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:out</span>","value":":out"},{"type":"html","content":"<span class='clj-double'>0.19939999999999755</span>","value":"0.19939999999999755"}],"value":"[:out 0.19939999999999755]"}],"value":"{:in 0.8005999999999384, :out 0.19939999999999755}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:out</span>","value":":out"},{"type":"html","content":"<span class='clj-double'>0.48379999999997486</span>","value":"0.48379999999997486"}],"value":"[:out 0.48379999999997486]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:in</span>","value":":in"},{"type":"html","content":"<span class='clj-double'>0.5161999999999713</span>","value":"0.5161999999999713"}],"value":"[:in 0.5161999999999713]"}],"value":"{:out 0.48379999999997486, :in 0.5161999999999713}"}],"value":"({:in 0.8005999999999384, :out 0.19939999999999755} {:out 0.48379999999997486, :in 0.5161999999999713})"}],"value":"[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:rain 0.6065999999999597, :norain 0.39339999999998315}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/friend],({:in 0.8005999999999384, :out 0.19939999999999755} {:out 0.48379999999997486, :in 0.5161999999999713})]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/prob-friend</span>","value":"#'party-or-not/prob-friend"}],"value":"[[[[[[[[[[[[[nil,#'party-or-not/party-rain],#'party-or-not/party-friend],#'party-or-not/utility-rain],#'party-or-not/utility-friend],#'party-or-not/posterior-estimate-rain],#'party-or-not/N],#'party-or-not/prob-rain],{:rain 0.6065999999999597, :norain 0.39339999999998315}],#'party-or-not/posterior-estimate-friend],#'party-or-not/N],#'party-or-not/friend],({:in 0.8005999999999384, :out 0.19939999999999755} {:out 0.48379999999997486, :in 0.5161999999999713})],#'party-or-not/prob-friend]"}
;; <=

;; @@
; si calcola U(Party);U(donParty); U(Visit);U(donVisit);
(defn expected-utility-of-single-action [posterior action]
  
  (if(or (= action :doParty)(= action :dontParty)) 
  (reduce + (map #(* (utility-rain action (key %)) 
                     (val %)) 
                 posterior))
  (reduce + (map #(* (utility-friend action (key %)) 
                     (val %)) 
                 posterior)))
  )

(expected-utility-of-single-action prob-friend :doVisit)
(expected-utility-of-single-action prob-rain :dontParty)


;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility-of-single-action</span>","value":"#'party-or-not/expected-utility-of-single-action"},{"type":"html","content":"<span class='clj-double'>-179.05999999998767</span>","value":"-179.05999999998767"}],"value":"[#'party-or-not/expected-utility-of-single-action,-179.05999999998767]"},{"type":"html","content":"<span class='clj-double'>19.669999999999156</span>","value":"19.669999999999156"}],"value":"[[#'party-or-not/expected-utility-of-single-action,-179.05999999998767],19.669999999999156]"}
;; <=

;; @@
;U( party , rain )
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
(expected-utility-rain N [:doParty :dontParty])


; Partial Takes a function f and fewer than the normal arguments to f, and returns a fn that takes a variable number of additional args.
;Map return the result of applying f on a set of items

;U(visit/dontvisit, friend in ) U(visit/dontvisit, friend out) con rain = true, false
(defn expected-utility-friends [N actions observation]
  (let [posterior (posterior-estimate-friend N observation)]
  (vector observation(apply 
                        assoc 
                        {}
                        (interleave 
                          actions 
                          (map 
                            (partial 
                              expected-utility-of-single-action 
                              posterior)
                            actions)))))
    )
  

(map (partial expected-utility-friends N [:doVisit :dontVisit]) [true false])

;; si definisce quale è l'azione migliore da fare in caso di pioggia o no
(defn make-rational-policy 
  [actions 
   possible-observations 
   expected-utility-estimator]
  (let [expected-utilities-friends (map (partial 
                                  expected-utility-estimator 
                                  actions) 
                                possible-observations)]
    (apply 
      assoc 
      {} 
      (interleave 
        (map first expected-utilities-friends)
        (map #(key (apply max-key val %)) 
             (map second expected-utilities-friends))))))


(make-rational-policy [:doVisit :dontVisit] 
                      [true false] 
                      #(expected-utility-friends N %1 %2))

(defn make-rational-policy-rain
  [N actions]
  (let [expected ( expected-utility-rain N actions)]
    (key (apply max-key val(set expected)))
   
   ))

(make-rational-policy-rain N [:doParty :dontParty] )
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility-rain</span>","value":"#'party-or-not/expected-utility-rain"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:doParty</span>","value":":doParty"},{"type":"html","content":"<span class='clj-double'>145.75999999999453</span>","value":"145.75999999999453"}],"value":"[:doParty 145.75999999999453]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontParty</span>","value":":dontParty"},{"type":"html","content":"<span class='clj-double'>20.47999999999907</span>","value":"20.47999999999907"}],"value":"[:dontParty 20.47999999999907]"}],"value":"{:doParty 145.75999999999453, :dontParty 20.47999999999907}"}],"value":"[#'party-or-not/expected-utility-rain,{:doParty 145.75999999999453, :dontParty 20.47999999999907}]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/expected-utility-friends</span>","value":"#'party-or-not/expected-utility-friends"}],"value":"[[#'party-or-not/expected-utility-rain,{:doParty 145.75999999999453, :dontParty 20.47999999999907}],#'party-or-not/expected-utility-friends]"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:doVisit</span>","value":":doVisit"},{"type":"html","content":"<span class='clj-double'>-179.59999999998755</span>","value":"-179.59999999998755"}],"value":"[:doVisit -179.59999999998755]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontVisit</span>","value":":dontVisit"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"[:dontVisit 0.0]"}],"value":"{:doVisit -179.59999999998755, :dontVisit 0.0}"}],"value":"[true {:doVisit -179.59999999998755, :dontVisit 0.0}]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:doVisit</span>","value":":doVisit"},{"type":"html","content":"<span class='clj-double'>-150.09999999999192</span>","value":"-150.09999999999192"}],"value":"[:doVisit -150.09999999999192]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontVisit</span>","value":":dontVisit"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"[:dontVisit 0.0]"}],"value":"{:doVisit -150.09999999999192, :dontVisit 0.0}"}],"value":"[false {:doVisit -150.09999999999192, :dontVisit 0.0}]"}],"value":"([true {:doVisit -179.59999999998755, :dontVisit 0.0}] [false {:doVisit -150.09999999999192, :dontVisit 0.0}])"}],"value":"[[[#'party-or-not/expected-utility-rain,{:doParty 145.75999999999453, :dontParty 20.47999999999907}],#'party-or-not/expected-utility-friends],([true {:doVisit -179.59999999998755, :dontVisit 0.0}] [false {:doVisit -150.09999999999192, :dontVisit 0.0}])]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/make-rational-policy</span>","value":"#'party-or-not/make-rational-policy"}],"value":"[[[[#'party-or-not/expected-utility-rain,{:doParty 145.75999999999453, :dontParty 20.47999999999907}],#'party-or-not/expected-utility-friends],([true {:doVisit -179.59999999998755, :dontVisit 0.0}] [false {:doVisit -150.09999999999192, :dontVisit 0.0}])],#'party-or-not/make-rational-policy]"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-keyword'>:dontVisit</span>","value":":dontVisit"}],"value":"[true :dontVisit]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-keyword'>:dontVisit</span>","value":":dontVisit"}],"value":"[false :dontVisit]"}],"value":"{true :dontVisit, false :dontVisit}"}],"value":"[[[[[#'party-or-not/expected-utility-rain,{:doParty 145.75999999999453, :dontParty 20.47999999999907}],#'party-or-not/expected-utility-friends],([true {:doVisit -179.59999999998755, :dontVisit 0.0}] [false {:doVisit -150.09999999999192, :dontVisit 0.0}])],#'party-or-not/make-rational-policy],{true :dontVisit, false :dontVisit}]"},{"type":"html","content":"<span class='clj-var'>#&#x27;party-or-not/make-rational-policy-rain</span>","value":"#'party-or-not/make-rational-policy-rain"}],"value":"[[[[[[#'party-or-not/expected-utility-rain,{:doParty 145.75999999999453, :dontParty 20.47999999999907}],#'party-or-not/expected-utility-friends],([true {:doVisit -179.59999999998755, :dontVisit 0.0}] [false {:doVisit -150.09999999999192, :dontVisit 0.0}])],#'party-or-not/make-rational-policy],{true :dontVisit, false :dontVisit}],#'party-or-not/make-rational-policy-rain]"},{"type":"html","content":"<span class='clj-keyword'>:doParty</span>","value":":doParty"}],"value":"[[[[[[[#'party-or-not/expected-utility-rain,{:doParty 145.75999999999453, :dontParty 20.47999999999907}],#'party-or-not/expected-utility-friends],([true {:doVisit -179.59999999998755, :dontVisit 0.0}] [false {:doVisit -150.09999999999192, :dontVisit 0.0}])],#'party-or-not/make-rational-policy],{true :dontVisit, false :dontVisit}],#'party-or-not/make-rational-policy-rain],:doParty]"}
;; <=

;; @@

;; @@
