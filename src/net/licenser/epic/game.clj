(ns net.licenser.epic.game
  (:require (net.licenser.epic [units :as units] [utils :as utils]))
  (:use clojure.stacktrace)
  (:use (net.licenser.epic.game [basic])))

(defstruct game :map :units)

(defn add-unit
  [game unit]
  (update-in game [:units (:id unit)] (utils/static (ref unit))))

(defn create-game
  ([]
     (struct game (ref {}) {}))
  ([map-size]
     (let [g (create-game)
	   s (* -1 map-size)
	   e (inc map-size)]
       (dorun
	(map (fn [x] (map (fn [y] (utils/get-field (:map g) x y)) (range s e))) (range s e)))
       g)))

(defn- cycle-reduce-fn
  [game unit-id]
  (units/cycle-unit game unit-id))

(defn- cycle-unit-fn
  [game unit-id]
  (utils/trace "cycle-unit-fn" "cyceling unit" unit-id)
  (units/cycle-unit game (utils/get-unit game unit-id)))

(defn cycle-game*
  ([game]
     (dorun
      (map (partial cycle-unit-fn game) (keys (:units game)))))
  ([game partition-size]
     (try
      (dorun (pmap 
	      (bound-fn [batch] (doall (map (fn [unit] (cycle-unit-fn game unit)) batch)))
	      (partition partition-size partition-size nil (filter #(not (units/unit-destroyed? @(utils/get-unit game %))) (keys (:units game))))))
      (catch StackOverflowError e
	(println "In cycle-game*")
	(print-stack-trace e 5)))))
