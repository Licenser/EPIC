(ns net.licenser.epic.utils
  ;(:require [clojure.contrib.io :as c.c.io])
  (:import java.util.UUID))

(def *trace* true)

(declare *log*)


(defn module-spec
  ([module spec]
     (get (:specification module) spec))
  ([module] (:specification module)))

(defn get-module
  [unit module-id]
    (get (:modules unit) module-id nil))

(defn get-modules
  ([unit]
     (vals (:modules unit)))
  ([unit type]
     (filter #(= type (:type %)) (get-modules unit))))

(defn unit-energy
  [unit]
  (reduce + 0 (map #(module-spec % :energy) (get-modules unit :reactor))))

(defn use-energy
  [unit energy]
     (let [energy (ref energy)]
       (dosync
	(if (< @energy (unit-energy @unit))
	  (do
	    (alter 
	     unit assoc :modules 
	     (reduce 
	      #(assoc %1 (:id %2) %2) {}
	      (map (fn [m] (if (= (:type m) :reactor)
			     (let [e (module-spec m :energy)
				   used-e (min @energy e)]
			       (alter energy - used-e)
			       (update-in m [:specification :energy] #(- % used-e)))
			     m))
		   (vals (:modules @unit)))))
	    true)
	  false))))

(defn unit-mass
  [unit]
  (reduce + (map (fn [m] (:mass m)) (get-modules unit))))

(defn unit-hull [unit]
  (:hull (first (get-modules unit :hull))))

(defn unit-data [unit]
  {:id (:id unit)
   :damage 0
   :type {
	  :name (:name (first (get-modules unit :hull)))
	  :hull (unit-hull unit)}
   :team (:team unit)
   :mass (unit-mass unit)
   :modules '()})

(def *cycle-log* (ref []))

(defn cycle-log []
  (dosync
   (alter *log* conj @*cycle-log*)
   (ref-set *cycle-log* [])))

(defn combat-log [type msg]
  (dosync
   (alter *cycle-log* conj (assoc msg :type type))))

(defn log [prefix id & msgs]
  (binding [*out* *err*]
	(apply println (str prefix "<" id ">:") msgs)))

;(defmacro trace [id & msgs]
;  (if *trace*
;    `(apply log "Trace" ~id (seq ~msgs))))

(defn trace [id & msgs]
  (if *trace*
    (apply log "Trace" id msgs)))


(defn update-module
  [unit module-id update-fn]
  (trace "update-module" "updating" module-id (str "(" (:type (get-module @unit module-id)) ")") "of" (:id @unit))
  (dosync
   (alter unit update-in [:modules module-id] update-fn)))

(defn update-module-specs
  ([unit module-id update-fn]
     (update-module unit module-id (fn [m] (assoc m :specification (update-fn (:specification  m))))))
  ([unit module-id spec update-fn]
     (update-module-specs unit module-id (fn [s] (assoc s spec (update-fn (get s spec)))))))


(defn get-unit
  [game unit-id]
  (get (:units game) unit-id nil))

(defn in-direction
  ([x y direction]
  (condp = (int direction)
    0 [(inc x) (inc y)]
    1 [(inc x) y]
    2 [x (dec y)]
    3 [(dec x) (dec y)]
    4 [(dec x) y]
    5 [x (inc y)]))
  ([u direction]
     (in-direction (:x u) (:y u) direction)))

(defn to-evil [x y]
  [(- x y) (int (Math/floor (/ (* -1 (+ x y)) 2.0)))])

(defn- to-cartesian [x y]
  (let [[e-x e-y] (to-evil x y)
	c-x e-x
	c-y (if (odd? e-x) (+ 0.5 e-y) e-y)
	c-y (* -1 c-y (Math/sqrt 3))]
  [c-x c-y]))

(defn friction-direction-to
  ([x1 y1 x2 y2]

     (let [[c-x1 c-y1] (to-cartesian x1 y1)
	   [c-x2 c-y2] (to-cartesian x2 y2)
	   a (Math/atan2 (- c-y2 c-y1) (- c-x2 c-x1))
	   a (- 0 (/ a Math/PI))
	   a (+ a 0.5)
	   a (* a 3)
	   a (mod a 6)]
       a))
  ([u1 u2]
     (friction-direction-to (:x u1) (:y u1) (:x u2) (:y u2))))

(defn direction-to
  ([x1 y1 x2 y2]
     (Math/round (friction-direction-to x1 y1 x2 y2)))
  ([u1 u2]
     (direction-to (:x u1) (:y u1) (:x u2) (:y u2))))

(defn uuid
  []
  (.toString (UUID/randomUUID)))

(defn static
  [val]
  (fn [ & _ ] val))

(defn map-distance
  ([u1 u2]
     (map-distance (:x u1) (:y u1) (:x u2) (:y u2)))
  ([x1 y1  x2 y2]
    (let [x1 (int x1)
	  x2 (int x2)
	  y1 (int y1)
	  y2 (int y2)
	  dx (- x1 x2)
	  dy (- y1 y2)
	  ax (Math/abs dx)
	  ay (Math/abs dy)]
    (if (= (< dx 0) (< dy 0))
      (max ax ay)
      (+ ax ay)))))

(defn get-field [map x y]
  (dosync
   (trace "fet-fied" x y)
   (let [f-x (get @map x)]
     (if f-x
       (let [f-y (get @f-x y)]
	 (trace "fet-fied" "x already present")
	 (if f-y
	   (do
	     (trace "fet-fied" "y already present")
	     f-y)
	   (let [f-y (ref '())]
	     (alter f-x assoc y f-y)
	     f-y)))
       (let [f-y (ref '())
	     f-x (ref {y f-y})]
	 (alter map assoc x f-x)
	 f-y)))))