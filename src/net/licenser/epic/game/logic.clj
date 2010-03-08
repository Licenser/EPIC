(ns net.licenser.epic.game.logic
  (:require [net.licenser.epic.game.basic :as basic])
  (:use clojure.stacktrace)
  (:use clojure.contrib.str-utils)
  (:require (net.licenser.epic [modules :as modules] [units :as units] [utils :as utils])))

(defn directions
  ([start i d] 
     (lazy-seq 
      (cons 
       (mod (+ start (* d i)) 6) 
       (cons 
	(mod (- start (* d i)) 6) 
	(directions start (inc i) d))))) 
  ([start d] 
     (take 6 (cons (mod start 6) (directions start 1 d)))))


(defn intercept-unit
  ([game unit target distance visited]
     (utils/trace "logic.intercept-unit" "start")
     (dosync
      (loop [game game unit unit target target distance distance visited visited]
	(let [u-id (:id @unit)
	      t-id (:id @target)
	      d (utils/map-distance @unit @target)
	      visited (conj visited [(:x @unit) (:y @unit)])]
	  (if (or
	       (zero? (units/unit-range @unit))
	       (= distance d)
	       (not (utils/use-energy unit (utils/module-spec (units/main-engine @unit) :energy-usage))))
	    (do
	      (if (zero? (units/unit-range @unit))
		(utils/trace "logic.intercept" "out of gass:" (units/main-engine @unit))
		(utils/trace "logic.intercept" "destinateion reached:" d "of" distance))
	      game)
	    (let [best-direction (if (or true (> d distance))
				   (utils/friction-direction-to @unit @target)
				   (mod (+ (utils/friction-direction-to @unit @target) 3) 6))
		  rounded-direction (int (Math/round best-direction))
		  rounded-direction (if (< distance d) rounded-direction (+ rounded-direction 3))
		  delta (if (< best-direction rounded-direction) 1 -1)
		  dirs (directions rounded-direction delta)
		  _ (utils/trace "logic.intercept" "possible directions:" dirs)
		  positions (filter (fn [p] 
				      (and (every? (partial not= p) visited) (empty? (basic/unit-at game (first p)  (second p)))))
				    (map (partial utils/in-direction (:x @unit) (:y @unit)) dirs))
		  _ (utils/trace "logic.intercept" "possible new and empty positions:" positions)
		  [x y] (first positions)]
	      (utils/trace "logic.intercept" "moving from" (:x @unit) (:y @unit) "to" x y)
	      (let [g (basic/move-unit* game unit x y)
		    e (units/main-engine @unit)
		    g (utils/update-module-specs unit (:id e) :times-used inc)]
		(recur game unit target distance visited))))))))
     ([game unit target distance]
	(intercept-unit game unit target distance '())))


 (defn find-units
   [game pred]
    (filter (fn [u]
	      (utils/trace "find-units" "testing unit:" (:id @u))
	      (if (and utils/*trace* (pred @u))
		(utils/trace "find-units" "testing unit:" (:id @u) "positive!")
		(utils/trace "find-units" "testing unit:" (:id @u) "negativ!"))
	      (pred @u)) (vals (:units game))))

 (defn find-hostile-units
   ([game unit]
       (let [t (int (:team @unit))]
	 (utils/trace "find-hostile-units" "Looking for units in team:" t)
	 (find-units game #(and
			    (not (:destroyed %))
			    (not= t (int (:team  %)))))))
   ([game unit range]
      (let [t (:team @unit)]
	 (utils/trace "find-hostile-units" "Looking for units in team:" t "in range" range)
	 (find-units
	  game
	  #(and
	    (not (:destroyed %))
	    (not= t (:team %))
	    (>= range (utils/map-distance @unit  %)))))))


 (defn can-fire-at? [unit weapon target]
   (dosync
    (let [spec (utils/module-spec weapon)
	  dist (utils/map-distance target unit)]
      (and
       (not (modules/module-destroyed? weapon))
       (not (units/unit-destroyed? target))
       (<= (- (:range spec) (:variation spec)) dist (+ (:range spec) (:variation spec)))
       (<= (:times-used spec) (:max-usage spec))))))

(defn fire-weapon [game unit weapon-id target]
  (dosync
   (let [w (utils/get-module @unit weapon-id)]
     (utils/trace "game.logic.fire-weapon" "u:" (:id @unit) "t:" (:id @target)  "w:" weapon-id)
     (if (and
	  (can-fire-at? @unit w @target)
	  (utils/use-energy unit (utils/module-spec w :energy-usage)))
       (do
	 (utils/trace "game.logic.fire-weapon" "Can Fire: attacker:" (:id @unit) "| target:" (:id @target))
	 (if ((utils/module-spec w :unit-hit-fn) unit w target)
	   (units/hit-unit game target unit (utils/module-spec w :damage))
	   game))
       (do
	 (utils/trace "game.logic.fire-weapon" "Can't-Fire attacker:" (:id @unit) "| target:" (:id @target))
	 game)))))

(defn emply-point-defense
  [game unit]
  (when-let [pd-s  (filter #(re-find #"Point Defense" (:name %)) (utils/get-modules @unit :weapon))]
    (let [r (reduce #(max %1 %2) (map #(+ (utils/module-spec % :range) (utils/module-spec % :variation)) pd-s))
	  ts (find-hostile-units game unit r)]
      (doall (map (fn [w]
		    (doall (map (fn [t] (fire-weapon game unit (:id w) t)) ts))) pd-s))))
    game)

(defn fire-all [game unit target]
  (let [modules (map #(:id %) (utils/get-modules @unit :weapon))]
   (utils/trace "fire-all" "modules:" modules "of" (:id @unit) "at" (:id @target))
   (reduce
    (fn [game weapon-id] (utils/trace "fire-all" "fiering:" weapon-id) (fire-weapon game unit weapon-id target) game)
    game
    modules)))