(ns net.licenser.epic
  (:use net.licenser.epic.utils)
  (:use net.licenser.epic.game)
  (:require (net.licenser.epic [modules :as modules] [units :as units]))
  (:require [clojure.contrib.json.write :as json])
  (:require [clojure.contrib.json.read :as jr])  
  (:require [clojure.contrib.duck-streams :as io])
  (:use net.licenser.epic.game.logic)
  (:use net.licenser.epic.game.basic)
  (:use clojure.template)
  (:use clojure.stacktrace)
  (:use clojure.contrib.command-line)
  (:gen-class))

(declare *game* *unit-id*)

(defn load-data-file
  [file add-fn]
  (doall
   (map (fn [data] (add-fn data)) (jr/read-json (slurp file)))))

(def *modules* (ref {}))

(def *scripts* (ref {}))

(defn add-hull
  [{name "name" size "size" maneuverability "maneuverability" hull "hull" mass "mass" :as data}]
  (dosync (alter *modules* assoc name (modules/create-hull name size mass hull maneuverability))))

(defn add-engine
  [{name "name" size "size" mass "mass" hull "hull" hit-propability "hit-propability" hit-priority "hit-priority" energy-usage "energy-usage" range "range"}]
  (dosync (alter *modules* assoc name (modules/create-engine name size mass hull hit-propability hit-priority range energy-usage))))

(defn add-armor
  [{name "name" size "size" mass "mass" hull "hull" hit-propability "hit-propability" hit-priority "hit-priority" damage-absorbtion "damage-absorbtion"}]
  (dosync (alter *modules* assoc name (modules/create-armor name size mass hull hit-propability hit-priority damage-absorbtion))))

(defn add-reactor 
  [{name "name" size "size" mass "mass" hull "hull" hit-propability "hit-propability" hit-priority "hit-priority" discharge-rate "discharge-rate" output "output" capacity "capacity" efficientcy "efficientcy"}]
  (dosync (alter *modules* assoc name (modules/create-reactor name size mass hull hit-propability hit-priority discharge-rate output capacity efficientcy))))

(defn add-shield
  [{name "name" size "size" mass "mass" hull "hull" energy "energy"}]
  (dosync (alter *modules* assoc name (modules/create-shield name size mass hull energy))))

(defn add-weapon
  [{
    name "name" 
    size "size" 
    mass "mass"
    hull "hull"
    hit-propability "hit-propability"
    hit-priority "hit-priority"
    damage "damage"
    fire-rate "fire-rate"
    range "range"
    variation "variation"
    accuracy "accuracy"
    rotatability "rotatability"
    energy-usage "energy-usage"}]
  (dosync (alter *modules* assoc name (modules/create-weapon name size mass hull hit-propability hit-priority damage fire-rate range variation accuracy rotatability energy-usage))))


(def *data-directory* "./data")

(def *pp-json* false)

(defn bind-game
  ([game]
     {:cycle-log (ref [])
      :game-log (ref [])
      :game game})
  ([]
     (bind-game (create-game 50))))

(defn cycle-game
  ([game-data]
     (binding [*cycle-log* (:cycle-log game-data)
	       *log* (:game-log game-data)]
       (let [t1  (filter #(= "one" (:team (deref %))) (vals (:units (:game game-data))))
	     c-t1 (count t1)
	     t1-a (filter #(not (:destroyed (deref %))) t1)
	     c-t1-a (count t1-a)
	     t2  (filter #(= "two" (:team (deref %))) (vals (:units (:game game-data))))
	     c-t2 (count t2)
	     t2-a (filter #(not (:destroyed (deref %))) t2)
	     c-t2-a (count t2-a)]
	 (println "team one:" c-t1-a "of" c-t1)
	 (println "team two:" c-t2-a "of" c-t2)
	 (if (and
	      (not (zero? c-t1-a))
	      (not (zero? c-t2-a)))
	   (do
	     (cycle-log)
	     (time (cycle-game* (:game game-data)))
	     (println "log:" (count @*log*))
	     (println "cycle-log:" (count @*cycle-log*))
	     @*cycle-log*)
	   nil))))
  ([] (cycle-game *game*)))

(defn ff-best-target
  ([unit hostiles range target variation]
     (try
      (if (not (empty? hostiles))
	(let [t (first hostiles)
	      d (map-distance unit @t)
	      v (Math/abs (- range d))]
	  (if (= d range)
	    t
	    (if (< v variation)
	      (recur unit (rest hostiles) range t v)
	      (recur unit (rest hostiles) range target variation))))
	target)
      (catch StackOverflowError e
	(println "In best-target")
	(print-stack-trace *e 5))))
     ([unit hostiles range]
	(ff-best-target unit hostiles range nil 1000000)))



(defn best-target
  ([hostiles perfect-fn better-fn target]
     (if (not (empty? hostiles))
       (let [t (first hostiles)]
	 (if (or (nil? target) (better-fn @t @target))
	   (if (perfect-fn @t)
	     t
	     (recur (rest hostiles) perfect-fn better-fn t))
	   (recur (rest hostiles) perfect-fn better-fn target)))
       target))
     ([hostiles perfect-fn better-fn]
	(best-target hostiles perfect-fn better-fn nil)))


(defn ff-cycle-script
  [game unit]
  ;(if (and (:last-target @unit) (not (:destroyed @unit)))
  ;  (dosync (fire-all (intercept-unit game unit (:last-target @unit) 2) unit (:last-target @unit)))
    (let [d (int 5)
	  hostiles (find-hostile-units game unit 100)
	  target (best-target 
		  hostiles 
		  (fn [t] (and (< 1 (map-distance @unit t) 3) (> 100 (unit-mass t))))
		  (fn [old-t new-t] (< (map-distance @unit new-t) (map-distance @unit old-t))))]
      (trace "cyclescript" "cycle for" (:id @unit) "attacking:" target)
      (if target
	(dosync (fire-all (intercept-unit game unit target 2) unit target))
	game)));)


(defn dd-cycle-script
  [game unit]
;  (if (and (:last-target @unit) (not (:destroyed @unit)))
;    (dosync (fire-all (intercept-unit game unit (:last-target @unit) 2) unit  (:last-target @unit)))
    (let [d (int 5)
	  hostiles (find-hostile-units game unit 100)
	  target (best-target 
		  hostiles 
		  (fn [t] (and (< 13 (map-distance @unit t) 17) (< 10000 (unit-mass t))))
		  (fn [old-t new-t] 
		    (or (> (unit-mass new-t) (* (unit-mass old-t) 2))
			(> (+ (Math/abs (- (map-distance @unit old-t) 15)) 5)
			   (Math/abs (- (map-distance @unit new-t) 15))))))]
      (trace "cyclescript" "cycle for" (:id @unit) "attacking:" target)
      (if target
	(dosync 
	 (combat-log :target {:unit (:id @unit) :target (:id @target)})
	 (combat-log :pause {})
	 (alter unit assoc :last-target target)
	 (fire-all (intercept-unit game unit target 15) unit target))
	game)));)

(dosync
 (alter *scripts* assoc "fighter" ff-cycle-script)
 (alter *scripts* assoc "destroyer" dd-cycle-script))

(defn build-unit
  [team unit]
  (let [modules (get unit "modules")
	script (get @*scripts* (get unit "script"))
	n (str (gensym "") "-" team)
	u (units/init-unit 
	   (apply units/create-unit 
		  0 n team script 0 0
		  (map #(get @*modules* %) modules)))]
    (assoc u :id n)))

(defn load-fight
  [file]
  (let [game (bind-game)
	data (jr/read-json (slurp file))]
    (binding [*cycle-log* (:cycle-log game)
	      *log* (:game-log game)]
      (assoc 
	  game :game 
	  (reduce (fn [game [team data]]
		    (let[units (get data "units")
			 start-x (get data "start-x")
			 start-y (get data "start-y")
			 d-x (get data "d-x")
			 d-y (get data "d-y")
			 row-size (get data "row-size")]
		      (reduce 
		       (fn [game [unit i]]
			 (let [u (build-unit team unit)
			       x (- start-x (mod i row-size))
			       x (int (+ x (* d-x (Math/floor (/ i row-size)))))
			       y (- start-y (mod i row-size))
			       y (int (+ y (* d-y (Math/floor (/ i row-size)))))
			       g (add-unit game u)
			       u (get-unit g (:id u))]
			   (combat-log :spawn {:unit (:id @u) :team team :data (unit-data @u)})
			   (move-unit* g u x y))) game (map (fn [a b] [a b]) units (iterate inc 0)) ))) (:game game) data)))))
  


(defn make-cycle-seq
     [game]
     (lazy-seq 
      (let [l (cycle-game game)]
	(if (not (empty? l))
	  (cons l (make-cycle-seq game))
	  nil))))

(defn save-log 
  [game file]
  (binding [*cycle-log* (:cycle-log game)
	    *log* (:game-log game)]
    (dosync
       (if (not (empty? @*cycle-log*)) (cycle-log))
       (io/with-out-writer (io/writer file)   ;))))
	 ((if *pp-json* json/print-json json/print-json) @*log*)))))

(defn save-log1
  [game]
  (save-log game "x:/interface/log.json"))
       


(defn multi-game-seq
     [games]
     (lazy-seq 
      (let [_ (println "=====START=====")
	    f (time (doall (map first games)))
	    _ (println "======END======")]
	(if (every? nil? f)
	  nil
	(cons f (multi-game-seq (map rest games)))))))



	;games  (map #(make-cycle-seq (init-game %)) [
;					1])
;	     1000  1000  1000  1000
;	      249   249   249  249  249  249  249  249
;	      249   249   249  249  249  249  249  249
;	       84    84    84   84   84   84   84   84
;	       84    84    84   84   84   84   84   84
;	       84    84    84   84   84   84   84   84])
;       m-g (multi-game-seq games)

(defn -main
  [& args]
  (with-command-line args
    "EPIC shell"
    [[data-directory "specifies a data directory." "./data"]
     [in-file "json fight definition" "./fight.json"]
     [out-file "output json file" "./log.json"]
     ]
    (load-data-file (str data-directory "/hulls.json") add-hull)
    (load-data-file (str data-directory "/engines.json") add-engine)
    (load-data-file (str data-directory "/armors.json") add-armor)
    (load-data-file (str data-directory "/generators.json") add-reactor)
    (load-data-file (str data-directory "/shields.json") add-shield)
    (load-data-file (str data-directory "/weapons.json") add-weapon)
    (let [a-game (load-fight in-file)
	  g (make-cycle-seq a-game)]
      (println "START")
      (time (def x (dorun g)))
      (save-log a-game out-file))))
