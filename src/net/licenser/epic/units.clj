(ns net.licenser.epic.units
  (:require (net.licenser.epic [modules :as modules] [utils :as utils]))
  (:require [net.licenser.epic.modules.damage :as modules.damage])
  (:require [net.licenser.epic.modules.cycle :as modules.cycle]))

(defstruct unit :id :name :team :cycle-script :x :y :modules :destroyed :last-target)

(defn no-script- [game unit-id]
  game)

(defn create-unit
  [id name team cycle-script x y & modules]
  (struct unit id name team (or cycle-script no-script-) x y (reduce #(let [id (utils/uuid)] (assoc %1 id (assoc %2 :id id))) {} modules) false nil))

(defn main-engine
  [unit]
   (first (sort-by (partial utils/module-spec :range) (utils/get-modules unit :engine))))

(defn unit-range
  [unit]
   (let [r (main-engine unit)]
     (modules/remaining-usages r)))


(defn unit-destroyed?
  [unit]
   (let [h (first (utils/get-modules unit :hull))]
     (>= (:damage h) (:hull h))))

(defn- update-unit
  [unit update-fn]
  (dosync
     (alter unit update-fn)))

(defn- make-module-list
  [modules]
  (if (empty? modules)
    {}
    (let [m (first modules)]
      (assoc (make-module-list (rest modules)) (:id m) m))))

(defn- hit-unit-modules
  [modules damage partial]
  (if modules
    (let [m (first modules)
	  ms (next modules)]
      (if (modules/module-destroyed? m)
	(let [{ms :modules ps :partial} (hit-unit-modules ms damage partial)]
	  {:modules (assoc ms (:id m) m) :partial (concat ps partial)})
	(let[t (modules.damage/module-hit m damage partial)
	     {damage :damage m :module continue :continue ps :partial} t
	     ps (if (modules/module-destroyed? m) (cons {:type :module_destroyed :module (:id m)} ps) ps)]
	  (utils/trace "hit-unit-modules" "hitting module:" m)  
	  (if (or (not continue) (<= damage 0))
	    (do 
	      (utils/trace "hit-unit-modules" "last module:" m)
	      (let [ms (make-module-list ms)]
		(utils/trace "hit-unit-modules" "generated module list:" ms)
		{:modules (assoc ms (:id m) m) :partial (concat ps partial)}))
	    (do
	      (utils/trace "hit-unit-modules" "handled module:" m)
	      (let [{ms :modules partial :partial} (hit-unit-modules ms damage partial)]
	      {:modules (assoc ms (:id m) m) :partial (concat ps partial)}))))))
    {:modules {} :partial partial}))


(defn init-unit
  [unit]
  (assoc unit :id (utils/uuid)))

(defn- hit-unit-priv
  [damage attacker unit]
   (let [id (:id unit)
	 ms (reverse (sort-by #(:hit-priority %) (vals (:modules unit))))
	 {ms :modules ps :partial} (hit-unit-modules ms damage '())
	 unit (assoc unit :modules ms)]
     (utils/combat-log :attack
		       {
			:unit (:id attacker)
			:target id
			:damage 5
			:partials ps
			})
     (if (unit-destroyed? unit)
       (do
	 (utils/combat-log :destroyed {:unit (:id unit) :partials ps})
	 (assoc unit :destroyed true))
       unit)))


(defn unit-energy
  [unit]
  (dosync
   (reduce + (map (partial utils/module-spec :energy) (utils/get-modules @unit :reactor)))))

(defn hit-unit
  [game unit attacker damage]
  (utils/trace "hit-unit" "hitting unit:" (type unit) "with:" damage "attacker:" (type attacker))
  (dosync
   (update-unit
    unit
    (partial hit-unit-priv damage @attacker))
   (if (unit-destroyed? @unit)
     (alter (utils/get-field (:map game) (:x @unit) (:y @unit))  #(doall (remove (partial = (:id @unit)) %)))))
  unit)

(defn unit-can-move-to?
  [unit x y]
  (let [d (utils/map-distance (:x unit) (:y unit) x y)]
    (and
     (>= (unit-range unit) d)
     (>= (unit-energy unit) (* (utils/module-spec :energy-usage (main-engine unit)) d)))))

(defn cycle-unit
  [game unit]
    (if (:destroyed @unit)
      (do
	(utils/trace 'units "Cycling unit: " (:id @unit) "(destroyed)")
	game)
      (do
	(utils/trace 'units "Cycling unit: " (:id @unit))
	(let [mods (map #(:id %) (utils/get-modules @unit))]
	  (dorun (map #(modules.cycle/cycle-module game unit %) mods)))
	(if (:cycle-script @unit)
	  (do
	    (utils/trace "units.cycle-unit" "Running custom script")
	    ((:cycle-script @unit) {'net.licenser.epic.utils/*cycle-log* utils/*cycle-log*} game unit)
	    game)
	  game))))