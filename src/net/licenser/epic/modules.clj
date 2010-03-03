(ns net.licenser.epic.modules
  (:use net.licenser.epic.utils))

(defstruct module
  :id :type :name :size :cycle-script :specification :mass :hull :damage :hit-propability :hit-priority)

(defn default-unit-hit-fn
  [attacker weapon target]
  (let [dist (int (map-distance @attacker @target))
	weapon-hull (int (:hull weapon))
	weapon-damage (int (:damage weapon))
	damage-penalty (/ (- weapon-hull weapon-damage) weapon-hull)
	accuracy (module-spec weapon :accuracy)
	variation (int (inc (module-spec weapon :variation)))
	range (int (module-spec weapon :range))
	rotatability (module-spec weapon :rotatability)
	a-manuv (module-spec (first (get-modules @attacker :hull)) :maneuverability)
	t-manuv (module-spec (first (get-modules @target :hull)) :maneuverability)
	aim (+ 
	     (* damage-penalty (/ (* accuracy (+ 2 (rand)))3)) 
	     (/ (- variation (int (Math/abs (- range dist)))) variation 2))
	mass (Math/log10 (+ (/ (Math/pow (unit-mass target) 1/3) (max dist 1)) 1))
	aiming (*
		(+
		 (/ (* a-manuv (+ 2 (rand))) 3)
		 rotatability)
		aim)
	evade (/ (* t-manuv (+ 2 (rand))) 3)]
    (> 1 (* (/ aiming evade) mass))))

(defn default-module-hit-fn
  [m]
  (> (rand) (:hit-propability m)))

(defn create-hull
  [name size mass hull maneuverability]
  (struct module nil :hull name size nil {:maneuverability maneuverability} mass hull 0 1.0 0.0))

(defn create-generic
  [type name size mass hull hit-propability hit-priority max-usage energy-usage]
  (struct module nil type name size nil {:max-usage max-usage :times-used 0 :energy-usage energy-usage} mass hull 0 hit-priority energy-usage))

(defn create-engine
  [name size mass hull hit-propability hit-priority range energy-usage]
  (create-generic :engine name size mass hull hit-propability hit-priority range energy-usage))

(defn create-weapon
  ([name  size  mass hull hit-propability hit-priority damage fire-rate range variation accuracy rotatability energy-usage unit-hit-fn module-hit-fn]
  (update-in 
   (create-generic :weapon name size mass hull hit-propability hit-priority fire-rate energy-usage)
   [:specification]
   #(assoc % 
      :damage damage
      :accuracy accuracy
      :rotatability rotatability
      :energy-usage energy-usage
      :unit-hit-fn unit-hit-fn
      :module-hit-fn module-hit-fn
      :range range
      :variation variation
      )))
  ([name  size  mass hull hit-propability hit-priority damage fire-rate range variation accuracy rotatability energy-usage]
     (create-weapon
     name  size  mass hull hit-propability hit-priority damage fire-rate range variation accuracy rotatability energy-usage
      default-unit-hit-fn default-module-hit-fn)))


(defn create-shield
  [name size mass hull energy]
  (struct module nil :shield name size nil {:max-energy energy :energy energy} mass hull 0 1.0 0.99))


(defn create-reactor
  [name size mass hull hit-propability hit-priority discharge-rate output capacity efficientcy]
  (struct module nil :reactor
	  name size nil
	  {:capacity capacity :energy capacity :output output :discharge-rate discharge-rate :discharged 0 :efficientcy efficientcy}
	  mass hull 0
	  hit-propability hit-priority))

(defn create-armor
  [name size mass hull hit-propability hit-priority damage-absorbtion]
  (struct module nil :armor
	  name size nil
	  {:damage-absorbation damage-absorbtion}
	  mass hull 0
	  hit-propability hit-priority))

(defn damage-module
  [module damage]
  (update-in module [:damage] #(min (:hull module) (+ % damage))))
  
(defn module-destroyed?
  [module]
  (>= (:damage module) (:hull module)))

(defn remaining-usages
  [module]
  (let [s (module-spec module)]
    (- (:max-usage s) (:times-used s))))
