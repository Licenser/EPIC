(ns net.licenser.epic.modules.damage
  (:use net.licenser.epic.utils)
  (:use net.licenser.epic.modules))

(defmulti module-hit-static
  "This method is called for every module if it be hit."
  (fn [module _ _] (trace "module-hit-static(dispatcher)" "got:" (:type module)) (:type module)))

(defmethod module-hit-static :armor
  [module damage partial]
  (let [my-dmg damage
	damage-absorbation (:damage-absorbation (:specification module))
	hull (max 0 (- (:hull module) (:damage module)))
	damage-absorbation (min hull damage-absorbation)
	damage (max 0 (- damage damage-absorbation))]
    {:module (damage-module module my-dmg)
     :damage damage
     :partial (if (>= damage-absorbation 0) (cons {:type :armor_impact :damage my-dmg :hp hull} partial) partial)
     :continue (if (<= damage 0) false true)}))

(defmethod module-hit-static :shield
  [module damage partial]
  (let [e (:energy (:specification module))
	my-dmg (min damage e)
	damage (- damage my-dmg)
	hull (max 0 (- (:hull module) damage))]
    {:module (update-in (damage-module module damage)
	       [:specification :energy] #(- % my-dmg))
     :partial (if (= 0 damage) (cons {:type :shield_impact :damage my-dmg :hp hull} partial) partial)
     :damage damage
     :continue (if (<= damage 0) false true)}))

(defmethod module-hit-static :hull
  [module damage partial]
  (let [m (damage-module module damage)]
    {:module m 
     :partial (cons {:type :impact :damage damage :hp (- (:hull m) (:damage m))} partial)
     :damage damage :continue true}))

(defmethod module-hit-static :default
  [module damage partial]
  {:module (damage-module module damage) :damage damage :continue true :partial partial})

(defn module-hit
  ([module damage prop partial]
     (if (and 
	  (< prop (:hit-propability module))
	  (not (module-destroyed? module)))
       (module-hit-static module damage partial)
       {:module module :damage damage :continue true :partial partial}))
  ([module damage partial]
     (module-hit module damage (rand) partial)))