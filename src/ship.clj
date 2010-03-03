(comment
  {
   :modules '(list of modules)
   :hull max-hull
   :damage damage taken
   })

(defn ship-destroyed?
  [ship]
  (>= (:damage ship) (:hull ship)))

(defn ship-mass
  [ship]
  (+ (get ship :mass 0) (reduce (fn [mass module] mass (get module :mass 0)) 0 (:modules ship))))

(defn ship-energy
  [ship]
  (reduce + (map :energy (filter #(= (:type %)) (:modules ship)))))


(defn- ship-modules-get-energy
  [modules energy]
  (let [module (first modules)]
    (if (not= (:type module) :reactor)
      (cons module (ship-modules-get-energy (next modules) energy))
      (if (>= (:energy module) energy)
	(cons (assoc module :energy (- (:energy module) energy)) (next modules))
	(cons
	 (assoc module :energy 0)
	 (ship-modules-get-energy (next modules) (- energy (:energy module))))))))
      
(defn ship-use-energy
  [ship energy]
  (if (> (ship-energy ship) energy)
    (assoc ship :modules (ship-modules-get-energy (:modules ship) energy))
    false))
  

  
(defn- damage-ship-module
  [data module]
  (if (:continue data)
    (let [result (module-hit-static module (:damage data) 0.5)]
      (assoc data
	:modules (conj (:modules data) (:module result))
	:damage (:damage result)
	:continue (and (:continue result) (> 0 (:damage result)))))
    (assoc data :modules (conj (:modules data) module))))


(defn damage-ship
  [ship damage]
  (let [result
	(reduce damage-ship-module {:damage damage :modules '() :continue true} (:modules ship))]
  (assoc ship
    :damage (+ (:damage ship) (:damage result))
    :modules (:modules result))))

(def ship
  {:hull 100
   :damage 0
   :mass 40
   :modules (list reactor1 reactor1)})

(damage-ship ship 10)