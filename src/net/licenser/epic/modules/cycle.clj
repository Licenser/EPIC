(ns net.licenser.epic.modules.cycle
  (:require [net.licenser.epic.utils :as utils]))


(defn- apply-cycle-script
  [game unit module-id]
  (let [s (:cycle-script (utils/get-module @unit module-id))]
    (if s
      (s game unit module-id)
      unit)))

;(defn default-module-cycle
;  ([game unit module-id]
;     (apply-cycle-script game unit module-id))
;  ([game unit module-id update-fn]
;     (utils/update-module (apply-cycle-script game unit module-id) module-id #(update-fn  %))))

(defn default-module-cycle
  ([game unit module-id]
     unit)
  ([game unit module-id update-fn]
     (utils/update-module unit module-id #(update-fn  %))))

(defn cycle-generic
  [m])

(defmulti cycle-module
  "This method is called for every module at the start of the tick"
  (fn [game unit module-id] 
    (let [type (:type (utils/get-module @unit module-id))]
    (utils/trace "modules.cycle" "Determining type: " type) 
    type)))

(defmethod cycle-module :reactor
  [game unit module-id]
  (default-module-cycle game unit module-id 
    (fn [module]
      (let [specs (:specification module)]
	(update-in
	 module [:specification :energy] #(min (:capacity specs) (+ % (:output specs))))))))

(defmethod cycle-module :engine
  [game unit module-id]
  (default-module-cycle game unit module-id
    (fn [module]
      (let [specs (:specification module)]
	(update-in module [:specification :times-used] (utils/static 0))))))

(defmethod cycle-module :weapon
  [game unit module-id]
  (default-module-cycle game unit module-id
    (fn [module]
      (let [specs (:specification module)]
	(update-in module [:specification :times-used] (utils/static 0))))))


(defmethod cycle-module :default
  [game unit module-id]
  (utils/trace "modules.cycle"  "Cyceling module:" module-id) 
  (default-module-cycle game unit module-id))