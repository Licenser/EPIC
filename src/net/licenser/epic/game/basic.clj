(ns net.licenser.epic.game.basic
  (:use net.licenser.epic.utils)
  (:require [net.licenser.epic.units :as units]))

(defn unit-at
  [game x y]
  (deref (get-field (:map game) x y)))
	   

(defn move-unit*
  [game unit x y]
  (if (and
       (integer? x)
       (integer? y))
    (dosync
     (let [u-x (:x @unit)
	   u-y (:y @unit)
	   id (:id @unit)
	   map (:map game)
	   f-u (get-field map u-x u-y)
	   f-d (get-field map x y)]
       (if (empty? @f-d)
	 (do 
	   (combat-log :move
		       {:unit id
			:position {:x x :y y}})
	   (trace "game.basic.move-unit*" "Moving" id "to" x "/" y)
	   (alter unit assoc :x x :y y)
	   (alter f-u  #(doall (remove (partial = id) %)))
	   (alter f-d conj id)
	   game)
	 game)
       game))))