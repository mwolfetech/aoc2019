(ns care-package.visual
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as async]))

(defn setup [chan]
  (q/frame-rate 60)
  {:input chan :buffer nil})

(defn update-state [state]
  (-> state
      (assoc :buffer (async/poll! (:input state)))))

(defn draw-state [state]
  (when-let [[x y type] (:buffer state)]
      (case type
        0 (q/fill 0 0 0)
        1 (q/fill 82 82 254)
        2 (q/fill 197 66 69)
        3 (q/fill 82 82 254)
        4 (q/fill 255 255 0))
      (q/rect (* x 10) (* y 10)  10 10)))

(defn screen [chan]
  (q/sketch
   :title "Breakout"
   :size [440 210]
   :setup (partial setup chan)
   :update update-state
   :draw draw-state
   :features [:keep-on-top]
   :middleware [m/fun-mode]))

