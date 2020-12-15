(ns the-nbody-problem.core
  (:require [orchestra.core :refer [defn-spec]]
            [orchestra.spec.test :as st]
            [clojure.spec.alpha :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(s/def ::x int?)
(s/def ::y int?)
(s/def ::z int?)
(s/def ::name string?)

(defprotocol Gravity
  (calculate-adjustment [self other])
  (apply-gravity [self]))

(defprotocol Movement
  (apply-velocity [self])
  (clear-adjustment [self]))

(defprotocol Energy
  (get-potential-energy [self])
  (get-kinetic-energy [self])
  (get-total-energy [self]))

(defrecord Position [x y z])
(s/def ::position (s/keys :req-un [:the-nbody-problem.core/x ::x :the-nbody-problem.core/y ::y :the-nbody-problem.core/z ::z]))

(s/fdef ->Position
  :args (s/cat  :the-nbody-problem.core/x ::x :the-nbody-problem.core/y ::y :the-nbody-problem.core/z ::z)
  :ret ::position)

(defrecord Velocity [x y z])
(s/def ::velocity (s/keys :req-un [:the-nbody-problem.core/x ::x :the-nbody-problem.core/y ::y :the-nbody-problem.core/z ::z]))

(s/fdef ->Velocity
  :args (s/cat  :the-nbody-problem.core/x ::x :the-nbody-problem.core/y ::y :the-nbody-problem.core/z ::z)
  :ret ::velocity)


(defrecord Adjustment [x y z])

(defn comparev [x1 x2]
  (cond
    (> x1 x2) -1
    (< x1 x2) 1
    :else 0))

(defn diff [k position other]
  (comparev (k position) (k (:position other))))

(defrecord Moon [name ^Position position ^Velocity velocity ^Adjustment adjustment]
  Gravity
  (calculate-adjustment [self other]
    (let [x (diff :x position other)
          y (diff :y position other)
          z (diff :z position other)]
      (assoc self :adjustment (merge-with + adjustment {:x x :y y :z z}))))
  (apply-gravity [self]
    (assoc self :velocity (merge-with + adjustment velocity)))
  Movement
  (apply-velocity [self]
    (assoc self :position (merge-with + position velocity)))
  (clear-adjustment [self]
    (assoc self :adjustment {:x 0 :y 0 :z 0}))
  Energy
  (get-potential-energy [self]
    (reduce + (map math/abs (vals position)))) 
  (get-kinetic-energy [self]
    (reduce + (map math/abs (vals velocity))))
  (get-total-energy [self]
    (* (get-potential-energy self) (get-kinetic-energy self))))

(defn spawn-moon [name x y z]
  (->Moon
   name
   (->Position x y z)
   (->Velocity 0 0 0)
   (->Adjustment 0 0 0)))

(s/def ::moon (s/keys :req-un [:the-nbody-problem.core/name ::name :the-nbody-problem.core/position ::position :the-nbody-problem.core/velocity ::velocity]))

(s/fdef ->Moon
  :args (s/cat :the-nbody-problem.core/name ::name :the-nbody-problem.core/position ::position :the-nbody-problem.core/velocity ::velocity))

(def Io (spawn-moon "Io" 0 4 0)) 

(def Europa (spawn-moon "Europa" -10 -6 -14)) 

(def Ganymede (spawn-moon "Ganymede" 9 -16 -3)) 

(def Callisto (spawn-moon "Callisto" 6 -1 2)) 

(defn make-system [moons]
   (reduce (fn [m v] (assoc m (:name v) v)) {} moons))

(defn calculate-gravities [a b]
  [(calculate-adjustment a b)
   (calculate-adjustment b a)])

(defn adjust-gravities-on-system [system]
  (let [pairs (combo/combinations (keys system) 2)]
    (reduce (fn [res [a b]]
              (let [[new-a new-b] (calculate-gravities (get res a) (get res b))]
                (-> res
                    (assoc (:name new-a) new-a)
                    (assoc (:name new-b) new-b))))
            system
            pairs)))

(defn apply-changes-on-system [system]
    (make-system (map (comp apply-velocity clear-adjustment apply-gravity) (vals system))))

(defn do-time-step [moons]
  (->> moons
       (make-system)
       (adjust-gravities-on-system)
       (apply-changes-on-system)
       vals))

(defn iterate-simulation [moons iterations]
  (loop [t iterations
         m moons]
    (if (>  1 t)
      m
      (recur (dec t) (do-time-step m)))))

(defn extract-position-coordinate [system coordinate]
  (map #(get-in %1 [:position coordinate]) (vals (select-keys system ["Io" "Ganymede" "Europa" "Callisto"]))))

(defn iterate-simulation-until-repeat-coordinates [moons coordinate]
  (let [initial (extract-position-coordinate (make-system moons) coordinate)]
    (loop [i 0
           m moons]
      (if (and (not= 0 i) (= initial (extract-position-coordinate (make-system m) coordinate)))
        (inc i)
        (recur (inc i) (do-time-step m))))))


(defn -main-part-1
  "N-body simulation on four moons"
  [& _]
  (let [moons (iterate-simulation [Io Ganymede Europa Callisto] 1000)]
    (reduce + (map get-total-energy moons))))

(defn -main-part-2
  [& _]
  (reduce math/lcm [
                    (iterate-simulation-until-repeat-coordinates [Io Ganymede Europa Callisto] :x)
                    (iterate-simulation-until-repeat-coordinates [Io Ganymede Europa Callisto] :y)
                    (iterate-simulation-until-repeat-coordinates [Io Ganymede Europa Callisto] :z)
                    ]))
