(ns care-package.core
    (:gen-class)
     (:require [care-package.intcode :as b]
               [care-package.visual :as v]
               [clojure.core.async :as async]
               [clojure.core.matrix :as mm]
               [clojure.math.numeric-tower :as nn]
               [clojure.pprint :as pp]
               [clojure.string :as string]))

(def input
  (-> (slurp "program.in")
      (string/replace-first #"\d+," "2,")
      (string/trim-newline)))

(defn initial-state [program-text in-channel out-channel log-chan]
  {:program-text program-text
   :in in-channel
   :out out-channel
   :log log-chan})

(defn load-input [in-channel val]
  (async/>!! in-channel val))

(defn make-compute-thread [in-channel out-channel log-chan]
  (async/thread (b/compute  (initial-state input in-channel out-channel log-chan)) (async/close! out-channel)(async/close! in-channel)))




(defn run []
  (let [log-chan (async/chan)
        input-channel (async/chan 1)
        output-channel (async/chan 3)
        screen-channel (async/chan 1)]
    (v/screen screen-channel)
    (make-compute-thread input-channel output-channel log-chan)
    (load-input input-channel 1)
    (async/thread (loop [] (when-let [v (async/<!! log-chan)]
                            (println v)
                             (recur))))
     (loop [x (async/<!! output-channel) count (+ 2 (* 44 21)) pos 22 joystick 1]
         (if-not (nil? x)
           (let [y (async/<!! output-channel)
                 type (async/<!! output-channel)
                 new-pos (if (= type 3) x pos)
                 new-joystick (if (= type 4)
                            (cond
                              (< x new-pos) -1
                              (> x new-pos) 1
                              (= x new-pos) 0) joystick)]
             (if (and (= x -1)(= y 0))
               (async/>!! log-chan type)
               (async/>!! screen-channel [x y type]))
             (if (and (>= 0 count) (= type 4)) 
               (async/>!! input-channel new-joystick))
             (recur (async/<!! output-channel) (dec count) new-pos new-joystick))))))
 
(defn -main [& _args]
  (run))
 
