(ns amplification-circuit.base
  (:gen-class)
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.core.async :refer (<!! >!!)]))

(defn take-while+ [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn read-instruction [state]
  (let [pc (:pc state)]
    (cond (nil? pc) nil
          :else (nth (:program state) pc))))

(defn write-dangerously [state dest value]
  (if (nil? dest) state
      (assoc-in state [:program] (assoc (:program state) dest value))))

(defn get-parameters [state opcodes values]
  (map-indexed
   (fn [index value]
     (if (= 0 (nth opcodes index))
       (nth (:program state) value)
       value)) values))

(defn get-function-parameters [state arity returns]
  (let [pc (:pc state)
        opcodes (:opcodes state)
        program (:program state)
        sz (+ arity returns)
        section (take sz (drop (inc pc) program))]
    {:parameters (get-parameters state opcodes (take arity section))
     :sz sz
     :section section}))

(defn move-pc [state fp]
  (update-in state [:pc] #(+ (inc (:sz fp)) %)))

(defn read-input [state]
  (let [fp (get-function-parameters state 0 1 )
        dest (last (:section fp))]
      (-> state (move-pc fp)
          (write-dangerously dest (<!! (:in state))))))

(defn do-output [state]
  (let [fp (get-function-parameters state 1 0)
        val (first (:parameters fp))]
      (>!! (:out state) val)
      (-> state
        (move-pc fp)
        (update-in [:output] #(conj % val)))))

(defn do-terminate [state]
  state)

(defn do-math [operator state]
  (let [fp (get-function-parameters state 2 1)
        dest (last (:section fp))
        value (reduce operator (:parameters fp))]
    (-> state
        (move-pc fp)
        (write-dangerously dest value))))

(defn do-jump [cmp val state]
  (let [fp (get-function-parameters state 2 0)
        parameters (:parameters fp)
        jmp? (cmp val (first parameters))
        value (second parameters)]
    (if jmp? (assoc-in state [:pc] value)
        (move-pc state fp))))

(defn do-compare [cmp state]
  (let [fp (get-function-parameters state 2 1)
        [a b] (:parameters fp)
        pass? (cmp a b)
        dest (last (:section fp))]
    (-> state
        (write-dangerously dest (if pass? 1 0))
        (move-pc fp))))

(defn get-op [num]
  (case num
    1 (partial do-math +)
    2 (partial do-math *) 
    3 (partial read-input)
    4 (partial do-output)
    5 (partial do-jump not= 0)
    6 (partial do-jump = 0)
    7 (partial do-compare <)
    8 (partial do-compare =)
    99 (partial do-terminate)))

(defn parse-instruction [instruction]
  (let [digits (map #(Character/digit % 10) (cl-format nil "~5'0d" instruction))
        opcodes (reverse (take 3 digits))
        op (Integer/parseInt (apply str (take-last 2 digits)))]
    [op opcodes]))

(defn execute-instruction  [state]
  (let [instruction (read-instruction state)
        [op opcodes] (parse-instruction instruction)]
    ((get-op op) (assoc-in state [:opcodes] opcodes))))

(defn update-state [state]
  (cond (nil? (:program state))
        (-> state
            (assoc-in [:program] (vec (map (comp #(Integer. %) #(second %)) (re-seq #"(-?\d+),?" (:program-text state)))))
            (assoc-in [:pc] 0)
            (assoc-in [:output] []))
        :else
        (execute-instruction state)))

(defn adjust-program [noun verb state]
  (let [program (:program state)]
    (assoc-in state [:program]
              (-> program
                  (assoc 1 noun)
                  (assoc 2 verb)))))

(defn compute [state]
  (->> state
       (update-state)
       (iterate update-state)
       (take-while+ #(not= 99 (read-instruction %)))
       (last)
       (:output)
       (flatten)))

