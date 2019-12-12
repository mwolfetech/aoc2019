(ns sunny-with-a-chance-of-asteroids.core
  (:gen-class)
  (:use clojure.pprint))

(def input "3,225,1,225,6,6,1100,1,238,225,104,0,1102,89,49,225,1102,35,88,224,101,-3080,224,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1101,25,33,224,1001,224,-58,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1102,78,23,225,1,165,169,224,101,-80,224,224,4,224,102,8,223,223,101,7,224,224,1,224,223,223,101,55,173,224,1001,224,-65,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,2,161,14,224,101,-3528,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1002,61,54,224,1001,224,-4212,224,4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1101,14,71,225,1101,85,17,225,1102,72,50,225,1102,9,69,225,1102,71,53,225,1101,10,27,225,1001,158,34,224,101,-51,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,102,9,154,224,101,-639,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,102,2,223,223,1006,224,329,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,344,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,359,1001,223,1,223,108,226,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,107,226,677,224,102,2,223,223,1006,224,389,101,1,223,223,1107,226,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1007,226,226,224,102,2,223,223,1006,224,434,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,479,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,494,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,509,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,524,101,1,223,223,7,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,584,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,599,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,614,101,1,223,223,108,677,677,224,102,2,223,223,1005,224,629,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,659,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226")

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
    (-> state
        (move-pc fp)
        (write-dangerously dest 5))))

(defn do-output [state]
  (let [ fp (get-function-parameters state 1 0)]
    (-> state
        (move-pc fp)
        (update-in [:output] #(conj % (first (:parameters fp)))))))

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
            (assoc-in [:program] (vec (map (comp #(Integer. %) #(second %)) (re-seq #"(-?\d+),?" (:input state)))))
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

(defn -main [& args]
  (println (compute {:input input})))
