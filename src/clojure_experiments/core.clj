(ns clojure-experiments.core)

(defprotocol IGraph
  "A directed graph, not necessarily finite or locally finite."
  (vertices [G])
  (pred [G])
  (succ [G])
  (no-neighbors [G]))

(defrecord Graph [verts back forw]
  IGraph
  (vertices [G] verts)
  (pred [G] back)
  (succ [G] forw)
  (no-neighbors [G] #{}))

(def empty-graph (Graph. #{} {} {}))

(defn with-vertex [G v]
  (cond ((vertices G) v)
        G
        true
        (Graph. (conj (vertices G) v)
                (conj (pred G) [v (no-neighbors G)])
                (conj (succ G) [v (no-neighbors G)]))))

(defn with-edge [G e]
  (let [[v w] e G1 (reduce with-vertex G e)]
    (Graph. (reduce conj (vertices G1) e)
            (conj (pred G1) [w (conj ((pred G1) w) v)])
            (conj (succ G1) [v (conj ((succ G1) v) w)]))))
