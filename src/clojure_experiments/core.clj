(ns clojure-experiments.core)

(defprotocol IGraph
  "A directed graph, not necessarily finite or locally finite."
  (vertices [G])
  (pred [G])
  (succ [G]))

(deftype Graph [verts back forw]
  IGraph
  (vertices [G] verts)
  (pred [G] back)
  (succ [G] forw))

(def empty-graph (Graph. #{} {} {}))

(defn with-vertex [G v]
  (cond ((vertices G) v)
        G
        true
        (Graph. (conj (vertices G) v)
                (conj (pred G) [v #{}])
                (conj (succ G) [v #{}]))))

(defn with-edge [G e]
  (let [[v w] e]
    (Graph. (reduce conj (vertices G) e)
            (conj (pred G) [w (conj ((pred G) w) v)])
            (conj (succ G) [v (conj ((succ G) v) w)]))))
