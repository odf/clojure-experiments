(ns clojure-experiments.core)

(defprotocol IGraph
  "A simple directed graph, not necessarily finite or locally finite."
  (vertices [G])
  (pred [G])
  (succ [G]))

(defrecord Graph [verts back forw]
  IGraph
  (vertices [G] verts)
  (pred [G] back)
  (succ [G] forw))

(defn edges [G]
  (mapcat (fn [[v adj]] (map (partial vector v) adj))
          (succ G)))

(defn isolated? [G v] (and ((vertices G) v)
                           (empty? ((pred G) v))
                           (empty? ((succ G) v))))

(def empty-graph (Graph. #{} {} {}))

(defn with-vertex [G v]
  (cond ((vertices G) v)
        G
        true
        (Graph. (conj (vertices G) v)
                (conj (pred G) [v #{}])
                (conj (succ G) [v #{}]))))

(defn with-vertices [G verts] (reduce with-vertex G verts))


(defn without-vertex [G v]
  (cond ((vertices G) v)
        (let [not-v (fn [u] (not (= u v)))
              purge-v-from-set (fn [s] (into #{} (filter not-v s)))
              purge-v-from-map (fn [m] (into {} (map (fn [[w adj]]
                                                       [w (purge-v-from-set adj)]) m)))]
          (Graph. (disj (vertices G) v)
                  (dissoc (purge-v-from-map (pred G)) v)
                  (dissoc (purge-v-from-map (succ G)) v)))
        true
        G))

(defn without-vertices [G verts] (reduce without-vertex G verts))

(defn with-edge [G [v w]]
  (let [G1 (with-vertices G [v w])]
    (Graph. (vertices G1)
            (conj (pred G1) [w (conj ((pred G1) w) v)])
            (conj (succ G1) [v (conj ((succ G1) v) w)]))))

(defn with-edges [G edges] (reduce with-edge G edges))

(defn without-edge [G [v w]]
  (Graph. (vertices G)
          (conj (pred G) [w (disj ((pred G) w) v)])
          (conj (succ G) [v (disj ((succ G) v) w)])))

(defn without-edges [G edges] (reduce without-edge G edges))
