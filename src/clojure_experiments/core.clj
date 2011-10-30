(ns clojure-experiments.core)

(defn filter-set [f s] (reduce disj s (filter #(not (f %)) s)))

(defn map-values [f m]
  (reduce (fn [m [k v]] (let [fv (f v)]
                          (cond (= v fv) m true (assoc m k fv))))
          m m))


(defprotocol IGraph
  "A simple directed graph."
  (vertices [G])
  (pred [G])
  (succ [G]))

(defrecord Graph [verts back forw]
  IGraph
  (vertices [G] verts)
  (pred [G] back)
  (succ [G] forw))


(defn graph [& vs] (apply with-edges (cons (Graph. #{} {} {}) vs)))

(defn vertex? [G v] (and ((vertices G) v) true))

(defn source? [G v] (and (vertex? G v) (empty? ((pred G) v)) true))

(defn sink? [G v] (and (vertex? G v) (empty? ((succ G) v)) true))

(defn internal? [G v] (not (or (empty? ((succ G) v)) (empty? ((pred G) v)))))

(defn isolated? [G v] (and (vertex? G v) (empty? ((pred G) v)) (empty? ((succ G) v))))

(defn edge? [G v w]
  (and (vertex? G v)
       (((succ G) v) w)
       true))

(defn edges [G]
  (mapcat (fn [[v adj]] (map vector (repeat v) adj))
          (succ G)))

(defn adj [G v]
  "The successors of a vertex followed by the predecessors"
  (concat ((succ G) v) ((pred G) v)))

(defn with-vertices [G & vs]
  (when G
    (reduce
     (fn [G v]
       (if (vertex? G v)
         G
         (Graph. (conj (vertices G) v)
                 (conj (pred G) [v #{}])
                 (conj (succ G) [v #{}]))))
     G vs)))

(defn without-vertices [G & vs]
  (when G
    (reduce
     (fn [G v]
       (if (vertex? G v)
        (let [purge-v-from-map (partial map-values (partial filter-set #(not= % v)))]
          (Graph. (disj (vertices G) v)
                  (dissoc (purge-v-from-map (pred G)) v)
                  (dissoc (purge-v-from-map (succ G)) v)))
        G))
     G vs)))

(defn with-edges [G & vs]
  (when G
    (reduce
     (fn [G [v w]]
       (if (or (edge? G v w) (= v w))
         G
         (let [G1 (with-vertices G v w)]
           (Graph. (vertices G1)
                   (conj (pred G1) [w (conj ((pred G1) w) v)])
                   (conj (succ G1) [v (conj ((succ G1) v) w)])))))
     G (partition 2 vs))))

(defn without-edges [G & vs]
  (when G
    (reduce
     (fn [G [v w]]
       (if (edge? G v w)
         (Graph. (vertices G)
                 (conj (pred G) [w (disj ((pred G) w) v)])
                 (conj (succ G) [v (disj ((succ G) v) w)]))
         G))
     G (partition 2 vs))))
