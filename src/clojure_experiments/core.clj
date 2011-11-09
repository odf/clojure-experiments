(ns clojure-experiments.core)


;; Variations of set and map that don't create new collections.

(defn filter-set [f s] (reduce disj s (filter #(not (f %)) s)))

(defn map-values [f m]
  (reduce (fn [m [k v]] (let [fv (f v)] (if (= v fv) m (assoc m k fv)))) m m))


;; An implementation of directed graphs as a persistent data structure.

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


(defn vertex? [G v] (and ((vertices G) v) true))

(defn source? [G v] (and (vertex? G v) (empty? ((pred G) v)) true))

(defn sink? [G v] (and (vertex? G v) (empty? ((succ G) v)) true))

(defn internal? [G v] (not (or (empty? ((succ G) v)) (empty? ((pred G) v)))))

(defn isolated? [G v]
  (and (vertex? G v) (empty? ((pred G) v)) (empty? ((succ G) v))))

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
         (let [purge-v-from-map (partial map-values
                                         (partial filter-set #(not= % v)))]
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

(defn graph [& vs] (apply with-edges (cons (Graph. #{} {} {}) vs)))


;; Generic graph traversal.

(defn traversal [adj seen todo push head tail]
  (when-let [node (head todo)]
    (let [neighbors (adj node)
          todo (reduce push (tail todo) (filter (complement seen) neighbors))
          seen (into (conj seen node) neighbors)]
      (lazy-seq (cons node (traversal adj seen todo push head tail))))))

(defn dfs [adj & sources]
  "Performs a lazy depth first traversal of the directed graph determined by
  the list 'sources' of source nodes and the adjacency function 'adj'."
  (traversal adj #{} (into '() sources) conj first rest))

(defn bfs [adj & sources]
  "Performs a lazy breadth first traversal of the directed graph determined by
  the list 'sources' of source nodes and the adjacency function 'adj'."
  (traversal adj #{} (into clojure.lang.PersistentQueue/EMPTY sources)
             conj first pop))

(defn by-edges [adj] (fn [[u v]] (map vector (repeat v) (adj v))))

(defn dfs-by-edges [adj & sources]
  (apply dfs (cons (by-edges adj) (map vector (repeat nil) sources))))

(defn bfs-by-edges [adj & sources]
  (apply bfs (cons (by-edges adj) (map vector (repeat nil) sources))))


;; Lazy sequence experiments.

(defn tails [s]
  "The sequence of sequences starting at each position in the given sequence."
  (lazy-seq (when-let [s (seq s)]
              (cons s (tails (rest s))))))

(defn reduce-true [f val coll]
  "A short-circuiting version of 'reduce'. Stops evaluation when the
  accumulated value is logically false."
  (let [v (->> (reductions f val coll)
               (tails)
               (take-while first)
               (last))]
    (if (empty? (rest v)) (first v))))
