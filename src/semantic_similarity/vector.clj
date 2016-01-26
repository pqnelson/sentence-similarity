(ns semantic-similarity.vector)

(defn subtract  [vec1 vec2]
  (map - vec1 vec2))

(defn add [vec1 vec2]
  (map + vec1 vec2))

(defn- sq [x] (* x x))

(defn norm [vec]
  (Math/sqrt
    (reduce + (map sq vec))))

(defn normalize [vec]
  (map #(/ %1 (norm vec))
       vec))

(defn dot-product [vec1 vec2]
  (apply + (map * vec1 vec2)))

(defn cross-product [vec1 vec2]
  ; pretend that this vec is not transposed
  (map #(* %1 %2) vec1 vec2))
