(ns semantic-similarity.vector)

(defn vec-subtract  [vec1 vec2]
  (map - vec1 vec2))

(defn vec-add [vec1 vec2]
  (map + vec1 vec2))

(defn- sq [x] (* x x))

(defn vec-norm [vec]
  (Math/sqrt
    (reduce + (map sq vec))))

(defn vec-normalize [vec]
  (map #(/ %1 (vec-norm vec))
       vec))

(defn vec-dot-product [vec1 vec2]
  (apply + (map * vec1 vec2)))

(defn cross-product [vec1 vec2]
  ; pretend that this vec is not transposed
  (map #(* %1 %2) vec1 vec2))
