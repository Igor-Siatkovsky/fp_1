(ns clusters.clusterize
  (:gen-class))
(require '[clojure.string :as str])

(defstruct Point :coordinates :potential)
(def e_bottom 0.15)
(def Ra 3)
(def alpha (/ 4 (Math/pow Ra 2)))
(def Rb (* Ra 1.5))

(def e_top 0.5)
(def beta (/ 4 (Math/pow Rb 2)))

(defn square_distance
  "Square of euclidean distance."
  [point_1 point_2 axis_number]
  (defn square_dif [axis_number]
    (Math/pow (- (get (get point_2 :coordinates) axis_number) (get (get point_1 :coordinates) axis_number)) 2))
  (if
    (< axis_number (count (get point_1 :coordinates)))
    (+ (square_dif axis_number) (square_distance point_1 point_2 (inc axis_number)))
    0
  )
)

(def import_points
  (memoize (fn [path_of_file]
    (for
      [point
        (for [string_point (str/split (slurp path_of_file) #"\n")]
          (vec (for [axis (str/split string_point #",")]
            (read-string axis))
          )
        )
      ]
      (struct Point point))
  ))
)

(defn point_to_point_potential
  "Get potential of point relatively to one point."
  [point_1, point_2, coef]
  (Math/pow Math/E (- (* coef (square_distance point_1 point_2 0))))
)

(defn ppoint_to_multiple_points_potential
  "Get potential of point relatively to multiple points."
  [ppoints, base_point]
  (reduce +
    (for [point ppoints]
      (point_to_point_potential base_point point alpha)
    )
  )
)

(def revise_potentials
  (memoize (fn
    [max_potential_point, rest_points]
    (sort-by :potential >
      (for [point rest_points]
        (struct Point
          (get point :coordinates)
          (- (get point :potential)
             (* (get max_potential_point :potential)
                (point_to_point_potential max_potential_point point beta)
             )
          )
        )
      )
    )
  ))
)

(def part_by_max_potential
  (memoize (fn [points]
    "Split set of points to ((biggest_potential_point)(rest points))."
    (let [max_potential (apply max (map #(get % :potential) points))]
      ((juxt filter remove) #(= max_potential (get % :potential)) points)
    )
  ))
)

(defn cluster
  [points clusters_centers first_maxim_point]
  (let [part_points (part_by_max_potential points)]
    (let [maxim_point (first (first part_points)) restt_points (second part_points)]
      (if
        (> (count points) 0)
        (if
          (> (get maxim_point :potential) (* e_top (get first_maxim_point :potential)))
          (recur
            (revise_potentials maxim_point restt_points)
            (conj clusters_centers maxim_point)
            first_maxim_point)
          (if
            (< (get maxim_point :potential) (* e_bottom (get first_maxim_point :potential)))
            clusters_centers
            (let [d_min (apply min (for [center clusters_centers] (Math/sqrt (square_distance maxim_point center 0))))]
              (if
                (>= (+ (/ d_min Ra) (/ (get maxim_point :potential) (get first_maxim_point :potential))))
                (recur (revise_potentials maxim_point restt_points) (conj clusters_centers maxim_point) first_maxim_point)
                (recur (revise_potentials maxim_point restt_points) clusters_centers first_maxim_point)
              )
            )
          )
        )
        clusters_centers
      )
    )
  )
)

(def points_with_potentials
  (memoize (fn [points]
;to add another points, we need only to add parameters here in function points
    (for [base_point points]
      (struct Point
        (get base_point :coordinates)
        (ppoint_to_multiple_points_potential points base_point)
      )
    )
  ))
)

(defn out_clusterize
  [path_of_file]
  (let [points (points_with_potentials (import_points path_of_file))]
    (let [parted_points (part_by_max_potential points)]
      (let [max_point (first (first parted_points)) rest_points (second parted_points)]
        (conj (cluster rest_points () max_point) max_point)
      )
    )
  )
)
