(ns fp-1.core
  (:gen-class))

(use 'clusters.clusterize)
(require '[clojure.string :as str])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [clusters_centers (out_clusterize (first args))]
    (println (count clusters_centers))
    (println clusters_centers)
  )
)
