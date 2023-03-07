(ns datalog.analysis.graph
  (:require [clojure.data.priority-map :refer [priority-map]]
            [medley.core :as m]))

(defn dijkstra
  "Computes shortest distances for graph with dijkstra algorithm using a priority queue.
   Complexity is O(|E|+|V|lg(|V|)).
   - vertex: vertex to compute distances for
   - neighbours: function or map containing the list of neighbouring vertices"
  [vertex neighbours]
  (loop [queue (priority-map vertex 0)
         distances {}]
    (if-let [[vertex distance] (peek queue)]
      (let [dist (-> (neighbours vertex)
                     (m/remove-keys (keys distances))
                     (m/map-vals (partial + distance)))]
        (recur (merge-with min (pop queue) dist)
               (assoc distances vertex distance)))
      distances)))


(defn has-cycle? [graph start-vertex]
  (let [reachable-nodes (keys (dijkstra start-vertex graph))]
    (m/find-first #(graph % start-vertex) reachable-nodes)))