(ns aoc-2022.day19
  (:require
   [aoc-utils.core :as utils]))

(defn- parse-line [[acc [max-orec max-clayc max-obsc]] line]
  ;; Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
  (let [s (re-seq #"\d+" line)
        [id orec clayc obsoc obscc geoorc geoobc] (map parse-long s)]
    [(conj acc {:id id,
                :robots [{:id :ore-robot :cost [orec 0 0] :prod [1 0 0]}
                         {:id :clay-robot :cost [clayc 0 0] :prod [0 1 0]}
                         {:id :obs-robot :cost [obsoc obscc 0] :prod [0 0 1]}
                         {:id :geode-robot :cost [geoorc 0 geoobc]}]})
     [(max max-orec orec clayc obsoc geoorc)
      (max max-clayc obscc)
      (max max-obsc geoobc)]]))

#_(parse-line "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.")

;; Given [orep clayp obsp] production and [orer clayr obsr] resources,
;; which robot can be produced in how many time?

;; How many turns to produce cost knowing rsc + prod/min
(defn- wait-time [cost prod rsc]
  ;; If the cost is null or below the available resources,
  ;; the time to wait is 0
  (if (or (< cost rsc) (= 0 cost)) 0
      ;; If the production is null and we don't have enough rsc
      ;; wait time is infinite
      (if (and (< rsc cost) (= prod 0)) nil
          (let [diff (- cost rsc)
                time (quot diff prod)]
            ;; The time to produce can't be negative because it's been
            ;; handled by the first condition
            ;; Just returns the closest greatest integer to the quotient
            (if (not= 0 (rem diff prod))
              (inc time)
              time)))))

#_(wait-time 1 1 0)

;; (defn- prune [robots]

(defn- robot-production [[orep clayp obsp] [orer clayr obsr] robots minutes max-minutes]
  (let [next-robots
        (reduce
         (fn [l {[orec clayc obsc] :cost :as robot}]
           (let [time-ore (wait-time orec orep orer)
                 time-clay (wait-time clayc clayp clayr)
                 time-obs (wait-time obsc obsp obsr)]
             ;; First case, one of the resources is not enough and the production is 0
             (cond
               (or (nil? time-ore) (nil? time-clay) (nil? time-obs))
               l

               :else
               (let [maxt (inc (max time-ore time-clay time-obs))]
                 (cond
                   (>= (+ maxt minutes) max-minutes)
                   l

                   :else
                   ;; The wait time is the time before we have the resources
                   ;; We need to increment it by one to actually produce the robot
                   (conj l [maxt robot])))))) '() robots)]
    next-robots))

#_(robot-production [1 4 0] [2 2 8] [{:id :ore-robot, :cost [4 0 0], :prod [1 0 0]}
                                     {:id :clay-robot, :cost [2 0 0], :prod [0 1 0]}
                                     {:id :obs-robot, :cost [1 14 0], :prod [0 0 1]}
                                     {:id :geode-robot, :cost [2 0 7]}] 21)

;; First filter, don't accept robots that would increase the production above the maximum production
(defn- filter-robots [robots [toto totc totobs] [maxo maxc maxobs]]
  (filter (fn [[_ {[orep clayp obsp] :prod}]]
            (or (nil? orep)
                (and (<= (+ toto orep) maxo)
                     (<= (+ totc clayp) maxc)
                     (<= (+ totobs obsp) maxobs)))) robots))

(defn- sort-robots [robots]
  ;; Sort robots before traversing them
  (sort
   (fn [[t1 r1] [t2 r2]]
     (cond
       (= (:id r1) :geode-robot)
       -1

       (= (:id r2) :geode-robot)
       1

       :else
       (let [c1 (compare t1 t2)]
         (if (= 0 c1) (compare (:id r1) (:id r2)) c1)))) robots))

(defn- max-possible [minutes] (/ (* (inc minutes) minutes) 2))

(defn- max-blueprint [{:keys [robots]} max-prod max-minutes]

  (let
   [curr-max (atom 0)
    mloop
    (fn mloop [tot-prod rsc geodes minutes]
          ;; (println history)
          ;; (println "\nprod" tot-prod "rsc" rsc "geodes" geodes "minutes" minutes)
      (cond
        (<= (dec max-minutes) minutes)
        (do
          (swap! curr-max #(max % geodes))
          ;; (println "curr-max" @curr-max)
          geodes)

        :else
        (let [next-robots
              (-> (robot-production tot-prod rsc robots minutes max-minutes)
                  (filter-robots tot-prod max-prod)
                  sort-robots)]
          (reduce
           (fn [max-geodes [time {:keys [id cost prod]}]]
             (let [minutes  (+ minutes time)
                   diff-minutes (- max-minutes minutes)]
               (cond
                 ;; (< (+ (max-possible diff-minutes) geodes) @curr-max)
                 ;; geodes

                 :else
                 (let [produced  (map #(* % time) tot-prod)
                       rsc       (map + rsc produced)
                       rsc       (map - rsc cost)
                       ;; If geode robot, just add its production to the geode
                       ;; score and forget about it
                       nprod     (if (= :geode-robot id) tot-prod (map + tot-prod prod))
                       ngeodes   (if (= :geode-robot id) (+ geodes diff-minutes) geodes)
                       geodes    (max max-geodes (mloop nprod rsc ngeodes minutes))]
                   (swap! curr-max #(max % geodes))
                   geodes)))) geodes next-robots))))]
    (mloop [1 0 0] [0 0 0] 0 0)))

(defn part-1 [file]
  (let [[blueprints max-prod] (utils/reduce-file parse-line [[] [0 0 0]] file)]
    (reduce (fn [acc robot] (+ acc (* (:id robot) (max-blueprint robot max-prod 24)))) 0 blueprints)))

(defn part-2 [file]
  (let [[blueprints max-prod] (utils/reduce-file parse-line [[] [0 0 0]] file)]
    (reduce (fn [acc robot] (* acc (max-blueprint robot max-prod 32))) 1 (take 3 blueprints))))

#_(part-1 (clojure.java.io/resource "day19-example"))

;; (defn- max-blueprint [{:keys [robots]} max-prod]
;;   (let [mloop
;;         (fn mloop [tot-prod rsc geodes minutes history]
;;           ;; (println history)
;;           ;; (println "\nprod" tot-prod "rsc" rsc "geodes" geodes "minutes" minutes)
;;           (cond
;;             (= minutes max-minutes)
;;             [geodes history]
;;             :else
;;             (let [next-robots (robot-production tot-prod rsc robots minutes)
;;                   next-robots (filter-robots next-robots tot-prod max-prod)
;;                   ;; _ (when (empty? next-robots) (println "history")
;;                   ;;         (pprint/pprint history))
;;                   result
;;                   (if (empty? next-robots) [geodes history]
;;                       (reduce
;;                        (fn [[max-geodes _ :as acc] [time {:keys [id cost prod] :as robot}]]
;;                      ;; (println "next robot taking" time "/" minutes "\n" robot)
;;                          (let [nminutes   (+ minutes time)
;;                                produced  (map #(* % time) tot-prod)
;;                                rsc       (map + rsc produced)
;;                                rsc       (map - rsc cost)
;;                            ;; If geode robot, just add its production to the geode
;;                            ;; score and forget about it
;;                                nprod     (if (= :geode-robot id) tot-prod (map + tot-prod prod))
;;                                ngeodes   (if (= :geode-robot id) (+ geodes (- max-minutes nminutes))
;;                                              geodes)
;;                                [ngeodes nhistory] (mloop nprod rsc ngeodes nminutes (conj history [robot nminutes nprod ngeodes]))]
;;                            (when (= 0 minutes) (println "received" ngeodes "from")
;;                                  (pprint/pprint nhistory))
;;                            (if (< ngeodes max-geodes) acc [ngeodes nhistory]))) [0 []] next-robots))]
;;               result)))]
;;     (mloop [1 0 0] [0 0 0] 0 0 [])))
