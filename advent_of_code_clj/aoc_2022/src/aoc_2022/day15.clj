(ns aoc-2022.day15
  (:require
   [aoc-utils.core :as utils]
   [clojure.string :as str]))

(defn- manhattan-distance [x1 y1 x2 y2]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn- mark-square [[voids beacons] sx sy bx by]
  (let [dist (manhattan-distance sx sy bx by)

        beacons (conj beacons [bx by])

        ;; left {:x (- sx dist) :y sy}
        ;; right {:x (+ sx dist) :y sy}
        ;; up {:x sx :y (- sy dist)}
        ;; down {:x sx :y (+ sy dist)}
        voids (conj voids {:sx sx :sy sy :dist dist})]
    [voids beacons]))

(defn- get_x_itvl [{:keys [sx sy dist]} y]
  (cond
    ;; y is crossing the top or bottom part of the rectangle
    (or (<= (- sy dist) y sy)
        (<= sy y (+ sy dist)))
    (let [diff (abs (- sy y))
          xl (+ (- sx dist) diff)
          xr (- (+ sx dist) diff)]
      [xl xr])

    :else
    nil))

#_(test-range 0 0 3 0)

#_(mark-square [{} ()] 3 3 6 3)

(defn- line->voids [acc line]
  (let [re #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
        [_ & rest] (re-find re line)
        [sx sy bx by] (map parse-long rest)]
    (mark-square acc sx sy bx by)))

(defn- merge-itvl [il]
  (let [sil (sort #(compare (first %1) (first %2)) il)
        [prev rest] (reduce (fn [[[low1 up1 :as i1] prev] [low2 up2 :as i2]]
              ;; Since the list is sorted low2 >= low1
                              (if (<= low2 up1) [[low1 (max up1 up2)] prev]
                                  [i2 (conj prev i1)])) [(first sil) '()] (rest sil))]
    (reverse (conj rest prev))))

(defn- split [xrows itvll]
  (loop [[[low up :as it] & itl :as il] itvll
         [x & xtl :as xl] xrows
         nitl '()]
    (cond
      (or (nil? x) (nil? low))
      (concat il nitl)

      (< x low)
      (recur il xtl nitl)

      (= low x up)
      (recur itl xtl nitl)

      (= low x)
      (recur itl xtl (conj nitl [(inc x) up]))

      (= up x)
      (recur itl xtl (conj nitl [low (dec x)]))

      (< low x up)
      (recur itl xtl (conj nitl [low (dec x)] [(inc x) up]))

      :else
      (recur itl xl (conj nitl it)))))

(defn- length [[low up]] (inc (- up low)))

#_(merge-itvl #{[4 9] [7 10] [8 8] [1 2]})
#_(split [0 5 11] (merge-itvl #{[4 9] [7 10] [8 8] [1 2]}))

(defn part-1 [file row]
  (let [[voids beacons] (utils/reduce-file line->voids [#{} #{}] file)
        beacons (filter #(= (second %) row) beacons)
        beacons (map first beacons)
        beacons (sort beacons)
        xrows (reduce
               (fn [xrows rect] (if-let [xrow (get_x_itvl rect row)]
                                  (conj xrows xrow)
                                  xrows)) #{} voids)
        xrows (merge-itvl xrows)
        xrows (split beacons xrows)]
    (reduce (fn [acc i] (+ acc (length i))) 0 xrows)))

(defn- dist-1-diamonds [diamonds {:keys [dist sx sy]}]
  (filter (fn [{sxd :sx syd :sy distd :dist}]
            (= (manhattan-distance sxd syd sx sy)
               (+ dist distd 2))) diamonds))

(defn- all-pairs [diamonds]
  (loop [[hd & tl] diamonds
         acc []]
    (cond
      (nil? hd)
      acc

      :else
      (let [neighbours (dist-1-diamonds tl hd)]
        (recur tl (reduce (fn [acc d] (conj acc [hd d])) acc neighbours))))))

(defn- point-in-diamond? [[x y] {:keys [dist sx sy]}]
  (< (manhattan-distance sx sy x y) dist))

(defn- diamonds-intersect [{:keys [dist sx sy]} diamond]
  (let [p1 [(- sx dist) sy]
        p2 [(+ sx dist) sy]
        p3 [sx (- sy dist)]
        p4 [sx  (+ sy dist)]]
    (println p1 "" p2 "" p3 "" p4)
    (and (or (point-in-diamond? p1 diamond)
             (point-in-diamond? p2 diamond)
             (point-in-diamond? p3 diamond)
             (point-in-diamond? p4 diamond))
         (not (and (point-in-diamond? p1 diamond)
                   (point-in-diamond? p2 diamond)
                   (point-in-diamond? p3 diamond)
                   (point-in-diamond? p4 diamond))))))

(defn- segment-intersection [[x1 y1] [x2 y2] [x3 y3] [x4 y4]]
  (let [det (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))
        tnum (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4)))
        unum (- (* (- x1 x3) (- y1 y2)) (* (- y1 y3) (- x1 x2)))
        t (/ (float tnum) (float det))
        u (/ (float unum) (float det))]
    (if (and (<= 0 t 1) (<= 0 u 1))
      [(int (+ x1 (* (/ tnum det) (- x2 x1))))
       (int (+ y1 (* (/ tnum det) (- y2 y1))))]
      nil)))

(defn- intersection [{psx :sx psy :sy pdist :dist} {qsx :sx qsy :sy qdist :dist}]
  (let [pleft [(- psx pdist) psy]
        pright [(+ psx pdist) psy]
        pup [psx (- psy pdist)]
        pdown [psx  (+ psy pdist)]
        qleft [(- qsx qdist) qsy]
        qright [(+ qsx qdist) qsy]
        qup [qsx (- qsy qdist)]
        qdown [qsx  (+ qsy qdist)]]
    (list
     (segment-intersection pleft pup qleft qdown)
     (segment-intersection pleft pup qup qright)
     (segment-intersection pdown pright qleft qdown)
     (segment-intersection pdown pright qup qright)

     (segment-intersection pleft pdown qdown qright)
     (segment-intersection pleft pdown qleft qup)
     (segment-intersection pup pright qdown qright)
     (segment-intersection pup pright qleft qup))))

(defn- ppoint [{:keys [sx sy dist]}]
  (let [left [(- sx dist) sy]
        right [(+ sx dist) sy]
        up [sx (- sy dist)]
        down [sx  (+ sy dist)]]
    (println left "" up "" right "" down)))

(defn part-2 [file]
  (let [[voids _] (utils/reduce-file line->voids [#{} ()] file)
        [[p1 p2] [q1 q2]] (all-pairs voids)
        i1 (intersection p1 q1)
        i2 (intersection p1 q2)
        i3 (intersection p2 q1)
        i4 (intersection p2 q2)]
    (ppoint p1)
    (ppoint p2)
    (ppoint q1)
    (ppoint q2)
    (clojure.pprint/pprint i1)
    (clojure.pprint/pprint i2)
    (clojure.pprint/pprint i3)
    (clojure.pprint/pprint i4)))

#_(part-1  (clojure.java.io/resource "day15-example") 10)
#_(part-2  (clojure.java.io/resource "day15-example2"))
#_(part-2  (clojure.java.io/resource "day15"))

#_(part-1  (clojure.java.io/resource "day15") 2000000)
