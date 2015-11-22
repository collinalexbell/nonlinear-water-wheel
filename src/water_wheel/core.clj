(ns water_wheel.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def center-x 540)
(def center-y 360)
(def base-moment 500)
(def start-momentum 20)
(def time-step 2)



(use 'clojure.test)

(with-test
  (defn angle-and-length-to-position [{angle :angle length :length}]
    (into {} (map (fn [entry] 
      ( if (= :x (first entry))
        [(first entry) (second entry)]
        [(first entry) (* -1 (second entry))])) 
    {
     :x (q/round (* length (q/cos (q/radians angle))))
     :y (q/round (* length (q/sin (q/radians angle))))
    })))
  (is (= {:x 2.0 :y 0.0} (angle-and-length-to-position {:angle 0 :length 2}))))



(defn evenly-divide-a-circle 
  "Returns a vector of evenly spaced angles"
  [num-divisions previous-angle]
  (let [angle-inc (/ 360.0 num-divisions)]
    (map #(mod (+ (* %1 angle-inc) previous-angle) 360) (range num-divisions)))
  )




(defn position-relative-to-center [position]
  {:x (+ center-x (:x position)) :y (+ center-y (:y position))}
  )




;Need to fix this using x-left x-right
(defn draw-bucket [{x :x y :y}]
  (q/fill 255 255 255 0)
  (q/quad (- x 50) (- y 50) (+ x 50) (- y 50) (+ x 35) (+ y 50) (- x 35) (+ y 50))  
  )




(defn create-default-source []
  {:fill-rate 10 :x-left (- center-x 25) :x-right (+ center-x 25)}
  )




(defn create-wheel [num-bins length wheel-angle]
  (assoc {} :angle wheel-angle :length length :bins
    (map (fn [bin] (assoc bin :x-left (- (bin :x) 50) :x-right (+ (bin :x) 50))) 
      (map #(position-relative-to-center %1)
        (map (fn [angle] (angle-and-length-to-position (identity {:angle angle :length length}))) 
          (evenly-divide-a-circle num-bins wheel-angle))))))




(defn fill-bins [wheel water-levels]
  (map (fn [bin water-level] (assoc bin :grams-of-water  (+ water-level (get bin :grams-of-water 0))))  
  (wheel :bins) water-levels))



(defn determine-fill [wheel source]
  ;(println "in determine fill")
  ;(println wheel)
  ;(println source)
  (map (fn [bin] 
         (if (not (or 
                  (> (source :x-left) (bin :x-right)) 
                  (< (source :x-right) (bin :x-left))
                  (> (bin :y) center-y )
                  ))
            (source :fill-rate) 
            0))
     (wheel :bins)))




(defn draw-water-wheel [wheel src]
  (q/stroke 212 161 106)
  (q/text (str "Wheel Angle: " (:angle wheel)) 20 20)
  (q/text (str "d/fill: " (apply str (determine-fill wheel src))) 20 40)
  (doseq [bin (:bins wheel)]
    (q/line center-x center-y (:x bin) (:y bin))
    (draw-bucket bin)) 
  (q/fill 255 255 255 0)
  (q/stroke 128 76 21)
  (q/ellipse center-x center-y 150 150)) 



(defn draw-source [source]
  (q/stroke 128 76 21)
  (q/quad (source :x-right) 10 (source :x-left) 10 (source :x-left) 40 (source :x-right) 40)
  )

(with-test 
  (defn calculate-single-torque  [center bin]
    (let [x (bin :x) y (bin :y) force (bin :newtons-of-water) c-x (center :x) c-y (center :y)]

    ;Multiply the distance from center by the normalized component
    ;and finally by the force of water due to gravity
    (* 
      
      force 
      
      ;Get the distance from center
      (q/dist x y c-x c-y) 

      ;Subtract 90 and then take sin of it to get the normalized component
      (if (= 0 (- x c-x))
        0.0
        (q/sin 
          (q/radians (-
            (+ 
              ;To make up for the -pi/2 - pi/2 range
              (if (< x 0)
                180  
                0
                )
            ;Get the line angle
              (q/degrees (q/atan (/ (- y c-y) (- x c-x)))))
            90 )))))))
  ;0deg case
  (is (= (* -20.0 30.0 ) (calculate-single-torque {:x 20  :y 20} 
                                             {:x 50 :y 20 :newtons-of-water 20})))
  ;90deg case
  (is (= 0.0 (calculate-single-torque {:x 20 :y 20}
                                    {:x 20 :y 50 :newtons-of-water 42})))
  ;180deg case
  (is (= (* 20.0 30.0) (calculate-single-torque {:x 20 :y 20}
                                           {:x -10 :y 20 :newtons-of-water 20}))))

(with-test
  (defn calculate-moment [{length :length bins :bins}] ;pass a wheel
    (+ base-moment (reduce 
      (fn [val bin]
              (+ val (* (q/sq length) (bin :grams-of-water))))
      0 bins)))
  (is (= 3000.0 (calculate-moment 
              {:length 10 :bins [{:grams-of-water 20} {:grams-of-water 0} {:grams-of-water 10}]}))))


(defn grams-to-newtons-bin [bin]
  (assoc bin :newtons-of-water (* (bin :grams-of-water) 9.8)))

(defn spin-wheel [wheel angle]
  ;(println wheel)
  ;(println angle)
  (let [spun-wheel (create-wheel (count (wheel :bins)) 200 (+ (wheel :angle) angle))]
    (assoc spun-wheel 
      :bins (map 
        (fn [bin old-bin] (assoc bin :grams-of-water (:grams-of-water old-bin)))
        (spun-wheel :bins) (wheel :bins)) 
      :angular-momentum (wheel :angular-momentum start-momentum))))

(defn update-wheel-state [wheel]
  (println wheel)
  (let [torque (reduce + 
                        (map 
                          (fn [bin] (calculate-single-torque {:x center-x :y center-y} (grams-to-newtons-bin bin))) (wheel :bins)))
        moment (calculate-moment wheel)]
  ;angular_momentum = old_angular_momentum + (tourque * time_step)
  ;angle = (old_angular_momentum/moment*time_step) + .5*(torque/moment)*t^2

    (let [d-angle (+ (* (wheel :angular-momentum start-momentum) (/ 1 moment) time-step)
                     (* 0.5 (/ torque moment) (q/sq time-step)))]
          (assoc 
            (spin-wheel wheel d-angle)
            :angular-momentum (+ (wheel :angular-momentum start-momentum) (* torque time-step)))))) 



(defn draw-water [source fill-array]
  (q/stroke 0 0 255 0)
  (q/fill 0 0 255 50)
  (if (= 0 (reduce + fill-array))
  (q/quad (source :x-right) 40 (source :x-left) 40 (source :x-left) 290 (source :x-right) 290)  
  (q/quad (source :x-right) 40 (source :x-left) 40 (source :x-left) 100 (source :x-right) 100)))


(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  (println "In update-state")
  (println (state :wheel))
  (let [wheel (update-wheel-state 
    (assoc (state :wheel) :bins 
      (fill-bins
        (state :wheel)
        (determine-fill (state :wheel) (create-default-source)))))] 
  (println "After update-wheel-state")
  (println wheel)
  {:wheel wheel  :source (create-default-source) :fill-array (determine-fill wheel (create-default-source))}))


(defn draw-state [state]
  (q/background 255 255 255)
  (q/stroke-weight  10)       ;; Set the stroke thickness randomly
  (q/fill 0 0 0 ) 
  ; Clear the sketch by filling it with light-grey color.
  ; Draw the wheel
  (draw-water-wheel (state :wheel) (state :source))
  (draw-source (state :source))
  (draw-water (state :source) (state :fill-array)))
 

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:wheel (create-wheel 6 200 0) :source (create-default-source)})


(q/defsketch water_wheel
  :title "Non Linear Water Wheel"
  :size [1080 720]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
