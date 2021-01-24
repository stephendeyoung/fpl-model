(ns fpl.scoring)

(def base-conversion 0.13)
(def min-shots 1.5)

(def rules
  (let [gk-def-goal (fn [probability]
                      ;(if (> probability 0.5)
                      ;(+ (/ (- probability 0.5) 2) 0.5)
                      (* probability 6))
        mid-goal (fn [probability] (* probability 5))
        fwd-goal (fn [probability] (* probability 4))
        penalty-miss (fn [probability] (* probability -2))
        assist (fn [player-xa] (* player-xa 3))
        gk-def-clean-sheet (fn [team-xg-concededs]
                             (reduce #(+ %1 %2)
                                     (mapv (fn [team-xg-conceded]
                                             (cond
                                               (= team-xg-conceded 0) 0
                                               (< team-xg-conceded 1) (- 4 (* team-xg-conceded 4))
                                               (>= team-xg-conceded 2) (- (/ (- 2 team-xg-conceded) 2) 1)
                                               (>= team-xg-conceded 1) (- 1 team-xg-conceded)))
                                           team-xg-concededs)))]

    {
     :gk-scores  (fn [team-xg-conceded gsaa appearance-pts bonus-pts]
                   (+ (gk-def-clean-sheet team-xg-conceded)
                      gsaa
                      appearance-pts
                      bonus-pts))
     :def-scores (fn [player-xg player-xga team-xg-conceded appearance-pts bonus-pts penalty-player-score-probability
                      penalty-player-miss-probability]
                   (+ (gk-def-goal player-xg)
                      (gk-def-goal penalty-player-score-probability)
                      (penalty-miss penalty-player-miss-probability)
                      (gk-def-clean-sheet team-xg-conceded)
                      (assist player-xga)
                      appearance-pts
                      bonus-pts))
     :mid-scores (fn [player-xg player-xga team-xg-concededs appearance-pts bonus-pts penalty-player-score-probability
                      penalty-player-miss-probability]
                   (+ (mid-goal player-xg)
                      (mid-goal penalty-player-score-probability)
                      (penalty-miss penalty-player-miss-probability)
                      (reduce #(+ %1 %2)
                              (mapv (fn [team-xg-conceded]
                                      (cond
                                        (= team-xg-conceded 0) 0
                                        (< team-xg-conceded 0) 1
                                        (< team-xg-conceded 1) (- 1 team-xg-conceded)
                                        :else 0))
                                    team-xg-concededs))
                      (assist player-xga)
                      appearance-pts
                      bonus-pts))
     ;(if (or (< player-shots min-shots) (<= player-conversion base-conversion))
     ;  0
     ;  (* (- player-conversion base-conversion) 5))))
     :fwd-scores (fn [player-xg player-xga appearance-pts bonus-pts penalty-player-score-probability
                      penalty-player-miss-probability]
                   (+ (fwd-goal player-xg)
                      (fwd-goal penalty-player-score-probability)
                      (penalty-miss penalty-player-miss-probability)
                      (assist player-xga)
                      appearance-pts
                      bonus-pts))
     ;(if (or (< player-shots min-shots) (<= player-conversion base-conversion))
     ;  0
     ;  (* (- player-conversion base-conversion) 4))))

     }))