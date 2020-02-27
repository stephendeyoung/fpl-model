(ns fpl.scoring)

(def base-conversion 0.13)
(def min-shots 1.5)

(def rules
  (let [gk-def-goal (fn [player-xg]
                      ;(if (> player-xg 0.5)
                        ;(+ (/ (- player-xg 0.5) 2) 0.5)
                        (* player-xg 6))
        mid-goal (fn [player-xg] (* player-xg 5))
        fwd-goal (fn [player-xg] (* player-xg 4))
        assist (fn [player-xa] (* player-xa 3))
        gk-def-clean-sheet (fn [team-xg-conceded]
                             (cond
                               (= team-xg-conceded 0) 0
                               (< team-xg-conceded 1) (- 4 (* team-xg-conceded 4))
                               (>= team-xg-conceded 2) (- (/ (- 2 team-xg-conceded) 2) 1)
                               (>= team-xg-conceded 1) (- 1 team-xg-conceded)))]

        {
      :gk-scores (fn [team-xg-conceded gsaa]
                       (+ (gk-def-clean-sheet team-xg-conceded)
                          gsaa))
         :def-scores (fn [player-xg player-xga team-xg-conceded]
                       (+ (gk-def-goal player-xg)
                          (gk-def-clean-sheet team-xg-conceded)
                          (assist player-xga)))
         :mid-scores (fn [player-xg player-xga team-xg-conceded player-shots player-conversion]
                       (+ (mid-goal player-xg)
                          (cond
                            (= team-xg-conceded 0) 0
                            (< team-xg-conceded 0) 1
                            (< team-xg-conceded 1) (- 1 team-xg-conceded)
                            :else 0)
                          (assist player-xga)))
                          ;(if (or (< player-shots min-shots) (<= player-conversion base-conversion))
                          ;  0
                          ;  (* (- player-conversion base-conversion) 5))))
         :fwd-scores (fn [player-xg player-xga player-shots player-conversion]
                       (+ (fwd-goal player-xg)
                          (assist player-xga)))
                          ;(if (or (< player-shots min-shots) (<= player-conversion base-conversion))
                          ;  0
                          ;  (* (- player-conversion base-conversion) 4))))

      }))