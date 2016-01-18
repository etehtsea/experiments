(ns guessing-game.core
  (:gen-class))

(def secret-number (inc (rand-int 100)))

(defn next-guess []
  (Integer/parseInt (read-line)))

(defn -main [& args]
  (println "Guess the number!")
  (println "Please input your guess:")

  (loop [guess (next-guess)]
    (println "You guessed:" guess)

    (case (compare guess secret-number)
      -1 (do (println "Too small!") (recur (next-guess)))
      1 (do (println "Too big!") (recur (next-guess)))
      0 (println "You win!"))))
