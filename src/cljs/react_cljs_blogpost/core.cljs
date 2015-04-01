(ns react-cljs-blogpost.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [cljsjs.react :as react])
  (:import goog.History))

(def board-size 16)
(def alpha-chars (mapv char (range 97 123)))
(defn generate-symbols [] (shuffle (flatten (repeat 2 (take (/ board-size 2) (shuffle alpha-chars))))))

;; -------------------------
(def state (atom {:cards [] :last-symbol nil}))

(defn generate-card [symb] (atom {:symbol symb :visible false :matched false}))
(defn generate-cards [] (mapv generate-card (generate-symbols)))

;couple of helpful predicate functions
(defn is-matched [card] (= (get @card :matched) true))
(defn is-visible [card] (= (get @card :visible) true))
(defn is-revealed [card] (is-visible card))

(defn revealed-cards-count [] (count (filter is-visible (@state :cards))))
(defn matched-cards-count [] (count (filter is-matched (@state :cards))))

; side effecting functions used for hiding/marking revealed cards
(defn hide-nonmatch! [] (mapv #(swap! % assoc :visible false) (@state :cards)) (swap! state assoc :last-symbol ""))
(defn mark-match! [symbol] (mapv
                             #(swap! % assoc :matched true)
                             (filterv #(= (get @% :symbol) symbol) (@state :cards))))

(defn reveal-card! [card-state] (swap! card-state assoc :visible true))
(defn start-game [] (swap! state assoc :cards (generate-cards)))


(defn card [card-state]
  (letfn [(handle-card-click! [event]
            ; pair of cards was revealed, now let's go for another pair step
            (if (= (revealed-cards-count) 2)
              (hide-nonmatch!))

            ;reveal next card in step
            (reveal-card! card-state)

            ;if 2 of cards are revealed, we have to check parity
            (if (and (= (revealed-cards-count) 2)
                     (= (get @state :last-symbol)
                        (get @card-state :symbol)))
              (mark-match! (get @card-state :symbol)))

            ;let's remember last symbol to make comparison in subsequent steps
            (swap! state assoc :last-symbol (@card-state :symbol)))]

    [:div.card
     {:onClick handle-card-click!
      :key     (.random js/Math)
      :class   (if (@card-state :matched)
                 "card-matched"
                 "card")}

     [:span.card-value
      {:class (if (@card-state :visible)
                "card-value"
                "card-value-hidden")}
      (@card-state :symbol)]]))

(defn home-page []
  [:div.pexeso
   [:h2 "Pexeso"]
   [:div.status
    (if (= (matched-cards-count) board-size)
      "Game is finished, congratulations !")]

   [:br]

   [:button.button {:onClick start-game} "Restart game"]

   [:div.board
    (doall (for [card-state (@state :cards)]
             (card card-state)))]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen EventType/NAVIGATE
                   (fn [event] (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-root))
