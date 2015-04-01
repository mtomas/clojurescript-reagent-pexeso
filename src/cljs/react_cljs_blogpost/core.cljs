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
(defn is-matched [card] (= (:matched @card) true))
(defn is-visible [card] (= (:visible @card) true))
(defn is-revealed [card] (is-visible card))

(defn revealed-cards-count [] (count (filter is-visible (:cards @state))))
(defn matched-cards-count [] (count (filter is-matched (:cards @state))))

; side effecting functions used for hiding/marking revealed cards
(defn hide-nonmatch! [] (mapv #(swap! % assoc :visible false) (:cards @state)) (swap! state assoc :last-symbol ""))
(defn mark-match! [symbol] (mapv
                             #(swap! % assoc :matched true)
                             (filterv #(= (:symbol @%) symbol) (:cards @state))))

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
                     (= (:last-symbol @state)
                        (:symbol @card-state)))
              (mark-match! (:symbol @card-state)))

            ;let's remember last symbol to make comparison in subsequent steps
            (swap! state assoc :last-symbol (:symbol @card-state)))]

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
      (:symbol @card-state)]]))

(defn home-page []
  [:div.pexeso
   [:h2 "Pexeso"]
   [:div.status
    (if (= (matched-cards-count) board-size)
      "Game is finished, congratulations !")]

   [:br]

   [:button.button {:onClick start-game} "Restart game"]

   [:div.board
    (doall (for [card-state (:cards @state)]
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