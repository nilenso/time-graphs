(ns harvest-times.core
  (:require [clojure.string :as s]
            [clj-http.client :as http]
            [incanter.charts :as charts]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-time.predicates :as tpr]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]
            [clj-time.periodic :as tp])
  (:use [incanter core stats charts io])
  (:import [org.jfree.chart.axis CategoryLabelPositions]))

(defn key-by [kfn coll]
  (zipmap (map kfn coll)
          coll))

(defn get-request [url query-params]
  (-> (http/get (str "https://nilenso.harvestapp.com" url)
                {:headers {"Accept" "application/json"
                           "Authorization" "Basic <fill this>"}
                 :query-params query-params})
      (update-in [:body] json/decode true)))

(defn who-am-i []
  (get-request "/account/who_am_i" {}))

(defn people []
  (->> (get-request "/people" {})
       :body
       (map :user)
       (filter :is_active)
       (remove :is_contractor)))

(defn projects []
  (->> (get-request "/projects" {})
       :body
       (map :project)
       (key-by :id)))

(defn tasks []
  (->> (get-request "/tasks" {})
       :body
       (map :task)
       (key-by :id)))

(defn int-date [date]
  (->> date
       tc/to-long
       (* 1/1000)
       int))

(defn parse-date [date-str]
  (->> date-str
       (tf/parse (tf/formatter "yyyy-MM-dd"))
       int-date))

(defn time-entries [project-info task-info user-id from to]
  ;; from and to need to be in YYYYMMDD format
  (->> (get-request (format "/people/%s/entries" user-id)
                    {:from from
                     :to to})
       :body
       (map :day_entry)
       (map #(assoc % :time-int (parse-date (:spent_at %))))
       (map #(assoc % :project-name (:name (get project-info (:project_id %)))))
       (map #(assoc % :task-name (:name (get task-info (:task_id %)))))))

(defn people-entries [from to]
  (let [project-info (projects)
        task-info (tasks)]
    (->>
     (doall
      (pmap (fn [{:keys [first_name id] :as p}]
              {first_name (time-entries project-info task-info id from to)})
            (people)))
     (into {}))))

(defn no-space [s]
  (s/replace s #"\s" "-"))

(defn graphite-entry-name [project-name task-name person-name]
  (->> [(no-space project-name)
        (no-space task-name)
        (no-space person-name)]
       (s/join ".")
       s/lower-case ))

(defn ->graphite [person-name {:keys [project-name task-name hours time-int] :as entry}]
  (s/join " "
          [(graphite-entry-name project-name task-name person-name)
           hours
           time-int]))

(defn graphite-entries [from to]
  (let [filename (str "synthesize-master/harvest-" from "-" to)]
    (->> (people-entries from to)
         (mapcat (fn [[p entries]] (map #(->graphite p %) entries)))
         (s/join "\n")
         (spit filename))
    filename))

(defn weekends [from num-days]
  (->> (t/days 1)
       (tp/periodic-seq (tf/parse (tf/formatter "yyyyMMdd") from))
       (take num-days)
       (filter tpr/weekend?)
       (map int-date)))

(defn graphite-weekends [from num-days]
  (let [filename (str "synthesize-master/harvest-weekends-" from "-" num-days)]
    (->> (weekends from num-days)
         (map (fn [ts] (s/join " " ["weekend" 8 ts])))
         (s/join "\n")
         (spit filename))
    filename))

(defn save-plot [prefix plot]
  (let [filename (str "/tmp/" prefix "-" (gensym) ".png")]
    (save plot filename :width 1200)
    filename))

(defn set-vertical-label [chart]
  (-> chart
      .getCategoryPlot
      .getDomainAxis
      (.setCategoryLabelPositions CategoryLabelPositions/UP_90))
  chart)

(defn graph [project-info task-info first_name user-id from to]
  (let [entries (time-entries project-info task-info user-id from to)]
    (with-data (to-dataset entries)
      (-> (bar-chart :spent_at
                     :hours
                     :y-label "hours"
                     :x-label "date"
                     :group-by :project-name
                     :legend true
                     :title (str first_name "- from " from " to " to))
          set-vertical-label))))

(defn people-graphs [from to]
  (let [project-info (projects)
        task-info (tasks)]
    (doall
     (pmap (fn [{:keys [first_name id] :as p}]
             (save-plot first_name (graph project-info task-info first_name id from to)))
           (people)))))
