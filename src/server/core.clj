(ns server.core
  (:require [clojure.java.io :as io]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as ring]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]))

;(defn write-to-project-root [filename contents](spit filename contents))

(defn page-layout
  [{:keys [title]} & page-content]
  [:html
   [:head [:title title]]
   [:body [:div {:class "content"} page-content]]])

(defn page-welcome
  []
  (page-layout {:title "Welcome"}
               [:div [:h1 "Hello"]]))

(defn page-404
  []
  (page-layout {:title "Woops..."}
               [:div [:h1 "Page not found"]]))

(defn file->str
  [file]
  "extract contents from a java.io.File as a string"
  (->> file
       io/reader
       line-seq
       doall
       (map clojure.string/trim)
       (clojure.string/join "\n")))

(defn extract-request-file
  "takes a request with a temp file and returns its contents and other meta in a map"
  [{{{:keys [filename tempfile]} "account-export"} :multipart-params}]
  {:filename filename
   :contents (file->str tempfile)})

(defroutes handler
  (route/resources "/")
  (route/not-found "Page not Found"))

(def application
  (wrap-defaults handler
                 (assoc-in site-defaults
                           [:security :anti-forgery]
                           false)))

(defn start [port]
  (ring/run-jetty application {:port port
                               :join? false}))

(defn -main
  [& args]
  (let [port (Integer. (or (System/getenv "PORT") "3000"))]
    (start port)))
