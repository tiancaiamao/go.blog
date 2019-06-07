(ns blog.core
  (:require [clojure.data.json :as json]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.reload :refer [wrap-reload]]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [compojure.handler]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [markdown-to-hiccup.core :as md]
            [blog.template :refer :all]
            [blog.config :refer :all])
  (:gen-class))

(defn build-index []
  (with-open [r (clojure.java.io/reader (str content-path "index.json"))]
    (json/read r)))

(defn insert-to-ret
  [set arr x]
  (loop [tmp set
         input arr]
    (if (> (count input) 0)
      (recur (update tmp (first input) conj x) (rest input))
      tmp)))

(defn build-tags [index]
  (loop [ret {}
         input index]
    (if (> (count input) 0)
      (let [x (first input)
            found (get x "Tags")]
        (if (and (vector? found) (> (count found) 0))
          (recur (insert-to-ret ret found x) (rest input))
          (recur ret (rest input))
          ))
      ret)))

(defn build-category [index]
  (loop [ret {}
         input index]
    (if (> (count input) 0)
      (let [x (first input)
            found (get x "Category")]
        (if (and (string? found) (> (count found) 0))
          (recur (update ret found conj x) (rest input))
          (recur ret (rest input))))
      ret)))

(def INDEX (build-index))
(def TAGS (build-tags INDEX))
(def CATEGORY (build-category INDEX))

(defn container0 [content]
  (container content (keys CATEGORY) (keys TAGS)))

(defn static-template-handler [name file]
  (html (page name (container0
                    [:div {:id "content"}
                     (md/file->hiccup (str template-path "/" file))
                     ]))))

(defn about-handler [request]
  (static-template-handler "About" "about.md"))

(defn project-handler [request]
  (static-template-handler "Project" "project.md"))

(defn home-handler
  [request]
  (html (page "Arthur的博客" (container0 [:p "hello world"]))))

(defn myfind [str vec]
  (loop [idx 0]
    (if (< idx (count vec))
      (let [x (nth vec idx)]
        (if (= (get x "File") str)
          idx
          (recur (+ idx 1))))
      false)))

(defn md-handler [filename]
  (let [idx (myfind filename INDEX)]
    (if idx
      (let [data (nth INDEX idx)
            title (get data "Title")
            date (subs (get data "Date") 0 10)
            tags (get data "Tags")
            prev (if (> idx 0) (nth INDEX (- idx 1)) false)
            next (if (< idx (- (count INDEX) 1)) (nth INDEX (+ idx 1)) false)
            content (-> (str content-path filename)
                        (md/file->hiccup)
                        (md/component))]
        (page title
              (container0
               (article title date content tags prev next filename))))
      (route/not-found filename))))

(defn blog-item [x]
  (list [:h2 [:a {:href (get x "File")} (get x "Title")]]
        [:div {:class "meta"} (subs (get x "Date") 0 10)]))

(defn blog-handler
  [request]
  (html (page "blog"
              (container0
               (map blog-item INDEX)))))

(defn summery-handler [name type title]
  (fn [request]
    (let [found (get type name)]
      (if found
        (html (page title
                    (container0 (map blog-item found))))
        (route/not-found name)))))

(defroutes app
  (GET "/" [] home-handler)
  (GET "/index" [] blog-handler)
  (GET "/about" [] about-handler)
  (GET "/project" [] project-handler)
  (GET ["/:filename.md" :filename #".*"] [filename]
       (fn [request] (html (md-handler (str filename ".md")))))
  (GET "/category" [name] (summery-handler name CATEGORY "category"))
  (GET "/tags" [name] (summery-handler name TAGS "tags"))
  ;; (GET "/feed.atom" [] atom-handler)
  (route/resources "/")
  (route/not-found "<h1> 404 page no found</h1>"))

(defn -main
  "A very simple web server using Ring & Jetty"
  [port-number]
  (jetty/run-jetty
   (compojure.handler/api app)
   {:port (Integer. port-number)}))

(defn -dev-main
  "A very simple web server using Ring & Jetty that reloads code changes via the development profile of Leiningen"
  [port-number]
  (jetty/run-jetty
   (wrap-reload
    (compojure.handler/api #'app))
   {:port (Integer. port-number)}))
