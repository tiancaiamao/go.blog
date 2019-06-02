(ns blog.template)

(defn sidebar [category tags]
  [:div {:class "well sidebar-nav"}
   [:h2 {:id "nav-pills"} "Category"]
   [:ul {:class "nav nav-pills nav-stacked"}
    (map (fn [name]
           [:li [:a {:href (str "/category?name=" name)} name]])
         category)]
   [:h2 {:id "nav-pills"} "Tags"]
   (map (fn [name]
          [:a {:href (str "/tags?name=" name)
               :class "btn btn-info btn-xs"}
           name])
        tags)])

(defn disqus [title url]
  (let [script
        (str "\nvar disqus_identifier = 'http://www.zenlife.tk" url "';\n"
             "var disqus_title = '" title "';\n"
             "var disqus_url = 'http://www.zenlife.tk" url "';\n\n"
             "(function() {\n"
             "\tvar dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;\n"
             "\tdsq.src = '//codingnow.disqus.com/embed.js';\n"
             "\t(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);\n"
             "})();\n")]

    [:aside {:id "comments"}
     [:div [:h2 "Comments"]]
     [:div {:id "disqus_thread"}]
     [:script {:type "text/javascript"}
      script
      ]
     [:noscript "Please enable JavaScript to view the "
      [:a {:href "http://disqus.com/?ref_noscript"} "comments powered by Disqus."]]
     [:a {:href "http://disqus.com"
          :class "dsq-brlink"} "comments powered by"
      [:span {:class "logo-disqus"} "Disqus"]]]))

(defn container
  [content category tags]
  [:div {:class "container"}
   [:div {:class "row row-offcanvas row-offcanvas-right"}
    [:div {:class "col-sm-9"}
     content]
    [:div {:class "col-sm-3"
           :id "sidebar"
           :role "navigation"}
     (sidebar category tags)]]])

(defn page
  [title container]
  [:html {:lang "zh_CN"}
   [:head
    [:meta {:charset "utf-8"}]
    [:title title]
    [:link {:rel "stylesheet"
            :href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"}]]
   [:body
    [:div {:class "navbar navbar-inverse"}
     [:div {:class "container"}
      [:div {:class "collapse navbar-collapse"}
       [:ul {:class "nav navbar-nav"}
        [:li [:a {:href "/"} "Home"]]
        [:li [:a {:href "/index"} "Blog"]]
        [:li [:a {:href "/project"} "Project"]]
        [:li [:a {:href "/about"} "About"]]
        [:li [:a {:href "/feed.atom"} "Rss"]]]]]]

    container

    [:hr]

    [:footer
     [:p {:class "nav navbar-nav navbar-right"}
      "Powered by " [:a {:href "https://clojure.org/"} "Clojure"]
      " & " [:a {:href "http://getbootstrap.com/"} "Bootstrap"]]]

    ] ;; end body
   ] ;; end html
  )

(defn article
  [title date content tags prev next permlink]
  (list [:div {:id "content"}
    [:h1 {:id "Title"} title]
    [:p date]

    content

    ]
   [:div
    (map (fn [x]
           [:a { :href (str "/tags?name=" x)
                :class "btn btn-info btn-xs"}
            x]) tags)

    [:ul {:class "pager"}
     (if prev
       [:li {:class "previous"}
        [:a {:href (get prev "File")} "上一篇:" (get prev "Title")]]
       '[])
     (if next
       [:li {:class "next"}
        [:a {:href (get next "File")} "下一篇:" (get next "Title")]]
       '[])]]

   ;; disqus here
   (disqus title permlink)
   )
  )
