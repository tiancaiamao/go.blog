// dsl/templates.dsl — go.blog page + atom feed templates in the template DSL.
//
// Output matches the golden fixtures in src-ta/test/baseline/ byte-for-byte;
// the generated KL builds html Node trees (not strings). Attributes use the flat
// surface form; nested SXML `(@ (class "x"))` is accepted too.

(def site-url "https://tiancaiamao.github.io")

(template tp_tag_link (name)
  (a (@ href ,(cat "/tags/" name) class "btn btn-info btn-xs") ,name))

(template sidebar (category tags)
  (div (@ class "well sidebar-nav")
    (h2 (@ id "nav-pills") "Category")
    (ul (@ class "nav nav-pills nav-stacked")
      (for name in category
        (li (a (@ href ,(cat "/category/" name)) ,name))))
    (h2 (@ id "nav-pills") "Tags")
    (for name in tags
      ,(template.tp_tag_link name))))

(template disqus (title url)
  (aside (@ id "comments")
    (div (h2 "Comments"))
    (div (@ id "disqus_thread"))
    (script (@ type "text/javascript")
      ,(cat "\nvar disqus_identifier = 'http://www.zenlife.tk" url
             "';\nvar disqus_title = '" title
             "';\nvar disqus_url = 'http://www.zenlife.tk" url
             "';\n\n(function() {\n"
             "\tvar dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;\n"
             "\tdsq.src = '//codingnow.disqus.com/embed.js';\n"
             "\t(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);\n"
             "})();\n"))
    (noscript "Please enable JavaScript to view the "
      (a (@ href "http://disqus.com/?ref_noscript") "comments powered by Disqus."))
    (a (@ href "http://disqus.com" class "dsq-brlink")
      "comments powered by"
      (span (@ class "logo-disqus") "Disqus"))))

(template tp_head (title)
  (head
    (meta (@ charset "utf-8"))
    (title ,title)
    (link (@ rel "stylesheet" href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"))
    (link (@ rel "stylesheet" href "/static/style.css"))))

(template tp_nav_item (href label)
  (li (a (@ href ,href) ,label)))

(template tp_navbar ()
  (div (@ class "navbar navbar-inverse")
    (div (@ class "container")
      (div (@ class "collapse navbar-collapse")
        (ul (@ class "nav navbar-nav")
          ,(template.tp_nav_item "/" "Home")
          ,(template.tp_nav_item "/index" "Blog")
          ,(template.tp_nav_item "/project" "Project")
          ,(template.tp_nav_item "/about" "About")
                    ,(template.tp_nav_item "/feed.atom" "Rss"))))))

(template tp_footer ()
  (footer
    (p (@ class "nav navbar-nav navbar-right")
      "Powered by "
      (a (@ href "https://github.com/tiancaiamao/tinyactor") "TinyActor")
      " & "
      (a (@ href "//getbootstrap.com/") "Bootstrap"))))

(template container (content category tags)
  (div (@ class "container")
    (div (@ class "row row-offcanvas row-offcanvas-right")
      (div (@ class "col-sm-9") ,content)
      (div (@ class "col-sm-3" id "sidebar" role "navigation")
                ,(template.sidebar category tags)))))

(template page (title body)
  (html (@ lang "zh_CN")
    ,(template.tp_head title)
    (body
      ,(template.tp_navbar)
      ,body
      (hr)
      ,(template.tp_footer))))

(template article (title date content tags prev next permlink)
  (div (@ id "content")
    (h1 (@ id "Title") ,title)
    (p ,date)
    ,content)
  (div
    (for x in tags ,(template.tp_tag_link x))
    (ul (@ class "pager"))))

// --- atom feed ---

(template atom_entry (title date entry-url html-content)
  (entry
    (title ,title)
    (link (@ href ,entry-url type "text/html" rel "alternate"))
    (id ,entry-url)
    (updated ,date)
    (content (@ type "html") ,html-content)))

(template atom_feed (updated-date entries url)
  (feed (@ xmlns "http://www.w3.org/2005/Atom")
    (title "Arthur 的博客")
    (subtitle "伟大的野心家，实践家")
    (updated ,updated-date)
    (link (@ rel "alternate" type "text/html" href ,url))
    (link (@ rel "self" type "application/atom+xml"
                href ,(cat (site-url) "/feed.atom")))
    (rights "Copyright (c) 2025, Arthur Mao")
    (id ,url)
    (author
      (name "Arthur")
      (email "tiancaiamao@gmail.com"))
    ,entries))