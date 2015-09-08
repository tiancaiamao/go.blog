(define sidebar
  (lambda ()
    `(div (@ (class "well sidebar-nav"))
	  (h2 (@ (id "nav-pills")) "Category")
	  (ul (@ (class "nav nav-pills nav-stacked"))
	      ,(map (lambda (name)
		      `(li (a (@ (href ,(string-append "/category?name=" name))) ,name)))
		    (hash-table-keys CATEGORY)))
	  (h2 (@ (id "nav-pills")) "Tags")
	  ,(map (lambda (name)
		  `(a (@ (href ,(string-append "/tags?name=" name))
			 (class "btn btn-info btn-xs"))
		      ,name))
		(hash-table-keys TAGS)))))

(define (disqus title url)
  (define script
    (apply string-append
	   `("\nvar disqus_identifier = 'http://www.zenlife.tk" ,url "';\n"
	     "var disqus_title = '" ,title "';\n"
	     "var disqus_url = 'http://www.zenlife.tk" ,url "';\n\n"
	     "(function() {\n"
	     "\tvar dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;\n"
	     "\tdsq.src = '//codingnow.disqus.com/embed.js';\n"
	     "\t(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);\n"
	     "})();\n")))

  `(aside (@ (id "comments"))
	  (div (h2 "Comments"))
	  (div (@ (id "disqus_thread")))
	  (script (@ (type "text/javascript"))
		  ,script
		  )
	  (noscript "Please enable JavaScript to view the " (a (@ (href "http://disqus.com/?ref_noscript")) "comments powered by Disqus."))
	  (a (@ (href "http://disqus.com")
		(class "dsq-brlink")) "comments powered by"
		(span (@ (class "logo-disqus")) "Disqus"))))

(define article
  (lambda (title date content tags prev next permlink)
    `((div (@ (id "content"))
	   (h1 (@ (id "Title")) ,title)
	   (p ,date)

	   ,content

	   )
      (div
       ,(map (lambda (x)
	       `(a (@ (href ,(string-append "/tags?name=" x))
		      (class "btn btn-info btn-xs"))
		   ,x))
	     tags)

       (ul (@ (class "pager"))
	   ,(if prev
		`(li (@ (class "previous"))
		     (a (@ (href ,(item 'File prev))) "上一篇:" ,(item 'Title prev)))
		'())
	   ,(if next
		`(li (@ (class "next"))
		     (a (@ (href ,(item 'File next))) "下一篇:" ,(item 'Title next)))
		'())))

      ;; disqus here
      ,(disqus title permlink)
      )
    ))

(define blog
  (lambda (data)
    (map (lambda (x)
	   (let ((title (item 'Title x))
		 (file (item 'File x))
		 (date (item 'Date x)))
	     `((h2 (a (@ (href ,file)) ,title))
	       (div (@ (class "meta")) ,(string-take date 10)))))
	 data)))

(define page
  (lambda (title container)
    `(html (@ (lang "zh_CN"))
	   (head
	    (meta (@ (charset "utf-8")))
	    (title ,title)
	    (link (@ (rel "stylesheet")
		     (href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"))))
	   (body
	    (div (@ (class "navbar navbar-inverse"))
		 (div (@ (class "container"))
		      (div (@ (class "collapse navbar-collapse"))
			   (ul (@ (class "nav navbar-nav"))
			       (li (a (@ href "/") "Home"))
			       (li (a (@ href "/index") "Blog"))
			       (li (a (@ href "/project") "Project"))
			       (li (a (@ href "/about") "About"))
			       (li (a (@ href "/feed.atom") "Rss"))))))

	    ,container

	    (hr)

	    (footer
	     (p (@ (class "nav navbar-nav navbar-right"))
		"Powered by " (a (@ (href "http://www.call-cc.org")) "chicken")
		" & " (a (@ (href "http://getbootstrap.com/")) "Bootstrap")))

	    ) ;; end body
	   ) ;; end html
    ))

(define (container content)
  `(div (@ (class "container"))
	(div (@ (class "row row-offcanvas row-offcanvas-right"))
	     (div (@ (class "col-sm-9"))
		  ,content)
	     (div (@ (class "col-sm-3")
		     (id "sidebar")
		     (role "navigation"))
		  ,(sidebar)))))

(define about
  (lambda ()
    '(div (@ id "content")
	  (div (@ (id "outline-container-1")
		  (class "outline-2"))
	       (h2 (@ (id "sec-1")) "关于我")
	       (div (@ (class "outline-text-2")
		       (id "text-1"))
		    (p
		     (br "2014年，一家DSP公司，DSP是比ASP高端一点点的东西。我终于成为一名后端工程师了")
		     (br "2013年，于某工厂的作坊混了一年生活，从事Android游戏相关的码砖工作")
		     (br "2013年，毕业于华中渴饥大学，混得计算机软件与理论的硕士文凭")
		     (br "2012年，被忽悠到某度打过几天酱油，当时假装在搞机器学习")))

	       (div (@ (id "outline-container-2")
		       (class "outline-2"))
		    (h2 (@ (id "sec-2")) "关于网站")

		    (div (@ (class "outline-text-2")
			    (id "text-2"))
			 (p
			  (br (a (@ (href "http://www.call-cc.org")) "chicken") "写的后端。前端使用了"
			      (a (@ (href "http://getbootstrap.com")) "bootstrap") "模板。文章内容使用markdown格式书写。")
			  (br (a (@ (href "http://github.com/tiancaiamao/go.blog")) "这里") "是这个博客的源代码"))))


	       (div (@ (id "outline-container-3")
		       (class "outline-2"))
		    (h2 (@ (id "sec-3")) "联系我")
		    (div (@ (class "outline-text-2")
			    (id "text-3"))
			 (p (br "可以加我QQ：306345843")
			    (br "或者发邮件：tiancaiamao at gmail dot com"))))))))

(define slide
  (lambda ()
    '(html
      (head
       (title "title")
       (meta (@ (charset "utf-8")))
       (style
	   ($STYLE$)
	 )
       )
      (body
       (textarea (@ (id "source")))
       (script (@ (src "https://gnab.github.io/remark/downloads/remark-latest.min.js")))
       (script
	($SCRIPT$)
	)
       )
      )))
