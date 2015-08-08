(define sidebar
  (lambda (category tags)
    `(div (@ (class "well sidebar-nav"))
	  (h2 (@ (id "nav-pills")) "Category")
	  (ul (@ (class "nav nav-pills nav-stacked"))	      
	      ,(map (lambda (x)
		      `(li (a (@ (href "/categories/xxx")) "name")))
		    cotegory))
	  (h2 (@ (id "nav-pills")) "Tags")
	  ,(map (lambda (x)
		  `(a (@ (href "/tags/xxx"))
		      (@ (class "btn btn-info btn-xs"))
		      "name"))
		tags))))

#|
(define disqus
  (lambda ()
    `(aside (@ (id "comments"))
	    (div (h2 "Comments"))
	    
	    (div (@ (id "disqus_thread")))
	    (script (@ (type "text/javascript"))
   
var disqus_shortname = '{{ .Site.DisqusShortname }}';
    var disqus_identifier = '{{with .GetParam "disqus_identifier" }}{{ . }}{{ else }}{{ .Permalink }}{{end}}';
    var disqus_title = '{{with .GetParam "disqus_title" }}{{ . }}{{ else }}{{ .Title }}{{end}}';
    var disqus_url = '{{with .GetParam "disqus_url" }}{{ . | html  }}{{ else }}{{ .Permalink }}{{end}}';

    (function() {
        var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
        dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();

)

(noscript "Please enable JavaScript to view the " (a (@ (href "http://disqus.com/?ref_noscript")) "comments powered by Disqus." ))
(a (@ (href "http://disqus.com")
      (class "dsq-brlink"))
   "comments powered by " (span (@ (class "logo-disqus")) "Disqus"))

	    )))
|#

(define article
  (lambda (title date content tags prev next)
    `(div (@ (class "container"))
	  (div (@ (class "row row-offcanvas row-offcanvas-right"))
	       (div (@ (class "col-sm-9"))
		    (div (@ (id "content"))
			 (h1 (@ (id "Title")) ,title)
			 (p ,date)
			 
			 ,content
			 
			 )
		    
		    (div
		     ,(map (lambda (x)
			     `(a (@ (href "/tags/xxx")
				    (class "btn btn-info btn-xs"))
				 x))
			   tags)

		     (ul (@ (class "pager"))
			 `(if ,prev
			      (li (@ (class "previous"))
				  (a (@ (href "xxx")) "上一篇:" ,prev)))
			 `(if ,next
			      (li (@ (class "next"))
				  (a (@ (href "xxx")) "下一篇:" ,next))))
		     
		     ;; disqus here

		     )

		    (div (@ (class "col-sm-3")
			    (id "sidebar")
			    (role "navigation"))
			 ,(sidebar '() '()))
		    )))
    ))
	  
(define page
  (lambda (title content)
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
			       (li (a (@ href "http://github.com/tiancaiamao") "Project"))
			       (li (a (@ href "/about") "About"))
			       (li (a (@ href "/feed.atom") "Rss")))))
		 
		 ,content

		 )

	    (hr)
	    
	    (footer
	     (p (@ (class "nav navbar-nav navbar-right"))
		"Powered by " (a (@ (href "http://www.call-cc.org")) "chicken")
		"& " (a (@ (href "http://getbootstrap.com/")) "Bootstrap")))
	    
	    ) ;; end body
	   ) ;; end html
    ))
