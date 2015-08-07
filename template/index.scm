(page "Article index" 
      `((h1 (@ (class "title")) "Article index")
	,(map (lambda (x)
	       (p (@ (class "blogtitle"))
		  (a (@ (href "x->file")) "Title")
		  (br (span (@ (class "date")) "2006-01-02"))
		  x))
	     data))
      category
      tags)
