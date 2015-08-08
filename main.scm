(use spiffy lowdown sxml-transforms intarweb uri-common files medea)

(include "template/about.scm")
(include "template/root.scm")

(define project-path "/Users/genius/project/src/github.com/tiancaiamao/go.blog/")
(define content-path (string-append project-path "content"))

(define (send-sxml sxml)
  (let ((body (with-output-to-string 
		(lambda () 
		  (SXML->HTML sxml)))))
    (send-response body: body)))

(define (md-handler filename)
  (with-input-from-file (string-append (root-path) filename)
    (lambda ()
      (receive (content _) (markdown->sxml (current-input-port))
	       (let ((body (with-output-to-string 
			     (lambda () 
			       (SXML->HTML (page "title" (cons content '()) '() '()))))))
		 (send-response body: body))))))

(define (index->sxml data)
  (define (item field x)
    (cdr (assq field x)))
  (define fn
    (lambda (x)
      `(p (@ (class "blogtitle"))
	  (a (@ (href ,(item 'File x)))
	     ,(item 'Title x))
	  (br (span (@ (class "date")) (item 'Date x))))))
  (map fn data))

(define (blog-handler)
  (with-input-from-file (string-append content-path "/index.json")
    (lambda ()
      (let* ((data (read-json))
	     (bloglist (index->sxml (vector->list data)))
	     (content (list '(h1 (@ (class "title")) "Article index") bloglist)))
	(let ((body (with-output-to-string 
		      (lambda () 
			(SXML->HTML (page "title" content '() '()))))))
	  (send-response body: body))))))

(define (about-handler)
  (let ((body (with-output-to-string 
		(lambda () 
		  (SXML->HTML (page "About" (cons about '()) '() '()))))))
    (send-response body: body)))

(define router
  (lambda (continue)
    (let* ((req (current-request))
	   (uri (request-uri req))
	   (path (uri-path uri))
	   (pl (cdr path)))
      (if (null? (cdr pl))
	  (let ((p (car pl)))
	    (cond
	     ((string=? p "") 1)
	     ((string=? p "index") (blog-handler))
	     ((string=? p "about") (about-handler))
	     ((string=? p "category") 4)
	     ((string=? p "tags") 5)
	     ((string=? p "feed.atom") 6)
	     ((string-suffix-ci? ".md" p)
	      (parameterize ((file-extension-handlers 
			      `(("md" . ,md-handler)))
			     (root-path content-path))
			    (continue)))
	     (else
	      ((handle-not-found) path))))
	  (parameterize ((root-path "/Users/genius/project/src/github.com/tiancaiamao/go.blog/"))
			(continue))))))

(server-port 8088)

(vhost-map `((".*" . ,router)))
