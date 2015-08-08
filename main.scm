(use spiffy lowdown sxml-transforms intarweb uri-common files)

(load "template/root.scm")

(define (md-handler filename)
  (with-input-from-file (string-append (root-path) filename)
    (lambda ()
      (receive (content _) (markdown->sxml (current-input-port))
	       (let ((body (with-output-to-string 
			     (lambda () 
			       (SXML->HTML (page "title" (cons content '()) '() '()))))))
		 (send-response body: body))))))

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
	     ((string=? p "index") 2)
	     ((string=? p "about") 3)
	     ((string=? p "category") 4)
	     ((string=? p "tags") 5)
	     ((string=? p "feed.atom") 6)
	     ((string-suffix-ci? ".md" p)
	      (parameterize ((file-extension-handlers 
			      `(("md" . ,md-handler)))
			     (root-path "/Users/genius/project/src/github.com/tiancaiamao/go.blog/content"))
			    (continue)))
	     (else
	      ((handle-not-found) path))))
	  (parameterize ((root-path "/Users/genius/project/src/github.com/tiancaiamao/go.blog/"))
			(continue))))))

(server-port 8088)

(vhost-map `((".*" . ,router)))
