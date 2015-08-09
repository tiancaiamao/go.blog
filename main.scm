(use spiffy lowdown sxml-transforms intarweb uri-common files medea srfi-69 vector-lib srfi-13 srfi-1)

(define project-path "/Users/genius/project/src/github.com/tiancaiamao/go.blog/")
(define content-path (string-append project-path "content"))
(define INDEX 
  (with-input-from-file (string-append content-path "/index.json")
    (lambda ()
      (read-json))))

(define CATEGORY
  (let ((ret (make-hash-table)))
    (vector-for-each
     (lambda (i x)
       (let ((found (assq 'Category x)))
	 (when found
	       (let ((str (cdr found)))
		 (when (not (string=? str ""))
		       (if (hash-table-exists? ret str)
			   (hash-table-set! ret str (cons x (hash-table-ref ret str)))
			   (hash-table-set! ret str (cons x '()))))))))
     INDEX)
    ret))

(define TAGS
  (let ((ret (make-hash-table)))
    (vector-for-each 
     (lambda (_ x)
       (let ((found (assq 'Tags x)))
	 (when (and found
		    (not (null? (cdr found)))
		    (vector? (cdr found)))
	       (vector-for-each
		(lambda (_ str)
		  (when (not (string=? str ""))
			(if (hash-table-exists? ret str)
			    (hash-table-set! ret str (cons x (hash-table-ref ret str)))
			    (hash-table-set! ret str (cons x ret)))))
		(cdr found)))))
     INDEX)
    ret))

(define (item field x)
  (cdr (assq field x)))


(include "template/root.scm")

(define (send-sxml sxml)
  (let ((body (with-output-to-string 
		(lambda () 
		  (SXML->HTML sxml)))))
    (send-response body: body)))

(define (md-handler filename)
  (define (myfind str vec)
    (let loop ((idx 0))
      (if (< idx (vector-length vec))
	  (let ((x (vector-ref vec idx)))
	    (if (string=? (item 'File x) str)
		idx
		(loop (+ idx 1))))
	  #f)))
  
  (let ((idx (myfind (substring filename 1 (string-length filename)) INDEX))) 
    (if idx
	(let* ((data (vector-ref INDEX idx))
	       (title (item 'Title data))
	       (date (string-take (item 'Date data) 10))
	       (tags-vec (item 'Tags data))
	       (tags (if (vector? tags-vec) (vector->list tags-vec) '()))
	       (prev (if (> idx 0) (vector-ref INDEX (- idx 1)) #f))
	       (next (if (< idx (- (vector-length INDEX) 1)) (vector-ref INDEX (+ idx 1)) #f))
	       (content (with-input-from-file (string-append content-path filename)
			  (lambda ()
			    (receive (ret _) (markdown->sxml (current-input-port))
				     ret)))))
	  (send-sxml (page title
			   (container
			    (article title date content tags prev next)))))
	((handle-not-found) filename))))

(define (blog-handler)
  (send-sxml (page "blog" 
		   (container (blog (vector->list INDEX))))))

(define (about-handler)
  (send-sxml (page "About" (container (about)))))

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
