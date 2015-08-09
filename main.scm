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
			    (hash-table-set! ret str (cons x '())))))
		(cdr found)))))
     INDEX)
    ret))

(define (item field x)
  (cdr (assq field x)))


(include "template/root.scm")

(define (sxml->html/string sxml)
  (with-output-to-string 
    (lambda () 
      (SXML->HTML sxml))))

(define (send-sxml sxml)
  (let ((body (sxml->html/string sxml)))
    (send-response body: body)))

(define (myfind str vec)
  (let loop ((idx 0))
    (if (< idx (vector-length vec))
	(let ((x (vector-ref vec idx)))
	  (if (string=? (item 'File x) str)
	      idx
	      (loop (+ idx 1))))
	#f)))

(define (md-handler filename)    
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

(define (read-file-as-string filename)
  (with-output-to-string
    (lambda ()
      (with-input-from-file filename
	(lambda ()
	  (copy-port (current-input-port) (current-output-port)))))))

(define (html-handler filename)   
  (define (my-string-replace str from to)
    (let ((start (string-contains str from)))
      (when start
	    (let ((end (+ start (string-length from))))
	      (string-replace str to start end)))))
  
  (let ((idx (myfind (substring filename 1 (string-length filename)) INDEX))) 
    (if idx
	(let* ((data (vector-ref INDEX idx))
	       (title (item 'Title data))
	       (date (string-take (item 'Date data) 10))
	       (tags-vec (item 'Tags data))
	       (tags (if (vector? tags-vec) (vector->list tags-vec) '()))
	       (prev (if (> idx 0) (vector-ref INDEX (- idx 1)) #f))
	       (next (if (< idx (- (vector-length INDEX) 1)) (vector-ref INDEX (+ idx 1)) #f))
	       (content (read-file-as-string (string-append content-path filename)))
	       (sxml (sxml->html/string
		      (page title
			    (container
			     (article title date 'THIS_IS_FOR_REPLACE tags prev next)))))
	       (body (my-string-replace sxml "THIS_IS_FOR_REPLACE" content)))
	  (send-response body: body))
	((handle-not-found) filename))))

(define (summery-handler query type title)
  (let ((found (assq 'name query)))
    (or (and found
	     (let ((cate (cdr found)))
	       (if (hash-table-exists? type cate)
		   (send-sxml (page title
				    (container (blog (hash-table-ref type cate)))))
		   #f)))
	((handle-not-found) query))))

(define (blog-handler)
  (send-sxml (page "blog" 
		   (container (blog (vector->list INDEX))))))

(define (about-handler)
  (send-sxml (page "About" (container (about)))))

(define (home-handler)
  (send-sxml (page "Arthur的博客" (container '(p "hello world")))))

(define router
  (lambda (continue)
    (let* ((req (current-request))
	   (uri (request-uri req))
	   (path (uri-path uri))
	   (pl (cdr path)))
      (if (null? (cdr pl))
	  (let ((p (car pl)))
	    (cond
	     ((string=? p "") (home-handler))
	     ((string=? p "index") (blog-handler))
	     ((string=? p "about") (about-handler))
	     ((string=? p "category") (summery-handler (uri-query uri) CATEGORY "category"))
	     ((string=? p "tags") (summery-handler (uri-query uri) TAGS "tags"))
	     ((string=? p "feed.atom") 6)
	     ((string-suffix-ci? ".md" p)
	      (parameterize ((file-extension-handlers 
			      `(("md" . ,md-handler)))
			     (root-path content-path))
			    (continue)))
	     ((string-suffix-ci? ".html" p)
	      (parameterize ((file-extension-handlers 
			      `(("html" . ,html-handler)))
			     (root-path content-path))
			    (continue)))
	     (else
	      ((handle-not-found) path))))
	  (parameterize ((root-path project-path))
			(continue))))))

(server-port 8088)

(vhost-map `((".*" . ,router)))
