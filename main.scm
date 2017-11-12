(use spiffy lowdown sxml-transforms intarweb uri-common files medea srfi-69 vector-lib srfi-13 srfi-1 atom)

(load "config.scm")

(define (build-index)
  (with-input-from-file (string-append content-path "/index.json")
    (lambda ()
      (read-json))))

(define (build-category index)
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
     index)
    ret))

(define (build-tags index)
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
     index)
    ret))

(define INDEX (build-index))
(define CATEGORY (build-category INDEX))
(define TAGS (build-tags INDEX))

(define check-index-update
  (let ((last-update (time->seconds (current-time))))
    (lambda ()
      (let ((now (time->seconds (current-time))))
        (when (> (- now last-update) 180)
              (set! last-update now)
              (set! INDEX (build-index))
              (set! CATEGORY (build-category INDEX))
              (set! TAGS (build-tags INDEX)))))))

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
                            (article title date content tags prev next filename)))))
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
                             (article title date 'THIS_IS_FOR_REPLACE tags prev next filename)))))
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


(define (static-template-handler name file)
  (send-sxml (page name (container
                         `(div (@ (id "content"))
                               ,(read-markdown-as-sxml (string-append template-path "/" file))
                               )))))

(define (about-handler)
  (static-template-handler "About" "about.md"))

(define (project-handler)
  (static-template-handler "Project" "project.md"))

(define (home-handler)
  (send-sxml (page "Arthur的博客" (container '(p "hello world")))))

(define (read-markdown-as-sxml filename)
  (with-input-from-file filename
    (lambda ()
      (receive (ret _) (markdown->sxml (current-input-port)) ret))))

(define (atom-entries)
  (let loop ((i 0)
             (count 0)
             (ret '()))
    (if (< count 10)
        (let* ((idx (vector-ref INDEX i))
               (file (item 'File idx)))
          (if (string-suffix-ci? ".md" file)
              (loop (+ i 1)
                    (+ count 1)
                    (cons (make-entry
                           title: (make-title (item 'Title idx))
                           links: (list (make-link type: 'html
                                                   uri: (string-append "http://www.zenlife.tk/" file)))
                           id: (string-append "www.zenlife.tk/" file)
                           updated: (item 'Date idx)
                           published: (item 'Date idx)
                           authors: (list (make-author name: "Arthur"
                                                       uri: "http://www.zenlife.tk"
                                                       email: "tiancaiamao@gmail.com"))
                           content: (make-content
                                     (sxml->html/string (read-markdown-as-sxml (string-append content-path "/" file)))
                                     type: 'html)) ret))
              (loop (+ i 1) count ret)))
        ret)))

(define (atom-handler)
  (send-response
   body:
   (with-output-to-string
     (lambda ()
       (write-atom-doc
        (make-atom-doc
         (make-feed
          title: (make-title "Arthur的博客")
          subtitle: (make-subtitle "伟大的野心家，实践家")
          updated: "2005-07-31T12:29:29Z"
          id: "www.zenlife.tk"
          authors: (list (make-author name: "Arthur" email: "tiancaiamao@gmail.com"))
          links: (list (make-link relation: "self"
                                  type: "application/atom+xml"
                                  uri: "http://www.zenlife.tk/feed.atom"))
          rights: (make-rights "Copyright (c) 2015, Arthur Mao")
          entries: (atom-entries))))))))

(define style-str
  (apply string-append
         '("@import url(https://fonts.googleapis.com/css?family=Yanone+Kaffeesatz);\n"
           "@import url(https://fonts.googleapis.com/css?family=Droid+Serif:400,700,400italic);\n"
           "@import url(https://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,400italic);\n"
           "\tbody { font-family: 'Droid Serif'; }\n"
           "\th1, h2, h3 {\n"
           "\t\tfont-family: 'Yanone Kaffeesatz';\n"
           "\t\tfont-weight: normal;\n"
           "\t}\n"
           "\t.remark-code, .remark-inline-code { font-family: 'Ubuntu Mono'; }\n"
           )))

(define (slide-handler file)
  (send-response
   body:
   (with-output-to-string
     (lambda ()
       (SRV:send-reply (pre-post-order
                        (pre-post-order (slide)
                                        `(($STYLE$ . ,(lambda (tag) style-str))
                                          ($SCRIPT$ . ,(lambda (tag)
                                                         (string-append "var slideshow = remark.create({\n\tsourceUrl: 'content/" file "'\n});")))
                                          (*text* . ,(lambda (tag str) str))
                                          (*default* . ,(lambda x x))))
                        universal-conversion-rules))))))

(define router
  (lambda (continue)
    (let* ((req (current-request))
           (uri (request-uri req))
           (path (uri-path uri))
           (pl (cdr path)))
      (if (null? (cdr pl))
          (let ((p (car pl)))
            (check-index-update)
            (cond
             ((string=? p "") (home-handler))
             ((string=? p "index") (blog-handler))
             ((string=? p "about") (about-handler))
             ((string=? p "project") (project-handler))
             ((string=? p "category") (summery-handler (uri-query uri) CATEGORY "category"))
             ((string=? p "tags") (summery-handler (uri-query uri) TAGS "tags"))
             ((string=? p "feed.atom") (atom-handler))
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
             ((string-suffix-ci? ".slide" p)
              (slide-handler p))
             (else
              ((handle-not-found) path))))
          (parameterize ((root-path project-path))
                        (continue))))))

(vhost-map `((".*" . ,router)))
(server-port PORT)
(start-server)
