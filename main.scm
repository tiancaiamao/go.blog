(use spiffy lowdown sxml-transforms)

(define (md-handler filename)
  (with-input-from-file (string-append (root-path) filename)
    (lambda ()
      (receive (sxml _) (markdown->sxml (current-input-port))
	       (let ((body (with-output-to-string (lambda () (SXML->HTML sxml)))))
		 (send-response body: body))))))

(server-port 8088)    

(vhost-map `((".*" . 
	      ,(lambda (continue)
		 (parameterize ((file-extension-handlers 
				 `(("md" . ,md-handler)))
				(root-path "/Users/genius/project/src/github.com/tiancaiamao/go.blog/content"))
			       (continue))))))
