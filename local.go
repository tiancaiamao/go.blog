// Copyright 2013 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !appengine

// This file implements a stand-alone blog server.

package main

import (
	"flag"
	"log"
	"net/http"
	"os"
)

var (
	httpAddr     = flag.String("http", "localhost:8080", "HTTP listen address")
	contentPath  = flag.String("content", "/app/content/", "path to content files")
	templatePath = flag.String("template", "/app/template/", "path to template files")
	staticPath   = flag.String("static", "/app/static/", "path to static files")
)

func main() {
	flag.Parse()
	port := os.Getenv("PORT")
	if port == "" {//run local
		*contentPath = "../src/blog/content/"
		*templatePath = "../src/blog/template/"
		*staticPath = "../src/blog/static/"
		port = "8080"
	}
	s, err := NewServer(*contentPath, *templatePath)
	if err != nil {
		log.Fatal(err)
	}
	http.Handle("/", s)
	fs := http.FileServer(http.Dir(*staticPath))
	http.Handle("/static/", http.StripPrefix("/static/", fs))
	log.Fatal(http.ListenAndServe(":"+port, nil))
}
