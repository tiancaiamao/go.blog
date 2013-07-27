// Copyright 2013 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !appengine

// This file implements a stand-alone blog server.

package main

import (
	"log"
	"net/http"
	"os"
)

func main() {
	var contentPath string
	var templatePath string
	var staticPath string

	var port = os.Getenv("PORT")
	if port == "" {
		port = "8080"
		contentPath = "../src/github.com/tiancaiamao/go.blog/content/"
		templatePath = "../src/github.com/tiancaiamao/go.blog/template/"
		staticPath = "../src/github.com/tiancaiamao/go.blog/static/"
	} else {
		contentPath = "/app/content/"
		templatePath = "/app/template/"
		staticPath = "/app/static/"
	}
	s, err := NewServer(contentPath, templatePath)
	if err != nil {
		log.Fatal(err)
	}
	http.Handle("/", s)
	fs := http.FileServer(http.Dir(staticPath))
	http.Handle("/static/", http.StripPrefix("/static/", fs))
	log.Fatal(http.ListenAndServe(":"+port, nil))
}
