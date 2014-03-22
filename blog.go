// Copyright 2013 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This is a blog server for articles written in present format.
// It powers blog.golang.org.
package main

import (
	"bytes"
	"encoding/json"
	"encoding/xml"
	"fmt"
	"html/template"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"time"

	"github.com/russross/blackfriday"
	"github.com/tiancaiamao/go.blog/atom"
	"github.com/tiancaiamao/go.blog/present"
)

const (
	hostname     = "www.zenlife.tk"
	baseURL      = "http://" + hostname
	homeArticles = 5  // number of articles to display on the home page
	feedArticles = 20 // number of articles to include in Atom feed
)

// Doc represents an article, adorned with presentation data:
// its absolute path and related articles.
type Doc struct {
	ArticleItem
	Slide *present.Doc
	// Permalink    string
	// Path         string
	Related      []*Doc
	Newer, Older *Doc
	HTML         template.HTML // rendered article
}

// Server implements an http.Handler that serves blog articles.
type Server struct {
	docs        []*Doc
	slides      []*present.Doc
	tags        []string
	docPaths    map[string]*Doc
	docTags     map[string][]*Doc
	docCategory map[string][]*Doc
	template    struct {
		home, index, article, doc, slide, about *template.Template
	}
	atomFeed []byte // pre-rendered Atom feed
	content  http.Handler
}

// NewServer constructs a new Server, serving articles from the specified
// contentPath generated from templates from templatePath.
func NewServer(contentPath, templatePath string) (*Server, error) {
	present.PlayEnabled = true

	root := filepath.Join(templatePath, "root.tmpl")
	parse := func(name string) (*template.Template, error) {
		t := template.New("").Funcs(funcMap)
		return t.ParseFiles(root, filepath.Join(templatePath, name))
	}

	s := &Server{}

	// Parse templates.
	var err error
	s.template.home, err = parse("home.tmpl")
	if err != nil {
		return nil, err
	}
	s.template.index, err = parse("index.tmpl")
	if err != nil {
		return nil, err
	}
	s.template.article, err = parse("article.tmpl")
	if err != nil {
		return nil, err
	}
	s.template.about, err = parse("about.tmpl")
	if err != nil {
		return nil, err
	}
	p := present.Template().Funcs(funcMap)
	s.template.doc, err = p.ParseFiles(filepath.Join(templatePath, "doc.tmpl"))
	if err != nil {
		return nil, err
	}

	tmpl := present.Template()
	actionTmpl := filepath.Join(templatePath, "action.tmpl")
	contentTmpl := filepath.Join(templatePath, "slides.tmpl")
	s.template.slide, err = tmpl.ParseFiles(actionTmpl, contentTmpl)
	if err != nil {
		return nil, err
	}

	// Load content.
	err = s.loadDocs(filepath.Clean(contentPath))
	if err != nil {
		return nil, err
	}

	err = s.renderAtomFeed()
	if err != nil {
		return nil, err
	}

	// Set up content file server.
	s.content = http.FileServer(http.Dir(contentPath))

	return s, nil
}

var funcMap = template.FuncMap{
	"sectioned": sectioned,
	"authors":   authors,
}

// sectioned returns true if the provided Doc contains more than one section.
// This is used to control whether to display the table of contents and headings.
func sectioned(d *present.Doc) bool {
	return len(d.Sections) > 1
}

// authors returns a comma-separated list of author names.
func authors(authors []present.Author) string {
	var b bytes.Buffer
	last := len(authors) - 1
	for i, a := range authors {
		if i > 0 {
			if i == last {
				b.WriteString(" and ")
			} else {
				b.WriteString(", ")
			}
		}
		b.WriteString(authorName(a))
	}
	return b.String()
}

// my old json+org format articles~~
func (s *Server) loadOld(p string, doc *Doc) error {
	f, err := os.Open(p)
	if err != nil {
		return err
	}
	defer f.Close()
	art, err := ioutil.ReadAll(f)
	if err != nil {
		return err
	}

	meta := &PostData{
		Name:  doc.File,
		Title: "Title Here",
	}
	if bytes.HasPrefix(art, []byte("{\n")) {
		i := bytes.Index(art, []byte("\n}\n"))
		if i < 0 {
			panic("cannot find end of json metadata")
		}
		hdr, rest := art[:i+3], art[i+3:]
		if err := json.Unmarshal(hdr, meta); err != nil {
			panic(fmt.Sprintf("loading %s: %s", doc.File, err))
		}

		_, file := filepath.Split(p)
		p = file[:len(file)-len(".html")]
		doc.HTML = template.HTML(string(rest))
	}
	return nil
}

func (s *Server) loadMarkdown(p string, doc *Doc) error {
	input, err := ioutil.ReadFile(p)
	if err != nil {
		return err
	}
	output := blackfriday.MarkdownBasic(input)
	doc.HTML = template.HTML(string(output))

	return nil
}

func (s *Server) loadPresent(p string, doc *Doc) error {
	f, err := os.Open(p)
	if err != nil {
		return err
	}
	defer f.Close()
	d, err := present.Parse(f, p, 0)
	if err != nil {
		return err
	}
	html := new(bytes.Buffer)
	err = d.Render(html, s.template.doc)
	if err != nil {
		return err
	}

	doc.HTML = template.HTML(html.String())
	// _, file := filepath.Split(p)
	// p = file[:len(file)-len(".article")] // trim root and extension
	// s.docs = append(s.docs, &Doc{
	// 	Doc:       d,
	// 	Path:      "/" + p,
	// 	Permalink: baseURL + "/" + p,
	// 	HTML:      template.HTML(html.String()),
	// })
	return nil
}

func (s *Server) loadSlide(p string, doc *Doc) error {
	f, err := os.Open(p)
	if err != nil {
		return err
	}
	defer f.Close()
	d, err := present.Parse(f, p, 0)
	if err != nil {
		return err
	}
	// html := new(bytes.Buffer)
	// err = d.Render(html, s.template.slide)
	// if err != nil {
	// 	return err
	// }
	_, file := filepath.Split(p)
	p = file[:len(file)-len(".slide")] // trim root and extension

	doc.Slide = d

	// s.docs = append(s.docs, &Doc{
	// 	Doc:       d,
	// 	Path:      "/" + p,
	// 	Slide:     true,
	// 	Permalink: baseURL + "/" + p,
	// 	HTML:      "",
	// })
	return nil
}

// authorName returns the first line of the Author text: the author's name.
func authorName(a present.Author) string {
	el := a.TextElem()
	if len(el) == 0 {
		return ""
	}
	text, ok := el[0].(present.Text)
	if !ok || len(text.Lines) == 0 {
		return ""
	}
	return text.Lines[0]
}

type blogTime time.Time

func (t *blogTime) UnmarshalJSON(data []byte) (err error) {
	str := string(data)
	tt, err := time.Parse(`"`+"2006-01-02"+`"`, str)
	if err != nil {
		return fmt.Errorf("did not recognize time: %s", str)
	}
	*t = blogTime(tt)
	return nil
}

// json格式的文件索引
type ArticleItem struct {
	Title    string
	Date     time.Time
	Category string
	Tags     []string
	File     string
}

type PostData struct {
	Title    string
	Date     blogTime
	Category []string
	Tags     []string
	Summary  string
	Article  string

	author   string
	favorite bool
	Name     string //file name of the post
}

// func (s *Server) walkFunc(p string, info os.FileInfo, err error) error {
// 	ext := filepath.Ext(p)
// 	switch ext {
// 	case ".html":
// 		return s.loadOld(p, info, err)
// 	case ".article":
// 		return s.loadPresent(p, info, err)
// 	case ".slide":
// 		return s.loadSlide(p, info, err)
// 	case ".md":
// 		return s.loadMarkdown(p, info, err)
// 	}
// 	return nil
// }

// loadDocs reads all content from the provided file system root, renders all
// the articles it finds, adds them to the Server's docs field, computes the
// denormalized docPaths, docTags, and tags fields, and populates the various
// helper fields (Next, Previous, Related) for each Doc.
func (s *Server) loadDocs(root string) error {
	indexFile := root + "/index.json"
	file, err := os.Open(indexFile)
	if err != nil {
		panic(err)
	}

	dec := json.NewDecoder(file)
	for {
		var doc Doc
		if err := dec.Decode(&doc.ArticleItem); err == io.EOF {
			break
		} else if err != nil {
			panic(err)
		}

		fileName := root + "/" + doc.File
		ext := filepath.Ext(doc.File)
		var err error
		switch ext {
		case ".html":
			err = s.loadOld(fileName, &doc)
		case ".article":
			err = s.loadPresent(fileName, &doc)
		case ".slide":
			err = s.loadSlide(fileName, &doc)
		case ".md":
			err = s.loadMarkdown(fileName, &doc)
		}
		if err != nil {
			log.Println("loadDocs error:", err)
		} else {
			s.docs = append(s.docs, &doc)
		}
	}

	sort.Sort(docsByTime(s.docs))

	// fmt.Println("load finished...len(s.docs) is ", len(s.docs))

	// Pull out doc paths and tags and put in reverse-associating maps.
	s.docPaths = make(map[string]*Doc)
	s.docTags = make(map[string][]*Doc)
	s.docCategory = make(map[string][]*Doc)
	for _, d := range s.docs {
		s.docPaths[d.File] = d
		for _, t := range d.Tags {
			s.docTags[t] = append(s.docTags[t], d)
		}

		if d.Category != "" {
			s.docCategory[d.Category] = append(s.docCategory[d.Category], d)
		}
	}

	// Pull out unique sorted list of tags.
	for t := range s.docTags {
		s.tags = append(s.tags, t)
	}
	sort.Strings(s.tags)

	// Set up presentation-related fields, Newer, Older, and Related.
	for _, doc := range s.docs {
		// Newer, Older: docs adjacent to doc
		for i := range s.docs {
			if s.docs[i] != doc {
				continue
			}
			if i > 0 {
				doc.Newer = s.docs[i-1]
			}
			if i+1 < len(s.docs) {
				doc.Older = s.docs[i+1]
			}
			break
		}

		// Related: all docs that share tags with doc.
		related := make(map[*Doc]bool)
		for _, t := range doc.Tags {
			for _, d := range s.docTags[t] {
				if d != doc {
					related[d] = true
				}
			}
		}
		for d := range related {
			doc.Related = append(doc.Related, d)
		}
		sort.Sort(docsByTime(doc.Related))
	}

	return nil
}

// renderAtomFeed generates an XML Atom feed and stores it in the Server's
// atomFeed field.
func (s *Server) renderAtomFeed() error {
	var updated time.Time
	if len(s.docs) > 1 {
		updated = s.docs[0].Date
	}
	feed := atom.Feed{
		Title:   "Arthur的博客",
		ID:       hostname,
		Updated: atom.Time(updated),
		Link: []atom.Link{{
			Rel:  "self",
			Href: baseURL + "/feed.atom",
		}},
	}
	for i, doc := range s.docs {
		if i >= feedArticles {
			break
		}
		e := &atom.Entry{
			Title: doc.Title,
			ID:    feed.ID + doc.File,
			Link: []atom.Link{{
				Rel:  "alternate",
				Href: baseURL +"/"+ doc.File,
			}},
			Published: atom.Time(doc.Date),
			Updated:   atom.Time(doc.Date),
			Summary: &atom.Text{
				Type: "html",
				Body: summary(doc),
			},
			Content: &atom.Text{
				Type: "html",
				Body: string(doc.HTML),
			},
			Author: &atom.Person{
				Name: "Arthur",
			},
		}
		feed.Entry = append(feed.Entry, e)
	}
	data, err := xml.MarshalIndent(&feed, "", " ")
	if err != nil {
		return err
	}
	s.atomFeed = data
	return nil
}

// summary returns the first paragraph of text from the provided Doc.
func summary(d *Doc) string {
	// if len(d.Sections) == 0 {
	// 	return ""
	// }
	// for _, elem := range d.Sections[0].Elem {
	// 	text, ok := elem.(present.Text)
	// 	if !ok || text.Pre {
	// 		// skip everything but non-text elements
	// 		continue
	// 	}
	// 	var buf bytes.Buffer
	// 	for _, s := range text.Lines {
	// 		buf.WriteString(string(present.Style(s)))
	// 		buf.WriteByte('\n')
	// 	}
	// 	return buf.String()
	// }
	return ""
}

// rootData encapsulates data destined for the root template.
type rootData struct {
	Category map[string][]*Doc
	Tags     map[string][]*Doc
	Data     interface{} //用于模板中content部分
}

// ServeHTTP servers either an article list or a single article.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	err := r.ParseForm()
	if err != nil {
		return
	}

	var (
		d rootData
		t *template.Template
	)
	d.Category = s.docCategory
	d.Tags = s.docTags
	switch p := r.URL.Path; p {
	case "/":
		d.Data = s.docs
		if len(s.docs) > homeArticles {
			d.Data = s.docs[:homeArticles]
		}
		t = s.template.home
	case "/index":
		d.Data = s.docs
		t = s.template.index
	case "/about":
		d.Data = s.docs
		t = s.template.about
	case "/category":
		form := r.FormValue("name")
		if docs, ok := s.docCategory[form]; ok {
			d.Data = docs
			t = s.template.index
		} else {
			http.NotFound(w, r)
			return
		}
	case "/tags":
		form := r.FormValue("name")
		if docs, ok := s.docTags[form]; ok {
			d.Data = docs
			t = s.template.index
		} else {
			http.NotFound(w, r)
			return
		}
	case "/feed.atom", "/feeds/posts/default":
		w.Header().Set("Content-type", "application/atom+xml")
		w.Write(s.atomFeed)
		return
	default:
		// log.Println("request:", p)
		doc, ok := s.docPaths[p[1:]] // p begin with "/"
		if !ok {
			// Not a doc; try to just serve static content.
			// log.Println("server static content")
			s.content.ServeHTTP(w, r)
			return
		} else if doc.Slide != nil {
			err := doc.Slide.Render(w, s.template.slide)
			if err != nil {
				log.Println(err)
			}
			return
		}
		d.Data = doc
		t = s.template.article
	}
	err = t.ExecuteTemplate(w, "root", d)
	if err != nil {
		log.Println(err)
	}
}

// docsByTime implements sort.Interface, sorting Docs by their Time field.
type docsByTime []*Doc

func (s docsByTime) Len() int           { return len(s) }
func (s docsByTime) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s docsByTime) Less(i, j int) bool { return s[i].Date.After(s[j].Date) }
