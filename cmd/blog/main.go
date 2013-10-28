package main

import (
	"encoding/json"
	"fmt"
	"io"
	// "io/ioutil"
	"os"
	"time"
)

type ArticleItem struct {
	Title    string
	Date     time.Time
	Category string
	Tags     []string
	File     string
}

func main() {
	path := os.Getenv("GOPATH")
	indexFile := path + "/src/github.com/tiancaiamao/go.blog/content/index.json"
	// indexFile := "index.json"
	file, err := os.Open(indexFile)
	if err != nil {
		panic(err)
	}
	dec := json.NewDecoder(file)
	for {
		var item ArticleItem
		if err := dec.Decode(&item); err == io.EOF {
			break
		} else if err != nil {
			panic(err)
		}
		fmt.Println(item)
	}
}
