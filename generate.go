package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

func main() {
	dir := "content/"
	content, err := os.Open(dir)
	checkErr(err)
	fileNames, err := content.Readdirnames(-1)
	checkErr(err)

	for _, fileName := range fileNames {
		if strings.HasSuffix(fileName, ".md") {
			generate(dir, fileName)
		}
	}
}

func generate(base, fileName string) {
	cmd := exec.Command("cmark", base+fileName)
	newName := newFileName("generated/" + fileName)
	out, err := os.Create(newName)
	checkErr(err)
	defer out.Close()
	fmt.Println("创建文件", newName)
	cmd.Stdout = out
	err = cmd.Run()
	if err != nil {
		fmt.Println("generate file fail:", fileName)
		panic(err)
	}
	// data, _ := cmd.Output()
	fmt.Println("generate file ", base+fileName)
}

// newFileName rename "xxx.md" to "xxx.out"
func newFileName(fileName string) string {
	return fileName[:len(fileName)-2] + "out"
}

func checkErr(err error) {
	if err != nil {
		panic(err)
	}
}
