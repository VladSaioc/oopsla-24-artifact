package main

import (
	"flag"
	"fmt"
	"io/fs"
	"log"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

func main() {
	var searchName, regexpInput, dir, ext string
	var list, verificationTimeMode, fvsMode bool
	flag.StringVar(&searchName, "name", "", "Name of the search")
	flag.StringVar(&regexpInput, "regexp", "", "Regular expression to parse files.")
	flag.StringVar(&dir, "dir", "", "Directory to include in the search.")
	flag.StringVar(&ext, "ext", "", "File extension to match the search.")
	flag.BoolVar(&list, "list", false, "When passed, will list matched files")
	flag.Parse()

	if strings.Contains(regexpInput, "verification time:") {
		verificationTimeMode = true
	}
	if strings.Contains(regexpInput, "FV") ||
		strings.Contains(regexpInput, "Size of precondition") {
		fvsMode = true
	}

	re, err := regexp.Compile(regexpInput)
	if regexpInput == "" || err != nil {
		log.Println("Provide a valid regular expression. Given value:", regexpInput)
		os.Exit(1)
	}

	if dir == "" {
		log.Println("Provide a valid directory. Given value:", dir)
		os.Exit(1)
	}

	var pre string
	var content []int
	files := make(map[string]string)
	filepath.WalkDir(dir, func(p string, d fs.DirEntry, err error) error {
		if d == nil || d.IsDir() || !strings.HasSuffix(p, ext) {
			return nil
		}

		rout, err := os.ReadFile(p)
		if err != nil {
			log.Println("Failed to open file:", p)
			log.Println("ERROR:", err)
			return nil
		}

		if strings.Contains(string(rout), "not parametric") {
			return nil
		}

		if re.Match(rout) {
			if _, exists := files[path.Base(p)]; !exists {
				switch {
				case verificationTimeMode:
					t := strings.Split(strings.Split(string(re.Find(rout)), "\n")[0], " verification time: ")
					if pre == "" {
						pre = t[0]
					}
					if v, err := strconv.Atoi(strings.Split(t[1], "ms")[0]); err == nil {
						content = append(content, v)
					}
				case fvsMode:
					parts := re.Split(string(rout), -1)
					if len(parts) < 2 {
						return nil
					}
					match := parts[1]
					if strings.Contains(match, "not parametric") {
						return nil
					}

					parts = strings.Split(match, "|")
					if len(parts) < 0 {
						return nil
					}

					match = parts[0]
					if v, err := strconv.Atoi(strings.TrimSpace(match)); err == nil {
						content = append(content, v)
					}
				}
			}

			files[path.Base(p)] = p
		}

		return nil
	})

	switch {
	case searchName != "" && fvsMode:
		slices.Sort(content)
		fmt.Print(searchName, " ", pre, " ::  Min: ", Min(content), "; Avg: ", Avg(content), "; P50: ", P50(content), "; P90: ", P90(content), "; Max: ", Max(content), "\n")
	case searchName != "" && !verificationTimeMode:
		fmt.Println(searchName, ":: found", len(files), "matches")
	case searchName != "" && verificationTimeMode:
		slices.Sort(content)
		fmt.Print(searchName, " :: Avg: ", Avg(content), "ms; P50: ", P50(content), "ms; P90: ", P90(content), "ms; Max: ", Max(content), "ms\n")
		fmt.Println("Performed:", len(content))
	default:
		fmt.Println("Found", len(files), "matches")
	}

	if list {
		filesSorted := make([]string, 0, len(files))
		for _, f := range files {
			filesSorted = append(filesSorted, f)
		}
		slices.Sort(filesSorted)
		for _, f := range filesSorted {
			fmt.Println(f)
		}
	}
}

func Avg(vs []int) float64 {
	if len(vs) == 0 {
		return 0
	}

	sum := 0
	for _, v := range vs {
		sum += v
	}

	return float64(int(float64(sum)/float64(len(vs))*100)) / 100
}

func Min(vs []int) int {
	if 0 < len(vs) {
		return vs[0]
	}
	return 0
}

func P50(vs []int) int {
	p := len(vs) / 2
	if p < len(vs) {
		return vs[p]
	}
	return 0
}

func P90(vs []int) int {
	p := len(vs) * 9 / 10
	if p < len(vs) {
		return vs[p]
	}
	return 0
}

func Max(vs []int) int {
	if 0 < len(vs) {
		return vs[len(vs)-1]
	}
	return 0
}
