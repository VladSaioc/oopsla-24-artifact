package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/nicolasdilley/gomela/promela"
	"github.com/nicolasdilley/gomela/stats"
	"golang.org/x/exp/slices"
)

type ProjectResult struct {
	Name             string            // the name of the project
	Num_states       int               // The overall number of states needed to verify the whole program
	Infer_timing     int               // time (ms) to infer the Promela model
	Models           []VerificationRun // Verification run for all Models
	Safety_error     bool              // is there any safety errors
	Global_deadlock  bool              // is there any global deadlock
	Spin_timing      int               // time (ms) to verify the program with spin
	Godel_timing     int               // time (ms) to verify the program with godel
	Migoinfer_timing int               // time (ms) to model the program with migoinfer
}

type VerificationInfo struct {
	multi_list                          string
	multi_projects                      string
	single_project                      string
	num_concurrency_primitive_as_global int
	unused_mutex                        int
	unused_wg                           int
	unused_chan                         int
	Go_names                            []string
	gopath                              string
	Comm_par_values                     []int
	print_trace                         bool
	spin_output                         string
	all_mandatory                       bool
	needs_project_folder                bool
	ginger_mode 						bool
	// single_file    *string
}

var (
	NUM_OF_MODELS            int = 0
	NUM_OF_EXECUTABLE_MODELS int = 0
	RESULTS_FOLDER           string
	PROJECTS_FOLDER          = "../projects"
	AUTHOR_PROJECT_SEP       = "--"
	PACKAGE_MODEL_SEP        = "++"
	TIMEOUT                  string
)

func main() {
	// connecting to github to parse all git projects
	// add timestamps to name of folder

	ver := &VerificationInfo{
		print_trace: false,
	}

	flag.StringVar(&ver.multi_list, "l", "", "a .csv is also given as args and contains a list of github.com projects with their commits to parse.")
	flag.StringVar(&ver.multi_projects, "mp", "", "Recursively loop through the folder given and parse all folder that contains a go file.")
	flag.StringVar(&ver.single_project, "s", "", "a single project is given to parse. Format \"creator/project_name\"")
	flag.StringVar(&ver.spin_output, "pt", "", "Specifies the file where the trace returned by spin is to be printed.")
	flag.StringVar(&TIMEOUT, "timeout", "30", "time limit for SPIN verification")
	flag.StringVar(&ver.gopath, "gopath", "", "a gopath to perform package loading from")
	flag.StringVar(&RESULTS_FOLDER, "result_folder", "result", "folder to store the result in")
	flag.StringVar(&PROJECTS_FOLDER, "p", PROJECTS_FOLDER, "a folder that contains all the projects.")
	flag.BoolVar(&ver.all_mandatory, "am", false, "turns all optional parameters into mandatory parameters.")
	flag.BoolVar(&ver.needs_project_folder, "needs-projects", false, "If set, then Gomela will capture projects from a given folder.")
	flag.BoolVar(&ver.ginger_mode, "ginger", false, "If set, Gomela will run in Ginger mode and skip standard Gomela verification.")

	flag.Parse()

	if ver.needs_project_folder {
		// create the projects folder if not there
		if _, err := os.Stat(PROJECTS_FOLDER); os.IsNotExist(err) {
			if errDir := os.MkdirAll(PROJECTS_FOLDER, 0755); errDir != nil {
				log.Fatal(err)
			}
		}
	}

	if ver.spin_output != "" {
		ver.print_trace = true
	}

	RESULTS_FOLDER, _ = filepath.Abs(RESULTS_FOLDER)
	f, _ := os.OpenFile(RESULTS_FOLDER+"/package_errors.csv",
		os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	os.Stderr = f
	defer f.Close()

	// parse the potential config file
	c := parseConfigFile()

	ver.Go_names = c.Go
	ver.Comm_par_values = c.Comm_par_values
	switch flag.Arg(0) {
	case "model": // the user wants to generate the model
		model(ver)
		fmt.Println("Num of global concurrency primitives ", ver.num_concurrency_primitive_as_global)
	case "verify": // the user wants to verify a .pml file with specifics bounds

		if flag.NArg() <= 1 || !strings.Contains(flag.Arg(1), ".pml") {
			panic("Please provide a .pml file : i.e. Gomela verify hello.pml")
		}
		model_to_verify := flag.Arg(1)

		// parse how many comm pars are in the file

		content, err := ioutil.ReadFile(model_to_verify)

		if err != nil {
			panic("Please provide a valid file , err : " + err.Error())
		}

		mand_params, opt_params := findNumCommParam(string(content))

		if flag.NArg()-2-mand_params-opt_params != 0 {
			panic("Please provide a value for each comm parameter in the order they appear in the program, num params = " + fmt.Sprint(mand_params+opt_params) + ", num args given " + fmt.Sprint(flag.Args()))
		}
		verifyModelWithSpecificValues(ver, string(content), flag.Args()[2:])
	case "full_stats": // Generate a set of stats for each projects and models
		stats.Stats()
	case "stats":
		printStats()
	case "fs": // Full scale model -> verify -> stats
		model(ver)
		verify(ver, RESULTS_FOLDER)

	case "bmc":
		if flag.NArg() <= 1 {
			panic("Please provide a folder that contains the .pml that you want to parse or a pml that you want to verify.")
		}
		verify(ver, flag.Arg(1))
	case "commit": // produce a list of commit from given list of projects
		commit(ver)

	case "sanity": // remove the .pml files that do nothing
		if flag.NArg() <= 1 {
			panic("You need to provide a folder containing the .pml or a .pml file.")
		}

		num_unsain := sanity(ver, flag.Arg(1), flag.NArg() > 2)
		fmt.Println("Removed a total of ", num_unsain, " files which did not contain any concurrent interactions")
		fmt.Println("Num of mutex : ", ver.unused_mutex)
		fmt.Println("Num of wg : ", ver.unused_wg)
		fmt.Println("Num of chan : ", ver.unused_chan)
	default:
		fmt.Println("Unrecognized mode, got", flag.Args())
		panic("You need to provide a mode.")
	}

}

func findNumCommParam(content string) (int, int) {
	mand_params := 0
	opt_params := 0

	lines := strings.Split(content, "\n")

	for _, line := range lines {
		if strings.Contains(line, "num_mand_comm_params") {
			splitted := strings.Split(line, "num_mand_comm_params=")

			n, _ := strconv.Atoi(splitted[1])

			mand_params += n
		}

		if strings.Contains(line, "num_opt_comm_params") {
			splitted := strings.Split(line, "num_opt_comm_params=")

			n, _ := strconv.Atoi(splitted[1])

			opt_params += n
		}
	}

	return mand_params, opt_params
}

func sanity(ver *VerificationInfo, path string, del bool) int {
	if strings.Contains(path, ".pml") {
		if !sanityCheckFile(ver, path, del) {
			return 1
		}
		return 0
	}

	unsanitaryFiles := 0
	f, err := os.Stat(path)

	if err != nil {
		panic("Could not read file/folder " + path)
	}
	if !f.IsDir() {
		panic("The file provided should be a .pml file or a directory")
	}
	filepath.Walk(path,
		func(fpath string, info os.FileInfo, err error) error {
			if info.IsDir() && fpath != path {
				unsanitaryFiles += sanity(ver, fpath, del)
				return filepath.SkipDir
			}
			if strings.Contains(fpath, ".pml") && !sanityCheckFile(ver, fpath, del) {
				unsanitaryFiles++
			}
			return nil
		})

	return unsanitaryFiles
}

func sanityCheckFile(ver *VerificationInfo, path string, del bool) bool {
	data, e := ioutil.ReadFile(path)

	if e != nil {
		panic("Could not read file " + path)
	}

	model := strings.Split(string(data), "/*")[0]

	for _, line := range strings.Split(model, "\n") {
		if strings.Contains(line, ".update!") ||
			strings.Contains(line, ".wait?") ||
			strings.Contains(line, ".Lock!") ||
			strings.Contains(line, ".Unlock!") ||
			strings.Contains(line, ".RLock!") ||
			strings.Contains(line, ".RUnlock!") ||
			strings.Contains(line, ".deq?") ||
			strings.Contains(line, ".rcving!") ||
			strings.Contains(line, ".sending!") ||
			strings.Contains(line, ".sync") ||
			strings.Contains(line, ".enq!") {
			return true
		}
	}

	for _, line := range strings.Split(model, "\n") {
		if strings.Contains(line, "run mutex_monitor") { // Mutex
			ver.unused_mutex++
		}
		if strings.Contains(line, "run wg_monitor") { // wg
			ver.unused_wg++
		}
		if strings.Contains(line, "run sync_monitor") { // Chan
			ver.unused_chan++
		}
	}
	// check whether it contains a channel, wg or mutex

	if del {
		os.Remove(path)
	}
	return false
}

// commit takes a list of projects as a .csv or .txt files, and parses them.
func commit(ver *VerificationInfo) {
	// parse multiple projects
	if flag.NArg() <= 1 {
		return
	}

	// Check whether the list of projects has been provided.
	if !(strings.HasSuffix(flag.Arg(1), ".csv") || strings.HasSuffix(flag.Arg(1), ".txt")) {
		fmt.Println("Please provide a .csv or .txt file containing the list of projects to be parsed")
		return
	}

	// parse each projects
	data, e := ioutil.ReadFile(flag.Arg(1))
	if e != nil {
		fmt.Printf("prevent panic by handling failure accessing a path %q: %v\n", flag.Arg(1), e)
		return
	}

	projects_commit := ""
	proj_listings := strings.Split(string(data), "\n")
	fmt.Println(len(proj_listings), " projects to parse")
	for _, project := range proj_listings[:len(proj_listings)-1] {
		if project_info := strings.Split(project, ","); strings.Contains(project, ",") && len(project_info) > 1 {
			panic("Commit already present")
		}

		// clone repo and get commit
		commit := CloneRepoAndGetCommit(project)
		projects_commit += project + "," + commit + "\n"
	}

	ioutil.WriteFile("commits.csv", []byte(projects_commit), 0664)
}

// genreate a model based on input and return the flags left
func model(ver *VerificationInfo) []string {
	if RESULTS_FOLDER == "result" {
		RESULTS_FOLDER += time.Now().Local().Format("2006-01-02--15:04:05")
	}

	os.MkdirAll(RESULTS_FOLDER, os.ModePerm)
	promela.CreateCSV(RESULTS_FOLDER)

	switch flag.Arg(1) {
	case "l":
		// parse multiple projects
		if flag.NArg() <= 2 || !strings.HasSuffix(flag.Arg(2), ".csv") {
			fmt.Println("Please provide a .csv file containing the list of projects to be parsed")
			return []string{}
		}

		// parse each projects
		data, e := ioutil.ReadFile(flag.Arg(2))
		if e != nil {
			panic(fmt.Sprintf("prevent panic by handling failure accessing a path %q: %v\n", flag.Arg(2), e))
		}
		proj_listings := strings.Split(string(data), "\n")
		fmt.Println(len(proj_listings), " projects to parse")
		for _, project := range proj_listings[:len(proj_listings)-1] {
			name, commit := project, "master"

			if strings.Contains(project, ",") {
				project_info := strings.Split(project, ",")
				if len(proj_listings) > 1 {
					name, commit = project_info[0], project_info[1]
				}
			}
			parseProject(name, commit, ver)
		}

		fmt.Println(NUM_OF_EXECUTABLE_MODELS, "/", NUM_OF_MODELS, " executable models overall.")
	case "mp":
		path := flag.Arg(2)
		// PROJECTS_FOLDER = path

		path, _ = filepath.Abs(path)
		files, _ := ioutil.ReadDir(path)

		for _, f := range files {
			fmt.Println("Modelling : ", f.Name())
			parseFolder(path+"/"+f.Name(), ver)
		}
	case "s":
		// parse project given
		parseProject(flag.Arg(2), "master", ver)
	default:
		path := flag.Arg(1)
		// PROJECTS_FOLDER = path

		_, err := ioutil.ReadDir(path)

		if err != nil {
			panic("please give a valid folder to parse. Path: " + path)
		}
		packages := []string{}
		filepath.Walk(path, func(path string, file os.FileInfo, err error) error {
			if !file.IsDir() || file.Name() == "vendor" || file.Name() == "third_party" {
				return nil
			}

			path, _ = filepath.Abs(path)
			packages = append(packages, path)
			return nil
		})

		inferProject(path, filepath.Base(path), "", packages, ver)
		if flag.NArg() <= 2 {
			return []string{}
		}
	}

	return flag.Args()[2:]
}

func verify(ver *VerificationInfo, toParse string) {
	if toParse[0] != '/' {
		toParse = "./" + toParse
	}
	// toPrint := "Model, Opt, #states, Time (ms), Channel Safety Error, Global Deadlock, Error, Comm param info, Link,\n"

	if toParse[0] != '/' {
		toParse = "./" + toParse
	}

	toPrint := ""

	// check if toParse is a folder or a .pml file

	f, err := os.Stat(toParse)

	if err != nil {
		panic("The folder given cannot be parsed")
	}
	bounds_to_check := []interface{}{}

	bounds_index := 2

	for bounds_index <= flag.NArg() {
		if _, err := strconv.Atoi(flag.Args()[bounds_index-1]); err == nil {
			break
		}
		bounds_index++
	}

	if flag.NArg() >= bounds_index {
		for _, b := range flag.Args()[bounds_index-1:] {
			num, err := strconv.Atoi(b)

			if err != nil {
				panic("Should provide a number for the bounds. Got : " + b)
			}

			bounds_to_check = append(bounds_to_check, num)
		}
	}

	if !f.IsDir() {
		// a single .pml has been given as arg
		RESULTS_FOLDER, _ = filepath.Abs("./")
		VerifyModels(ver, []os.FileInfo{f}, "", bounds_to_check)
		return
	}
	// Print CSV
	file, err := os.OpenFile(toParse+"/verification.csv",
		os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)

	if err != nil {
		fmt.Println("Could not create file verification.csv")
		return
	}
	if _, err := file.WriteString(toPrint); err != nil {
		panic(err)
	}

	projects, err := ioutil.ReadDir(RESULTS_FOLDER)
	if err != nil {
		fmt.Println("Could not read folder :", RESULTS_FOLDER)
	}

	for _, p := range projects {
		if !p.IsDir() {
			continue
		}

		//verify the models inside the projects
		models, err := ioutil.ReadDir(RESULTS_FOLDER + "/" + p.Name())
		if err != nil {
			fmt.Println("Could not read folder :", p)
			continue
		}
		VerifyModels(ver, models, p.Name(), bounds_to_check)
	}
}

func parseFolder(path string, ver *VerificationInfo) {
	files, _ := ioutil.ReadDir(path)

	containsGoFile := false
	for _, f := range files {
		if strings.Contains(f.Name(), ".go") {
			containsGoFile = true
		}
	}

	if !containsGoFile {
		for _, f := range files {
			parseFolder(path+"/"+f.Name(), ver)
		}
		return
	}

	packages := []string{}
	filepath.Walk(path, func(path string, file os.FileInfo, err error) error {
		if !file.IsDir() {
			return filepath.SkipDir
		}

		externalDirs := []string{"vendor", "third_party"}
		if !slices.Contains(externalDirs, file.Name()) {
			path, _ = filepath.Abs(path)
			packages = append(packages, path)
		}
		return nil
	})

	inferProject(path, filepath.Base(path), "", packages, ver)
}

func parseProject(project_name string, commit string, ver *VerificationInfo) {
	fmt.Println("Infering : " + project_name)
	path_to_dir, commit_hash, err := CloneRepo(project_name, commit)

	if err != nil {
		fmt.Println("Could not download project ", project_name)
		return
	}

	packages := []string{}
	if err = filepath.Walk(path_to_dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			fmt.Printf("prevent panic by handling failure accessing a path %q: %v\n", path_to_dir, err)
			return err
		}

		if !info.IsDir() {
			return nil
		}

		if info.Name() == "vendor" {
			return filepath.SkipDir
		}
		packages = append(packages, "."+strings.TrimPrefix(path, path_to_dir))
		return nil
	}); err != nil {
		fmt.Printf("Error walking the path %q: %v\n", path_to_dir, err)
	}
	inferProject(path_to_dir, project_name, commit_hash, packages, ver)
}

type Param struct {
	Name      string
	Mandatory bool
}

func printStats() {
	if flag.NArg() <= 1 {
		panic("Please provide a .pml file : ie, gomela stats hello.pml")
	}

	// read file
	data, e := ioutil.ReadFile(flag.Arg(1))
	if e != nil {
		panic("The file provided " + flag.Arg(1) + " could not be open")
	}

	// print stats
	lines := strings.Split(string(data), "\n")
	params := []Param{}

	for _, line := range lines {
		if strings.Contains(line, "//") && (strings.Contains(line, " opt ") || strings.Contains(line, " mand ")) {
			splitted := strings.Split(line, "//")
			splitted = strings.Split(splitted[1], " ")
			param := Param{Name: splitted[2], Mandatory: splitted[1] == "mand"}
			params = append(params, param)
		}
	}

	fmt.Println("Num comm params : ", len(params))
	fmt.Println("Mandatory Param : ")
	for _, param := range params {
		if param.Mandatory {
			fmt.Println(param.Name)
		}
	}

	fmt.Println("Optionnal Param : ")
	for _, param := range params {
		if !param.Mandatory {
			fmt.Println(param.Name)
		}
	}
}

func inferProject(path string, dir_name string, commit string, packages []string, ver *VerificationInfo) {

	// Partition program
	dir_name = strings.Replace(dir_name, "/", AUTHOR_PROJECT_SEP, -1)
	var gopath string

	if ver.gopath != "" {
		gopath = "GOPATH=" + ver.gopath
	}

	f, ast_map := GenerateAst(path, packages, dir_name, gopath)

	if f == nil {
		fmt.Println("Error while parsing project")
		return
	}
	projects_folder, _ := filepath.Abs(PROJECTS_FOLDER)
	ParseAst(f, dir_name, commit, ast_map, ver, RESULTS_FOLDER, projects_folder)

	// Have a way to give values Wto individual candidates and unknown parameter
}

func colorise(flag bool) string {
	if flag {
		return color.New(color.FgRed).SprintFunc()("true")
	}

	return color.New(color.FgGreen).SprintFunc()("false")
}
