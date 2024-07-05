package promela

import (
	"bytes"
	"errors"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"go/types"
	"strconv"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
	"github.com/nicolasdilley/gomela/promela/promela_types"
	"golang.org/x/tools/go/packages"
)

var (
	DEF_PREFIX            = "def_"
	VAR_PREFIX            = "var_"
	CHAN_NAME             = "_ch"
	DEFAULT_BOUND         = "??"
	OPTIONAL_BOUND        = "-2"
	CHAN_BOUND            = 0
	ADD_BOUND             = 1
	LOWER_FOR_BOUND       = 2
	UPPER_FOR_BOUND       = 3
	RANGE_BOUND           = 4
	AUTHOR_PROJECT_SEP    = "--"
	MAX_STRUCTS_INTER_PRO = 4

	Features = []Feature{}
)

// GlobalProps captures traits that are relevant and must be preserved and updated across models.
type GlobalProps struct {
	Fileset *token.FileSet

	// Relevant flags
	ContainsWg       bool
	ContainsChan     bool
	ContainsMutexes  bool
	ContainsReceiver bool
	GingerMode    bool // Is the model running in Ginger mode?
}

type Model struct {
	Props *GlobalProps

	Result_fodler string // the name of the folder where the model need to ne printed
	Project_name  string // the full name of  project (eg. "nicolasdilley/Gomela")
	Package       string // the name of the package
	Name          string // the name of the file that will be generated. (Composed of "pack_functionName")
	Commit        string // the commit of the project

	RecFuncs      []RecFunc
	SpawningFuncs []*SpawningFunc
	Defines       []promela_ast.DefineStmt      // the channel bounds
	Proctypes     []*promela_ast.Proctype       // the processes representing the functions of the model
	Inlines       []*promela_ast.Inline         // the inlines function that represent the commpar args that are function calls
	Fun           *ast.FuncDecl                 // the function being modelled
	Chans         map[ast.Expr]*ChanStruct      // the promela chan used in the module mapped to their go expr
	WaitGroups    map[ast.Expr]*WaitGroupStruct // the promela chan used in the module mapped to their go expr
	Mutexes       []ast.Expr                    // The promela mutex declaration
	Init          *promela_ast.InitDef          // The proctype consisting of the "main" function of the source program
	Global_vars   []promela_ast.Node            // the global variable used in the ltl properties
	CommPars      []*CommPar                    // the communications paramer
	Features      []Feature                     // The features for the survey
	ClosedVars    map[*ChanStruct][]ast.Expr    // The variable that are used to test if a channel is closed when receiving (i.e ok in r,ok := >-ch )

	For_counter          *ForCounter                  // Used to translate the for loop to break out properly out of them
	process_counter      int                          // to give unique name to Promela processes
	func_counter         int                          // to give unique name to inline func call
	Counter              int                          // used to differentiate call expr channels
	AstMap               map[string]*packages.Package // the map used to find the type of the channels
	Projects_folder      string
	GenerateFeatures     bool // should the model print features ?
	Current_return_label string
	defer_counter        int

	// Represents functions that may act as an alias for goroutine spawning
	Go_names []string
	// Turns all optional parameters mandatory
	All_mandatory bool
}

// Used to represent a function for recursive calls
type RecFunc struct {
	Pkg  string
	Name string
	Decl *ast.FuncDecl
}

// represent a function and states if it spawns or not
type SpawningFunc struct {
	Rec_func    RecFunc
	is_spawning bool
}

type Bound struct {
	Name ast.Expr
	Var  promela_ast.Ident
	Val  int
}

// Take a go function and translate it to a Promela module.
// Returns true if the model translation was successful.
func (m *Model) GoToPromela(SEP string) (bool, error) {
	AUTHOR_PROJECT_SEP = SEP
	Features = []Feature{}
	b, err := m.TranslateGoStmt(
		&ast.GoStmt{
			Go:   m.Fun.Pos(),
			Call: &ast.CallExpr{Fun: m.Fun.Name, Args: make([]ast.Expr, m.Fun.Type.Params.NumFields())},
		}, true)

	m.Init = &promela_ast.InitDef{
		Def:  m.Props.Fileset.Position(m.Fun.Pos()),
		Body: &promela_ast.BlockStmt{List: []promela_ast.Node{}},
	}

	// Check whether generating the model produced an error
	if err != nil {
		fmt.Println("Could not parse model ", m.Name+":", err)

		logFeature(Feature{
			Proj_name: m.Project_name,
			Model:     m.Name,
			Fun:       m.Fun.Name.String(),
			Name:      "MODEL ERROR = " + err.Error(),
			Mandatory: "false",
			Line:      0,
			Commit:    m.Commit,
			Filename:  m.Props.Fileset.Position(m.Fun.Pos()).Filename,
		}, m)
		return false, err
	}
	// Skip the model if it is uninteresting
	if len(m.Chans) == 0 && len(m.WaitGroups) == 0 && len(m.Mutexes) == 0 {
		return false, nil
	}

	m.Init.Body.List = append(m.Init.Body.List,
		b.List...)

	// clean the model by removing empty for loops and unused opt param
	m.Features = Features

	Clean(m)

	m2 := m.Clone()
	m.Props.GingerMode = false
	// Print original the model
	Print(m)
	PrintFeatures(m.Features, m)
	// Print Ginger-friendly model
	m2.Name = m.Name + "-ginger"
	m2.Props.GingerMode = true
	Print(m2)
	return true, nil
}

func (m *Model) translateNewVar(s ast.Stmt, lhs []ast.Expr, rhs []ast.Expr) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}

	new_var := false

	// check if the assign or declaration is a declaration of a new var
	switch s := s.(type) {
	case *ast.AssignStmt:
		new_var = s.Tok == token.DEFINE
	default:
		new_var = true
	}

	for i, rh := range rhs {
		b1, err1 := m.lookForChans(lhs[i], rh, new_var)
		if err1 != nil {
			return b, err1
		}

		addBlock(b, b1)
	}
	// check if the new var is a struct
	for _, lh := range lhs {
		t := m.AstMap[m.Package].TypesInfo.TypeOf(lh)
		if t != nil {
			t = GetElemIfPointer(t)
			switch t := t.(type) {
			case *types.Named:
				b1, err1 := m.translateStruct(s, lh, t, []*types.Named{t}, 0, new_var)
				if err1 != nil {
					return b, err1
				}

				addBlock(b, b1)
			}
		}
	}

	return b, err
}

func GetElemIfPointer(t types.Type) types.Type {
	switch t := t.(type) {
	case *types.Pointer:
		return GetElemIfPointer(t.Elem())
	default:
		return t
	}
}

func (m *Model) translateStruct(s ast.Stmt, lhs ast.Expr, t types.Type, seen []*types.Named, inter_pro int, new_var bool) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	// Tests if one of the field of the assign structs is a WG
	// We have the definition of a struct

	if inter_pro <= MAX_STRUCTS_INTER_PRO {
		if t.String() == "sync.WaitGroup" {
			if !new_var {
				return b, errors.New(WG_ALIASING + m.Props.Fileset.Position(lhs.Pos()).String())
			}
			return m.translateWg(s, lhs)

		} else if t.String() == "sync.Mutex" || t.String() == "sync.RWMutex" {
			if !new_var {
				return b, errors.New(MUTEX_ALIASING + m.Props.Fileset.Position(lhs.Pos()).String())
			}
			return m.translateMutex(s, lhs)
		}

		switch t := t.Underlying().(type) {

		case *types.Struct:
			for i := 0; i < t.NumFields(); i++ {

				field_type := t.Field(i).Type()

				field_type = GetElemIfPointer(field_type)

				switch field := field_type.(type) {
				case *types.Array:
					elem := GetElemIfPointer(field.Elem())
					switch elem := elem.(type) {
					case *types.Chan:
						return b, errors.New(CHAN_IN_LIST + m.Props.Fileset.Position(s.Pos()).String())
					default:
						if elem.String() == "sync.WaitGroup" {
							return b, errors.New(WG_IN_LIST + m.Props.Fileset.Position(s.Pos()).String())
						}
						if elem.String() == "sync.Mutex" || elem.String() == "sync.RWMutex" {
							return b, errors.New(MUTEX_IN_LIST + m.Props.Fileset.Position(s.Pos()).String())
						}
					}
				case *types.Slice:
					elem := GetElemIfPointer(field.Elem())
					switch elem := elem.(type) {
					case *types.Chan:
						return b, errors.New(CHAN_IN_LIST + m.Props.Fileset.Position(s.Pos()).String())
					default:
						if elem.String() == "sync.WaitGroup" {
							return b, errors.New(WG_IN_LIST + m.Props.Fileset.Position(s.Pos()).String())
						}
						if elem.String() == "sync.Mutex" || elem.String() == "sync.RWMutex" {
							return b, errors.New(MUTEX_IN_LIST + m.Props.Fileset.Position(s.Pos()).String())
						}
					}
				case *types.Map:
					elem := GetElemIfPointer(field.Elem())
					switch elem := elem.(type) {
					case *types.Chan:
						return b, errors.New(CHAN_IN_MAP + m.Props.Fileset.Position(s.Pos()).String())
					default:
						if elem.String() == "sync.WaitGroup" {
							return b, errors.New(WG_IN_MAP + m.Props.Fileset.Position(s.Pos()).String())
						}
						if elem.String() == "sync.Mutex" || elem.String() == "sync.RWMutex" {
							return b, errors.New(MUTEX_IN_MAP + m.Props.Fileset.Position(s.Pos()).String())
						}
					}

				case *types.Struct:

					b1, err1 := m.translateStruct(
						s,
						&ast.SelectorExpr{
							X: lhs,
							Sel: &ast.Ident{
								Name: t.Field(i).Name()}},
						field, seen, inter_pro+1, new_var)

					if err1 != nil {
						return b, err1
					}
					addBlock(b, b1)

				case *types.Named:

					if !t.Field(i).Embedded() {
						b1, err1 := m.translateNamed(s, &ast.SelectorExpr{
							X: lhs,
							Sel: &ast.Ident{
								Name: t.Field(i).Name()}},
							field,
							seen, inter_pro, new_var)
						if err1 != nil {
							return b, err1
						}

						addBlock(b, b1)
					}
				}

			}
		}

		// this is to accomodate struct that have a Mutex, Waitgroup embedded
		switch t := t.(type) {
		case *types.Struct:
			for i := 0; i < t.NumFields(); i++ {
				switch t := t.Field(i).Type().(type) {
				case *types.Named:
					b1, err1 := m.translateNamed(s, lhs,
						t,
						seen, inter_pro, new_var)
					if err1 != nil {
						return b, err1
					}

					addBlock(b, b1)
				}
			}
		}

		// if the structures contains a channel that we don't know
		// of then generate one with a bounded size and generate comm param.
	}
	return b, nil
}

func (m *Model) translateNamed(s ast.Stmt, name ast.Expr, t *types.Named, seen []*types.Named, inter_pro int, new_var bool) (*promela_ast.BlockStmt, error) {
	contains := false
	b := &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	for _, s := range seen {
		if s.String() == t.String() {
			contains = true
		}
	}
	if !contains {

		new_seen := seen

		if t.String() != "sync.Mutex" || t.String() != "sync.RWMutex" || t.String() != "sync.WaitGroup" {
			new_seen = append(seen, t)
		}

		if inter_pro <= MAX_STRUCTS_INTER_PRO {
			return m.translateStruct(s,
				name,
				t, new_seen, inter_pro+1, new_var)
		}

	}

	return b, nil
}

// Takes the declaration of a composite list {field: expr, ...} and checks for wg and mutex
func (m *Model) lookForChans(lhs ast.Expr, rhs ast.Expr, new_var bool) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	// Tests if one of the field of the assign structs is a WG

	switch c := rhs.(type) {
	case *ast.CallExpr:
		switch ident := c.Fun.(type) {
		case *ast.Ident:
			if ident.Name != "make" || len(c.Args) < 1 || len(c.Args) > 2 {
				break
			}

			// possibly a new chan
			switch c.Args[0].(type) {
			case *ast.ChanType:
				if !new_var {
					return b, errors.New(CHAN_ALIASING + m.Props.Fileset.Position(lhs.Pos()).String())
				}
				b1, err1 := m.translateChan(lhs, c.Args)

				if err1 != nil {
					return b, err1
				}

				addBlock(b, b1)
			}
		}
	case *ast.UnaryExpr:
		return m.lookForChans(lhs, c.X, new_var)
	case *ast.CompositeLit:
		for _, f := range c.Elts {

			var expr ast.Expr = f

			switch u := f.(type) {
			case *ast.UnaryExpr:
				expr = u.X
			}

			switch expr := expr.(type) {
			case *ast.KeyValueExpr:

				switch ident := expr.Key.(type) {
				case *ast.Ident:
					b1, err1 := m.lookForChans(&ast.SelectorExpr{X: lhs, Sel: ident}, expr.Value, new_var)

					if err1 != nil {
						return b, err1
					}

					addBlock(b, b1)
				default:
					switch c.Type.(type) {
					case *ast.StructType:
						panic(fmt.Sprint("A key on a struct must be an Ident at pos : ", m.Props.Fileset.Position(c.Pos()), " with ", expr, " and key :", expr.Key))
					}
				}

			case *ast.CompositeLit:
				b1, err1 := m.lookForChans(lhs, expr, new_var)

				if err1 != nil {
					return b, err1
				}

				addBlock(b, b1)
			}
		}
	}

	return b, err
}

func (m *Model) translateWg(s ast.Stmt, name ast.Expr) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	if !m.For_counter.In_for {

		prom_wg_name := translateIdent(name)

		if !m.containsWaitgroup(name) {
			m.Props.ContainsWg = true
			m.WaitGroups[name] = &WaitGroupStruct{
				Name:    &prom_wg_name,
				Wait:    m.Props.Fileset.Position(name.Pos()),
				Counter: 0,
			}

			m.PrintFeature(Feature{
				Proj_name: m.Project_name,
				Model:     m.Name,
				Fun:       m.Fun.Name.String(),
				Name:      "new WaitGroup",
				Info:      "Name :" + prom_wg_name.Name,
				Mandatory: "false",
				Line:      m.Props.Fileset.Position(s.Pos()).Line,
				Commit:    m.Commit,
				Filename:  m.Props.Fileset.Position(s.Pos()).Filename,
			})

			b.List = append(b.List,
				&promela_ast.DeclStmt{Name: &prom_wg_name, Types: promela_types.Wgdef},
				&promela_ast.RunStmt{X: &promela_ast.CallExpr{Fun: &promela_ast.Ident{Name: "wg_monitor"}, Args: []promela_ast.Node{&prom_wg_name}}})
		}
	} else {
		m.PrintFeature(Feature{
			Proj_name: m.Project_name,
			Model:     m.Name,
			Fun:       m.Fun.Name.String(),
			Name:      "WaitGroup in for",
			Mandatory: "false",
			Line:      m.Props.Fileset.Position(s.Pos()).Line,
			Commit:    m.Commit,
			Filename:  m.Props.Fileset.Position(s.Pos()).Filename,
		})
		err = errors.New(WAITGROUP_IN_FOR + m.Props.Fileset.Position(s.Pos()).String())
	}

	return b, err
}

func (m *Model) translateMutex(s ast.Stmt, prom_mutex_name ast.Expr) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}

	if !m.For_counter.In_for {

		if !m.containsMutex(prom_mutex_name) {
			m.Props.ContainsMutexes = true
			name := translateIdent(prom_mutex_name)
			m.Mutexes = append(m.Mutexes, prom_mutex_name)
			m.PrintFeature(Feature{
				Proj_name: m.Project_name,
				Model:     m.Name,
				Fun:       m.Fun.Name.String(),
				Name:      "new Mutex",
				Info:      "Name :" + translateIdent(prom_mutex_name).Name,
				Mandatory: "false",
				Line:      m.Props.Fileset.Position(s.Pos()).Line,
				Commit:    m.Commit,
				Filename:  m.Props.Fileset.Position(s.Pos()).Filename,
			})

			b.List = append(b.List,
				&promela_ast.DeclStmt{Name: &name, Types: promela_types.Mutexdef},
				&promela_ast.RunStmt{X: &promela_ast.CallExpr{Fun: &promela_ast.Ident{Name: "mutex_monitor"}, Args: []promela_ast.Node{&name}}})
		}
	} else {
		m.PrintFeature(Feature{
			Proj_name: m.Project_name,
			Model:     m.Name,
			Fun:       m.Fun.Name.String(),
			Name:      "Mutex in for",
			Mandatory: "false",
			Line:      m.Props.Fileset.Position(s.Pos()).Line,
			Commit:    m.Commit,
			Filename:  m.Props.Fileset.Position(s.Pos()).Filename,
		})
		err = errors.New(MUTEX_IN_FOR + m.Props.Fileset.Position(s.Pos()).String())
	}

	return b, err
}

func (m *Model) translateChan(go_chan_name ast.Expr, args []ast.Expr) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}

	if m.For_counter.In_for {
		m.PrintFeature(Feature{
			Proj_name: m.Project_name,
			Model:     m.Name,
			Fun:       m.Fun.Name.String(),
			Name:      "Chan in for",
			Mandatory: "false",
			Line:      m.Props.Fileset.Position(go_chan_name.Pos()).Line,
			Commit:    m.Commit,
			Filename:  m.Props.Fileset.Position(go_chan_name.Pos()).Filename,
		})
		err = errors.New(CHAN_IN_FOR + m.Props.Fileset.Position(go_chan_name.Pos()).String())
		return
	}

	// a new channel is found lets change its name, rename it in function and add to struct
	prom_chan_name := translateIdent(go_chan_name)
	prom_chan_name.Name += CHAN_NAME
	channel := &ChanStruct{Name: &prom_chan_name, Chan: m.Props.Fileset.Position(go_chan_name.Pos())}

	var size *promela_ast.Ident

	if len(args) > 1 { // check if the channel is buffered or not
		channel.Buffered = true
		size, err = m.lookUp(args[1], CHAN_BOUND, false)
	} else {
		size = &promela_ast.Ident{Name: "0"}
	}

	chan_def := &ChanDefDeclStmt{
		Decl: m.Props.Fileset.Position(go_chan_name.Pos()),
		Name: &prom_chan_name,
		Size: size,
		M:    m.Props,
	}

	b.List = append(b.List, chan_def)
	m.Chans[go_chan_name] = channel
	m.Props.ContainsChan = true
	m.PrintFeature(Feature{
		Proj_name: m.Project_name,
		Model:     m.Name,
		Fun:       m.Fun.Name.String(),
		Name:      "new channel",
		Info:      "Name :" + channel.Name.Name,
		Mandatory: "false",
		Line:      channel.Chan.Line,
		Commit:    m.Commit,
		Filename:  channel.Chan.Filename,
	})
	return b, err
}

// takes a promela body and add break if there are no breaks at the end or if there is
// add a goto to the surrounding for loop
func checkForBreak(body promela_ast.Node, g *promela_ast.GotoStmt) {
	treatBlock := func(block *promela_ast.BlockStmt) {
		switch found := containsBreak(block); {
		case !found && !containsReturn(block): // no return or break then just break
			block.List = append(block.List, &promela_ast.Ident{Name: "break"})
		case found:
			replaceBreak(block, g)
		}
	}

	switch body := body.(type) {
	case *promela_ast.BlockStmt:
		treatBlock(body)
	case *GenSendStmt:
		treatBlock(body.Sync_body)
		treatBlock(body.Async_body)
	case *GenRcvStmt:
		treatBlock(body.Sync_body)
		treatBlock(body.Async_body)
	case *promela_ast.SingleGuardStmt:
		treatBlock(body.Body)
	}
}

func replaceBreak(body *promela_ast.BlockStmt, g *promela_ast.GotoStmt) {

	promela_ast.Inspect(&promela_ast.BlockStmt{List: []promela_ast.Node{body}}, func(s promela_ast.Node) bool {
		switch s := s.(type) {
		case *promela_ast.BlockStmt:
			for i, ident := range s.List {
				switch ident := ident.(type) {
				case *promela_ast.Ident:
					if ident.Name == "break" {
						s.List[i] = g
					}
				}
			}
			return true
		case *promela_ast.ForStmt:
			return false
		case *promela_ast.DoStmt:
			return false
		}
		return true
	})
}

func containsBreak(b *promela_ast.BlockStmt) bool {
	contains := false
	promela_ast.Inspect(b, func(stmt promela_ast.Node) bool {
		switch stmt := stmt.(type) {

		case *promela_ast.Ident:
			if stmt.Name == "break" {
				contains = true
			}
		case *promela_ast.ForStmt:
			return false
		case *promela_ast.DoStmt:
			return false
		case *promela_ast.SelectStmt:
			return false
		}
		return true
	})
	return contains
}

func (m *Model) TranslateExpr(expr ast.Expr) (stmts *promela_ast.BlockStmt, err error) {
	stmts = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	if expr == nil {
		return
	}

	// Inspect whether the expression contains a function literal that uses
	// a known concurrency parameter as a free variable. If so, we need to
	// flag the fragment as not being a candidate for modeling, due to unsoundness.
	ast.Inspect(expr, func(n ast.Node) bool {
		switch expr := n.(type) {
		case *ast.FuncLit:
			err = errors.Join(err, m.checkFreeVars(expr))
			return false
		case *ast.CallExpr:
			// Call expressions must be surveyed for known goroutine spawns,
			// so they are treated separately.
			return false
		case *ast.UnaryExpr:
			switch expr.Op {
			case token.ARROW:
				var guard promela_ast.GuardStmt
				guard, err = m.translateRcvStmt(false, expr.X, &promela_ast.BlockStmt{List: []promela_ast.Node{}}, &promela_ast.BlockStmt{List: []promela_ast.Node{}})
				stmts.List = append(stmts.List, guard)
			}
		}
		return true
	})
	if err != nil {
		return
	}

	var translateExpr func(ast.Expr)
	translateExpr = func(e ast.Expr) {
		switch expr := e.(type) {
		case *ast.BinaryExpr:
			translateExpr(expr.X)
			translateExpr(expr.Y)
		case *ast.CallExpr:
			switch name := expr.Fun.(type) {
			case *ast.Ident:
				switch {
				// Closing a channel
				case name.Name == "close" && len(expr.Args) == 1:
					if !m.containsChan(expr.Args[0]) {
						err = errors.Join(err, errors.New(UNKNOWN_CHAN_CLOSE+m.Props.Fileset.Position(expr.Pos()).String()))
						return
					}
					clos := &GenCloseStmt{
						M: m.Props,
						Pos: m.Props.Fileset.Position(name.Pos()),
						Chan: m.getChanStruct(expr.Args[0]),
					}
					stmts.List = append(stmts.List, clos)
				// Call to panic
				case name.Name == "panic" && len(expr.Args) == 1:
					translateExpr(expr.Args[0])
					stmts.List = append(stmts.List, &promela_ast.CallExpr{
						Call:  m.Props.Fileset.Position(expr.Pos()),
						Model: "Panic",
						Fun:   &promela_ast.Ident{Name: "assert"},
						Args: []promela_ast.Node{
							&promela_ast.Ident{
								Name: "20==0",
							}}})
				default:
					call, err1 := m.TranslateCallExpr(expr)
					err = errors.Join(err, err1)
					addBlock(stmts, call)
				}
			case *ast.SelectorExpr:
				switch name.Sel.Name {
				case "Lock", "Unlock", "RUnlock", "RLock":
					t := m.AstMap[m.Package].TypesInfo.TypeOf(name.X)
					t = GetElemIfPointer(t)

					switch t := t.(type) {
					case *types.Named:
						switch t.String() {
						case "sync.Mutex", "sync.RWMutex":
							call, err1 := m.TranslateMutexOp(expr)
							err = errors.Join(err, err1)
							addBlock(stmts, call)
							return
						}
					}
				}
				call, err1 := m.TranslateCallExpr(expr)
				err = errors.Join(err, err1)
				addBlock(stmts, call)
			case *ast.FuncLit:
				new_block := name.Body
				for x, field := range name.Type.Params.List {
					for y, name := range field.Names {
						arg := expr.Args[x+y]
						switch expr := expr.Args[x+y].(type) {
						case *ast.UnaryExpr:
							arg = expr.X
						}
						new_block = RenameBlockStmt(new_block, []ast.Expr{name}, arg)
					}
				}
				stmts2, d1, err1 := m.TranslateBlockStmt(new_block)
				err = errors.Join(err, err1)
				addBlock(stmts, stmts2)
				addBlock(stmts, d1)
			default:
				e, err1 := m.TranslateCallExpr(expr)
				err = errors.Join(err, err1)
				addBlock(stmts, e)
			}
		case *ast.UnaryExpr:
			if expr.Op != token.ARROW {
				// If the operation is not a receive operation, process its underlying contents.
				translateExpr(expr.X)
			}
		case *ast.TypeAssertExpr:
			translateExpr(expr.X)
		case *ast.ParenExpr:
			translateExpr(expr.X)
		}
	}
	translateExpr(expr)
	return
}

func addBlock(b1 *promela_ast.BlockStmt, b2 *promela_ast.BlockStmt) {
	if b1 == nil || b2 == nil {
		return
	}
	for _, b := range b2.List {
		b1.List = append(b1.List, b)
	}
}

func (m *Model) getChanStruct(expr ast.Expr) *ChanStruct {
	for e, s := range m.Chans {
		if IdenticalExpr(e, expr) ||
			IdenticalExpr(e, &ast.Ident{Name: translateIdent(expr).Name}) ||
			isSubsetOfExpr(expr, e) {
			return s
		}
	}
	return nil
}

func (m *Model) FindDecl(call_expr *ast.CallExpr) (bool, *ast.FuncDecl, string) {
	var func_name, pack_name string
	var is_method_call bool
	var method_type types.Type
	// Find the decl of the function

	switch name := call_expr.Fun.(type) {
	case *ast.Ident:
		func_name = name.Name
		pack_name = m.Package
	case *ast.SelectorExpr:

		sel := m.AstMap[m.Package].TypesInfo.ObjectOf(name.Sel)

		var upper_name *ast.Ident = getIdent(name.X)

		if upper_name == nil {
			return false, nil, ""
		}

		x := m.AstMap[m.Package].TypesInfo.ObjectOf(upper_name)

		pack_name = upper_name.Name
		if sel != nil && sel.Pkg() != nil {
			pack_name = sel.Pkg().Name()
		}

		if x != nil {
			switch GetElemIfPointer(x.Type()).(type) {
			case *types.Named, *types.Struct:
				is_method_call = true
				method_type = x.Type()
			}
		}
		func_name = name.Sel.Name
	}

	// Look in the package of the call_expr
	if m.AstMap[pack_name] == nil {
		return false, nil, ""
	}

	for _, file := range m.AstMap[pack_name].Syntax {
		if file.Decls == nil {
			continue
		}

		for _, decl := range file.Decls {
			switch decl := decl.(type) {
			case *ast.FuncDecl:
				if func_name == decl.Name.Name {
					// lets check its type
					if !is_method_call {
						return true, decl, pack_name
					}

					if decl.Recv == nil {
						continue
					}

					for _, f := range decl.Recv.List {
						for _, n := range f.Names {
							obj := m.AstMap[pack_name].TypesInfo.ObjectOf(n)

							if obj == nil {
								return false, decl, pack_name
							}

							t := obj.Type()

							switch c := obj.Type().(type) {
							case *types.Pointer:
								switch method_type.(type) {
								case *types.Pointer:
								default:
									t = c.Elem()
								}
							default:
								switch c := method_type.(type) {
								case *types.Pointer:
									method_type = c.Elem()
								default:
								}
							}

							if types.Identical(method_type, t) {
								return true, decl, pack_name
							}
						}
					}
				}
			}
		}
	}

	return false, nil, ""
}

func (m *Model) containsChan(expr ast.Expr) bool {
	for e, _ := range m.Chans {
		if IdenticalExpr(
			&ast.Ident{Name: translateIdent(e).Name},
			&ast.Ident{Name: translateIdent(expr).Name}) ||
			isSubsetOfExpr(expr, e) {
			return true
		}
	}
	return false
}

func (m *Model) isChan(expr ast.Expr) bool {
	for e, _ := range m.Chans {

		if IdenticalExpr(&ast.Ident{Name: translateIdent(e).Name}, &ast.Ident{Name: translateIdent(expr).Name}) {
			return true
		}
	}
	return false
}

func (m *Model) containsWaitgroup(expr ast.Expr) bool {
	switch ptr := expr.(type) {
	case *ast.UnaryExpr:
		expr = ptr.X
	}

	for e, _ := range m.WaitGroups {
		if IdenticalExpr(e, expr) ||
			IdenticalExpr(e, &ast.Ident{Name: translateIdent(expr).Name}) ||
			isSubsetOfExpr(expr, e) {
			return true
		}
	}
	return false
}

func (m *Model) isWaitgroup(expr ast.Expr) bool {
	switch ptr := expr.(type) {
	case *ast.UnaryExpr:
		expr = ptr.X
	}

	for e, _ := range m.WaitGroups {
		if IdenticalExpr(e, expr) ||
			IdenticalExpr(&ast.Ident{Name: translateIdent(e).Name}, &ast.Ident{Name: translateIdent(expr).Name}) {
			return true
		}
	}
	return false
}

func (m *Model) CallExists(decl *ast.FuncDecl) (bool, error) {
	for _, proc := range m.Proctypes {
		if proc.Decl.Name.Name != decl.Name.Name {
			continue
		}

		if decl.Recv == nil {
			return true, nil
		}

		if proc.Decl.Recv != nil &&
			len(decl.Recv.List) > 0 {
			expr := getElemIfStar(decl.Recv.List[0].Type)
			expr1 := getElemIfStar(proc.Decl.Recv.List[0].Type)
			if IdenticalExpr(expr, expr1) {
				return true, nil
			}
		}
	}

	return false, nil
}

func getElemIfStar(expr ast.Expr) ast.Expr {
	switch expr := expr.(type) {
	case *ast.StarExpr:
		return getElemIfStar(expr.X)
	default:
		return expr
	}
}

func isRecursive(pack string, block *ast.BlockStmt, ast_map map[string]*packages.Package, call_seen []ast.Expr) bool {
	var recursive bool

	ast.Inspect(block, func(n ast.Node) bool {
		switch n := n.(type) {
		case *ast.CallExpr:
			recursive = recursive || IdenticalExprs(call_seen, n.Fun)
		}
		return !recursive
	})

	return recursive
}

// Takes a commPar and genrate a define stmt out of the name of the commPar and the function under analysis
// Returns the name of the defined variable
func (m *Model) GenerateDefine(commPar *CommPar) string {
	var_name := DEF_PREFIX + VAR_PREFIX + commPar.Name.Name + strconv.Itoa(m.Props.Fileset.Position(commPar.Expr.Pos()).Line)
	rhs := DEFAULT_BOUND

	if commPar.Mandatory {
		rhs += " // mand "
	} else {
		rhs = OPTIONAL_BOUND
		rhs += " // opt "
	}
	var buff *bytes.Buffer = bytes.NewBuffer([]byte{})
	printer.Fprint(buff, m.Props.Fileset, commPar.Expr)
	rhs += string(buff.Bytes()) + " line " + strconv.Itoa(m.Props.Fileset.Position(commPar.Expr.Pos()).Line)

	m.Defines = append(m.Defines, promela_ast.DefineStmt{Name: &promela_ast.Ident{Name: var_name}, Rhs: &promela_ast.Ident{Name: rhs}})

	return var_name
}

func IsConst(expr ast.Expr, pack *packages.Package) (found bool, val int) {
	switch expr := expr.(type) {
	case *ast.Ident:
		obj := expr.Obj
		if obj != nil && obj.Kind == ast.Con {
			switch value_spec := obj.Decl.(type) {
			case *ast.ValueSpec:
				if value_spec.Values != nil && len(value_spec.Values) > 0 {
					switch val := value_spec.Values[0].(type) {
					case *ast.BasicLit:
						v, err := strconv.Atoi(val.Value)
						return err == nil, v
					case *ast.Ident:
						return IsConst(val, pack)
					}
				}
			}
		}
	case *ast.SelectorExpr:
		obj := expr.Sel.Obj
		if obj != nil && obj.Kind == ast.Con {
			switch value_spec := obj.Decl.(type) {
			case *ast.ValueSpec:
				if value_spec.Values != nil && len(value_spec.Values) > 0 {
					switch val := value_spec.Values[0].(type) {
					case *ast.BasicLit:
						v, err := strconv.Atoi(val.Value)
						return err == nil, v
					case *ast.Ident:
						return IsConst(val, pack)
					}
				}
			}
		}
	case *ast.BasicLit:
		if expr.Kind == token.INT {
			val, err := strconv.Atoi(expr.Value)
			return err == nil, val
		}
	}
	return false, -1
}

func translateIdent(expr ast.Expr) (expr1 promela_ast.Ident) {
	switch e := expr.(type) {
	case *ast.Ident:
		expr1 = promela_ast.Ident{Name: e.Name}
	case *ast.SelectorExpr:
		expr1 = promela_ast.Ident{Name: translateIdent(e.X).Name + "_" + e.Sel.Name}
	}

	return expr1
}

func containsExpr(exprs []ast.Expr, expr ast.Expr) bool {
	for _, e := range exprs {
		switch e := e.(type) {
		case *ast.SelectorExpr:
			if IdenticalExpr(&ast.Ident{Name: translateIdent(e.X).Name + "_" + e.Sel.Name}, expr) {
				return true
			}
		default:
			if IdenticalExpr(e, expr) {
				return true
			}
		}
	}

	return false
}
func (m *Model) containsInline(expr ast.Expr) bool {
	for _, inline := range m.Inlines {
		if inline.Name.Name == m.getIdent(expr).Name {
			return true
		}
	}
	return false
}
func containsReturn(b *promela_ast.BlockStmt) bool {
	for _, stmt := range b.List {
		switch stmt.(type) {
		case *promela_ast.GotoEndStmt:
			return true
		}
	}
	return false
}

func (m *Model) ContainsRecFunc(pkg string, name string) bool {
	for _, fun := range m.RecFuncs {
		if pkg == fun.Pkg && name == fun.Name {
			return true
		}
	}

	return false
}

func (m *Model) AddRecFunc(pkg string, name string, decl *ast.FuncDecl) {
	m.RecFuncs = append(m.RecFuncs, RecFunc{Pkg: pkg, Name: name, Decl: decl})
}

func (m *Model) inDefine(name string) bool {
	found := false

	for _, define := range m.Defines {
		if define.Name.Name == name {
			found = true
		}
	}
	return found
}

func (m *Model) PrintFeature(f Feature) {
	if m.GenerateFeatures {
		Features = append(Features, f)
	}
}

func (m *Model) newModel(pack string, fun *ast.FuncDecl) *Model {
	return &Model{
		Props:                m.Props,

		Result_fodler:        m.Result_fodler,
		Project_name:         m.Project_name,
		Package:              pack,
		Name:                 m.Name,
		Commit:               m.Commit,

		RecFuncs:             []RecFunc{},
		SpawningFuncs:        m.SpawningFuncs,
		Defines:              m.Defines,
		Proctypes:            m.Proctypes,
		Inlines:              m.Inlines,
		Fun:                  fun,
		Chans:                make(map[ast.Expr]*ChanStruct),
		WaitGroups:           make(map[ast.Expr]*WaitGroupStruct),
		Mutexes:              []ast.Expr{},
		Init:                 m.Init,
		Global_vars:          m.Global_vars,
		CommPars:             []*CommPar{},
		Features:             []Feature{},
		ClosedVars:           make(map[*ChanStruct][]ast.Expr),

		For_counter:          m.For_counter,
		process_counter:      0,
		func_counter:         0,
		Counter:              m.Counter,
		AstMap:               m.AstMap,
		Projects_folder:      m.Projects_folder,
		GenerateFeatures:     m.GenerateFeatures,
		Current_return_label: "stop_process",

		Go_names:             m.Go_names,
		All_mandatory:        m.All_mandatory,
	}
}

func (m *Model) addNewProctypes(new_model *Model) {
	for _, proc := range new_model.Proctypes {
		contains := false
		for _, proc2 := range m.Proctypes {
			contains = contains || proc.Name.Name == proc2.Name.Name
		}

		if !contains {
			m.Proctypes = append(m.Proctypes, proc)
		}
	}
	for _, def := range new_model.Defines {
		contains := false
		for _, def2 := range m.Defines {
			contains = contains || def.Name.Name == def2.Name.Name
		}

		if !contains {
			m.Defines = append(m.Defines, def)
		}
	}
}

func getIdent(expr ast.Expr) *ast.Ident {
	switch expr := expr.(type) {
	case *ast.Ident:
		return expr
	case *ast.SelectorExpr:
		return expr.Sel
	case *ast.CallExpr:
		return getIdent(expr.Fun)
	}

	return nil
}

// Produces a pretty-printed AST expression.
func prettyPrint(expr ast.Expr) string {
	switch expr := expr.(type) {
	case *ast.CallExpr:

		name := prettyPrint(expr.Fun) + "("

		for i, arg := range expr.Args {
			name += prettyPrint(arg)

			if i < len(expr.Args)-1 {
				name += "."
			}
		}

		return name + ")"
	default:
		return fmt.Sprint(expr)
	}
}

// Produces a deep copy of the model.
func (m *Model) Clone() *Model {
	m2 := &Model{
		Props: m.Props,

		Result_fodler:        m.Result_fodler,
		Project_name:         m.Project_name,
		Package:              m.Package,
		Name:                 m.Name,
		Commit:               m.Commit,
		RecFuncs:             m.RecFuncs,
		SpawningFuncs:        m.SpawningFuncs,
		Fun:                  m.Fun,             // the function being modelled
		Mutexes:              m.Mutexes,         // The promela mutex declaration
		CommPars:             m.CommPars,        // the communications paramer
		Features:             m.Features,        // The features for the survey
		process_counter:      m.process_counter, // to give unique name to Promela processes
		func_counter:         m.func_counter,    // to give unique name to inline func call
		Counter:              m.Counter,
		AstMap:               m.AstMap,
		Projects_folder:      m.Projects_folder,
		GenerateFeatures:     m.GenerateFeatures, // should the model print features ?
		Current_return_label: m.Current_return_label,
		defer_counter:        m.defer_counter,

		Go_names:      m.Go_names,      // function that can be used to behave as spawning goroutines
		All_mandatory: m.All_mandatory, // turns all optionnal params into mandatory if true
	}

	if m.For_counter != nil {
		m2.For_counter = new(ForCounter)
		*m2.For_counter = *m.For_counter
	}

	if m.ClosedVars != nil {
		m2.ClosedVars = make(map[*ChanStruct][]ast.Expr)
		for c, es := range m.ClosedVars {
			m2.ClosedVars[c] = es
		}
	}
	for _, d := range m.Defines {
		d2 := (&d).Clone().(*promela_ast.DefineStmt)
		m2.Defines = append(m2.Defines, *d2)
	}
	for _, d := range m.Global_vars {
		m2.Global_vars = append(m2.Global_vars, d.Clone())
	}
	for _, d := range m.Inlines {
		d2 := d.Clone().(*promela_ast.Inline)
		m2.Inlines = append(m2.Inlines, d2)
	}
	for _, d := range m.Proctypes {
		d2 := d.Clone().(*promela_ast.Proctype)
		m2.Proctypes = append(m2.Proctypes, d2)
	}
	if m.Init != nil {
		m2.Init = m.Init.Clone().(*promela_ast.InitDef)
	}
	if m.WaitGroups != nil {
		m2.WaitGroups = make(map[ast.Expr]*WaitGroupStruct)
		for e, wg := range m.WaitGroups {
			m2.WaitGroups[e] = wg
		}
	}
	if m.Chans != nil {
		m2.Chans = make(map[ast.Expr]*ChanStruct)
		for e, c := range m.Chans {
			m2.Chans[e] = c
		}
	}
	return m2
}
