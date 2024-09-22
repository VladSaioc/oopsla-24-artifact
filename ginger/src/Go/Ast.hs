module Go.Ast where

import Utilities.Position
import Utilities.PrettyPrint (PrettyPrint, indent, multiline, prettyPrint)

newtype Prog = Prog [Pos Stmt] deriving (Eq, Ord, Read)

-- | Represents statement syntax in the Go programming language.
--
-- > S ::= skip
-- >    | return
-- >    | c := make(chan, E)
-- >    | w := WaitGroup
-- >    | break
-- >    | continue
-- >    | w.Add(E)
-- >    | w.Wait()
-- >    | c! | c? | *
-- >    | x := E
-- >    | x = E
-- >    | close(c)
-- >    | { {S; ...}* }
-- >    | if E { {S; ...}* } { {S: ...}* }
-- >    | select { {case C: {S; ... } ...}* [ default: {S; ... }* ] }
-- >    | for x E E D { {S; ... }* }
-- >    | while Exp { {S; ...}* }
-- >    | go { {S; ...}* }
data Stmt -- | Represents a skip statement
  --
  -- > skip
  = Skip
  -- | Represents a return statement
  --
  -- > return
  | Return
  -- | Represents a channel declaration with a channel name and an expression for the channel capacity
  --
  -- > c := make(chan, E)
  | Chan String Exp
  -- | Represents a WaitGroup declaration
  --
  -- > w := WaitGroup
  | Wgdef String
  -- | Represents a break statement
  --
  -- > break
  | Break
  -- | Represents a continue statement
  --
  -- > continue
  | Continue
  -- | Represents an addition to a WaitGroup counter
  --
  -- > w.Add(E)
  | Add Exp String
  -- | Represents waiting until a WaitGroup counter reaches 0
  --
  -- > w.Wait()
  | Wait String
  -- | Represents an atomic channel operation (send, receive or operate on external channel)
  --
  -- > c! | c? | *
  | Atomic CommOp
  -- | Represents a variable declaration and instantation with expression
  --
  -- > x := E
  | Decl String Exp
  -- | Represents an assignment to a variable
  --
  -- > x = E
  | As String Exp
  -- | Represents a close statement for a channel
  --
  -- > close(c)
  | Close String
  -- | Represents a block of statements enclosed in curly braces
  --
  -- > { S; ... }
  | Block [Pos Stmt]
  -- | Represents an if statement with a condition and two sets of statements for the true and false branches
  --
  -- > if E { S; ... } { S; ... }
  | If Exp [Pos Stmt] [Pos Stmt]
  -- | Represents a select statement with a list of communication operations and an optional default branch
  --
  -- > select { case o: { S; ... } ... [ default: { S; ... }* ] }
  | Select [(Pos CommOp, [Pos Stmt])] (Maybe [Pos Stmt])
  -- | Represents a for loop with a variable name, start expression, end expression, step difference, and a set of statements
  --
  -- > for x E E D { S; ... }
  | For String Exp Exp Diff [Pos Stmt]
  -- | Represents a while loop with a condition and a set of statements
  --
  -- > while E { S; ... }
  | While Exp [Pos Stmt]
  -- | Represents a goroutine statement with a set of statements
  --
  -- > go { S; ... }
  | Go [Pos Stmt]
  deriving (Eq, Ord, Read)

-- | Represents the growth factor in a loop, either increment or decrement.
--
-- > D ::= ++ | --
data Diff
  = Inc  -- ^ Increment (++).
  | Dec  -- ^ Decrement (--).
  deriving (Eq, Ord, Read)

-- | Represents channel operations.
--
-- > C ::= c!
-- >     | c?
-- >     | *
data CommOp
  -- | Send operation.
  --
  -- > c!
  = Send String
  -- | Receive operation.
  --
  -- > c?
  | Recv String
  -- | Wildcard operation (unknown channel operation).
  --
  -- > *
  | Star
  deriving (Eq, Ord, Read)

-- | Represents Go expressions:
--
-- > E ::= n
-- >     | true
-- >     | false
-- >     | E && E
-- >     | E || E
-- >     | !E
-- >     | E == E
-- >     | E != E
-- >     | E <= E
-- >     | E < E
-- >     | E >= E
-- >     | E > E
-- >     | E + E
-- >     | E - E
-- >     | -E
-- >     | E * E
-- >     | E / E
data Exp
  -- | Integer constant.
  --
  -- > n
  = CNum Int
  -- | Boolean constant true.
  --
  -- > true
  | CTrue
  -- | Boolean constant false.
  --
  -- > false
  | CFalse
  -- | Logical AND operation.
  --
  -- > E && E
  | And Exp Exp
  -- | Logical OR operation.
  --
  -- > E || E
  | Or Exp Exp
  -- | Logical NOT operation.
  --
  -- > !E
  | Not Exp
  -- | Equality comparison.
  --
  -- > E == E
  | Eq Exp Exp
  -- | Inequality comparison.
  --
  -- > E != E
  | Ne Exp Exp
  -- | Less than or equal to comparison.
  --
  -- > E <= E
  | Le Exp Exp
  -- | Less than comparison.
  --
  -- > E < E
  | Lt Exp Exp
  -- | Greater than or equal to comparison.
  --
  -- > E >= E
  | Ge Exp Exp
  -- | Greater than comparison.
  --
  -- > E > E
  | Gt Exp Exp
  -- | Addition operation.
  --
  -- > E + E
  | Plus Exp Exp
  -- | Subtraction operation.
  --
  -- > E - E
  | Minus Exp Exp
  -- | Multiplication operation.
  --
  -- > E * E
  | Mult Exp Exp
  -- | Unary negation.
  --
  -- > -E
  | Neg Exp
  -- | Variable reference.
  --
  -- > x
  | Var String
  -- | Division operation.
  --
  -- > E / E
  | Div Exp Exp
  deriving (Eq, Ord, Read)

instance Show Prog where
  show (Prog ss) = multiline $ map (prettyPrint 0) ss

instance Show Stmt where
  show = prettyPrint 0

instance PrettyPrint Stmt where
  prettyPrint n s =
    let block n' = multiline . map (prettyPrint $ n + n')
        tab = indent n
     in case s of
          Skip -> tab "skip"
          Break -> tab "break"
          Continue -> tab "continue"
          Return -> tab "return"
          Decl x e -> tab x ++ " := " ++ show e
          As x e -> tab x ++ " = " ++ show e
          Chan c e -> unwords [tab c, ":=", "make(chan,", show e ++ ")"]
          Wgdef w -> unwords [tab w, ":=", "WaitGroup"]
          Add e w -> tab $ w ++ ".Add(" ++ show e ++ ")"
          Wait w -> tab $ w ++ ".Wait()"
          Atomic o -> tab $ show o
          Close c -> tab "close(" ++ c ++ ")"
          Block ss -> block 0 ss
          If e s1 s2 ->
            let s2' = [unwords [tab "}, else", multiline ["{", block 1 s2]] | s2 /= []]
             in multiline $
                  [ tab $ unwords ["if", show e, "{"],
                    block 1 s1
                  ]
                    ++ s2'
                    ++ [tab "}"]
          Go ss -> multiline [tab "go {", block 1 ss, tab "}"]
          While e ss ->
            multiline
              [ unwords [tab "for", show e, "{"],
                block 1 ss,
                tab "}"
              ]
          Select cs dfs ->
            let showCase (Pos _ c, ss) =
                  multiline
                    [ unwords [indent (n + 1) "case", show c ++ ":"],
                      block 2 ss
                    ]
                defCase = case dfs of
                  Nothing -> []
                  Just dfs' -> [multiline [indent (n + 1) "default:", block 2 dfs']]
             in multiline $
                  [tab "select {"]
                    ++ map showCase cs
                    ++ defCase
                    ++ [tab "}"]
          For x e1 e2 diff ss ->
            multiline
              [ unwords [tab "for", x, "=", show e1 ++ ";", x, "<=", show e2 ++ ";", x ++ show diff, "{"],
                block 1 ss,
                tab "}"
              ]

instance Show Exp where
  show =
    let bin e1 op e2 = unwords ["(" ++ show e1 ++ ")", op, "(" ++ show e2 ++ ")"]
        un op e = op ++ "(" ++ show e ++ ")"
     in \case
          CTrue -> "true"
          CFalse -> "false"
          And e1 e2 -> bin e1 "&&" e2
          Or e1 e2 -> bin e1 "||" e2
          Le e1 e2 -> bin e1 "<=" e2
          Lt e1 e2 -> bin e1 "<" e2
          Ge e1 e2 -> bin e1 ">=" e2
          Gt e1 e2 -> bin e1 ">" e2
          Eq e1 e2 -> bin e1 "==" e2
          Ne e1 e2 -> bin e1 "!=" e2
          Not e -> un "!" e
          Neg e -> un "-" e
          Plus e1 e2 -> bin e1 "+" e2
          Minus e1 e2 -> bin e1 "-" e2
          Mult e1 e2 -> bin e1 "*" e2
          Div e1 e2 -> bin e1 "/" e2
          CNum n -> show n
          Var x -> x

instance Show CommOp where
  show = \case
    Send c -> c ++ " <-"
    Recv c -> "<-" ++ c
    Star -> "*"

instance Show Diff where
  show = \case
    Inc -> "++"
    Dec -> "--"
