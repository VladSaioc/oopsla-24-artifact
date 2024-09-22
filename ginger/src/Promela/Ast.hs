module Promela.Ast where

import Data.List (intercalate)
import Utilities.Position
import Utilities.PrettyPrint


-- | An alias for Promela identifier names.
type Ident = String

-- | The syntax of a Promela specification.
--
-- > S ::= ϵ | M S
newtype Spec = Spec [Module] deriving (Eq, Ord, Read)

-- | Promela module syntax:
--
-- > M ::= #define x T V
-- >    | init { {S; ...}* }
-- >    | proctype f ({P; ...}*) { {S; ...}* }
-- >    | typedef x { {D; ...}* }
data Module
  = -- | Top-level constant declaration.
    -- 
    -- > #define x T v
    TopDecl Ident Type Val
    -- | Entry-point process declaration.
    --
    -- > init { {S; ...}* }
  | Init [Pos Stmt]
    -- | Process declaration.
    --
    -- > proctype f ({P; ...}*) { {S; ...}* }
  | Proc Ident [Param] [Pos Stmt]
    -- | Custom type declaration.
    --
    -- > typedef x { {D; ...}* }
  | Typedef Ident [Field]
  deriving (Eq, Ord, Read)

-- | An alias for struct fields.
--
-- > D ::= T x [= E]
type Field = (Ident, Type, Maybe Exp)

-- | An alias for function parameters
--
-- > P ::= T x
type Param = (Ident, Type)

-- | An alias for a 'case' branch
--
-- > C ::= :: S -> {S; ...}*
type Case = (Pos Stmt, [Pos Stmt])

-- | Promela statement syntax:
--
-- > S ::= T x [= E]
-- >    | if {C ...}* [else :: {S; ...}*] fi
-- >    | do {C ...}* [else :: {S; ...}*] od
-- >    | for R { {S; ...}* }
-- >    | L ::= E
data Stmt
  = Decl Ident Type (Maybe Exp)                -- ^ Variable declaration: T x [= E]
  | If [(Pos Stmt, [Pos Stmt])] (Maybe [Pos Stmt])  -- ^ If statement with optional else: if {C ...}* [else :: {S; ...}*] fi
  | Do [(Pos Stmt, [Pos Stmt])] (Maybe [Pos Stmt])  -- ^ Do-while loop with optional else: do {C ...}* [else :: {S; ...}*] od
  | For Range [Pos Stmt]                       -- ^ For loop: for R { {S; ...}* }
  | As LVal Exp                                 -- ^ Assignment statement: L ::= E
  | Goto Ident                                  -- ^ Goto statement: goto x
  | Break                                       -- ^ Break statement: break
  | Skip                                        -- ^ Skip statement: skip
  | Assert Exp                                  -- ^ Assert statement: assert E
  | Recv LVal [Exp]                             -- ^ Receive statement: L?{E, ...}*
  | Send LVal [Exp]                             -- ^ Send statement: L!{E, ...}*
  | ExpS Exp                                    -- ^ Expression statement: E
  | Label Ident                                -- ^ Label statement: x:
  deriving (Eq, Ord, Read)

-- | Promela expression syntax:
--
-- > E ::= [E] of {?}
-- >    | V | L
-- >    | E1 && E2 | E1 || E2 | !E
-- >    | E1 == E2 | E1 != E2
-- >    | E1 <= E2 | E1 < E2
-- >    | E1 >= E2 | E1 > E2
-- >    | E1 + E2 | E1 - E2 | -E
-- >    | E1 * E2 | E1 / E2
-- >    | len(L) | run x ({E, ...}*)
data Exp
  = Chan Exp                  -- ^ Channel expression: [E] of {?}
  | Const Val                 -- ^ Constant value expression: V
  | And Exp Exp               -- ^ Logical AND expression: E1 && E2
  | Or Exp Exp                -- ^ Logical OR expression: E1 || E2
  | Eq Exp Exp                -- ^ Equality expression: E1 == E2
  | Ne Exp Exp                -- ^ Inequality expression: E1 != E2
  | Le Exp Exp                -- ^ Less than or equal expression: E1 <= E2
  | Lt Exp Exp                -- ^ Less than expression: E1 < E2
  | Ge Exp Exp                -- ^ Greater than or equal expression: E1 >= E2
  | Gt Exp Exp                -- ^ Greater than expression: E1 > E2
  | Plus Exp Exp              -- ^ Addition expression: E1 + E2
  | Minus Exp Exp             -- ^ Subtraction expression: E1 - E2
  | Mult Exp Exp              -- ^ Multiplication expression: E1 * E2
  | Div Exp Exp               -- ^ Division expression: E1 / E2
  | Neg Exp                   -- ^ Negation expression: -E
  | Not Exp                   -- ^ Logical NOT expression: !E
  | Len LVal                  -- ^ Length expression: len(L)
  | EVar LVal                 -- ^ Variable expression: L
  | Run Ident [Exp]           -- ^ Run process expression: run x ({E, ...}*)
  deriving (Eq, Ord, Read)

-- | Promela syntax for range clauses in for loops:
--
-- > R ::= x : E1 .. E2 | x in x
data Range
  = -- | Iterate over a finite integer range.
    --
    -- > x : E1 .. E2
    Between Ident Exp Exp
    -- | Iterate over an array
    --
    -- > x in x
  | In Ident Ident
  deriving (Eq, Ord, Read)

-- | Promela l-values syntax:
--
-- > L ::= x | L.x | L[E]
data LVal
  = -- | Simple variable:
    --
    -- > x
    Var Ident
    -- | Structure field access:
    --
    -- > L.x
  | Field LVal Ident
    -- | Array access:
    --
    -- > L[E]
  | Arr LVal Exp
  deriving (Eq, Ord, Read)

-- | The syntax of Promela values:
--
-- > V ::= ?? | n | true | false, where n ∈ ℤ
data Val = 
      -- | The free Promela value. Denotes a name with an unknown value.
      --
      -- > ??
      Free 
      -- | A Promela integer.
    | VInt Int 
      -- | A Promela boolean.
    | VBool Bool deriving (Eq, Ord, Read)

-- | The syntax of Promela types.
--
-- > T ::= int | bool | chan | x
data Type
  = TInt      -- ^ Integer type: int
  | TBool     -- ^ Boolean type: bool
  | TChan     -- ^ Channel type: chan
  | TNamed Ident  -- ^ Named type: x
  deriving (Eq, Ord, Read)

block :: PrettyPrint a => Int -> [a] -> String
block n ss = multiline $ map ((++ ";") . prettyPrint n) ss

instance Show Spec where
  show (Spec ms) = multiline $ map show ms

instance Show Module where
  show = \case
    TopDecl x _ v -> unwords ["#define ", x, show v]
    Init ss -> multiline $ ["init {"] ++ map (prettyPrint 1) ss ++ ["}"]
    Proc f ps ss ->
      let params = intercalate "; " $ map showParam ps
          header = unwords ["proctype", f ++ "(" ++ params ++ ")", "{"]
       in multiline $ header : map (prettyPrint 1) ss
    Typedef x fs -> unwords ["typedef", x ++ "{", multiline $ map ((++ ";") . prettyPrintField 1) fs, "}"]

instance Show Stmt where
  show = prettyPrint 0

showParam :: Show a => (String, a) -> String
showParam (x, t) = unwords [show t, x]

instance PrettyPrint Stmt where
  prettyPrint n =
    let tab = indent n
        makeElse =
          maybe
            ""
            ( \ss ->
                multiline
                  [ tab "else  ->",
                    block (n + 1) ss
                  ]
            )
     in \case
          Decl x t me -> prettyPrintField n (x, t, me)
          If os els ->
            multiline
              [ tab "if",
                multiline $ map (prettyPrintCase n) os,
                makeElse els,
                tab "fi"
              ]
          Do os els ->
            multiline
              [ tab "do",
                multiline $ map (prettyPrintCase n) os,
                makeElse els,
                tab "od"
              ]
          For r ss ->
            multiline
              [ tab "for(" ++ show r ++ ") {",
                multiline $ map (prettyPrint $ n + 1) ss,
                tab "}"
              ]
          As x e -> unwords [tab (show x), "=", show e]
          Goto l -> unwords ["goto", l]
          Break -> tab "break"
          Skip -> tab "skip"
          Assert e -> unwords [tab "assert", show e]
          Recv c es -> tab $ show c ++ "?" ++ intercalate ", " (map show es)
          Send c es -> tab $ show c ++ "!" ++ intercalate ", " (map show es)
          ExpS e -> tab $ show e
          Label l -> unwords [tab l ++ ":", "skip"]

instance Show Range where
  show = \case
    Between x e1 e2 -> unwords [x, ":", show e1, "..", show e2]
    In x1 x2 -> unwords [x1, "in", x2]

prettyPrintCase :: (Show a1, PrettyPrint a2) => Int -> (a1, [a2]) -> String
prettyPrintCase n (c, ss) =
  multiline
    [ unwords [indent n "::", show c, "->"],
      block (n + 1) ss
    ]

instance Show Val where
  show = \case
    Free -> "??"
    VInt n -> show n
    VBool True -> "true"
    VBool False -> "false"

instance Show Type where
  show = \case
    TInt -> "int"
    TBool -> "bool"
    TChan -> "chan"
    TNamed x -> x

prettyPrintField :: (Show a1, Show a2) => Int -> (String, a1, Maybe a2) -> String
prettyPrintField n (f, t, me) =
  let e = maybe "" show me
   in unwords [indent n (show t), f, "=", e]

instance Show Exp where
  show =
    let bin e1 op e2 = unwords ["(" ++ show e1 ++ ")", op, "(" ++ show e2 ++ ")"]
        un op e = op ++ "(" ++ show e ++ ")"
     in \case
          Chan e -> unwords ["[" ++ show e ++ "]", "of", "{?}"]
          Const v -> show v
          And e1 e2 -> bin e1 "&&" e2
          Or e1 e2 -> bin e1 "||" e2
          Eq e1 e2 -> bin e1 "==" e2
          Ne e1 e2 -> bin e1 "!=" e2
          Le e1 e2 -> bin e1 "<=" e2
          Lt e1 e2 -> bin e1 "<" e2
          Ge e1 e2 -> bin e1 ">=" e2
          Gt e1 e2 -> bin e1 ">" e2
          Plus e1 e2 -> bin e1 "+" e2
          Minus e1 e2 -> bin e1 "-" e2
          Mult e1 e2 -> bin e1 "*" e2
          Div e1 e2 -> bin e1 "/" e2
          Neg e -> un "-" e
          Not e -> un "1" e
          Len v -> un "len" v
          EVar v -> show v
          Run f es -> unwords ["run", f ++ "(" ++ intercalate ", " (map show es) ++ ")"]

instance Show LVal where
  show = \case
    Var x -> x
    Field x f -> show x ++ "." ++ f
    Arr x e -> show x ++ "[" ++ show e ++ "]"
