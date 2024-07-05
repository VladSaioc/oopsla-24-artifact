module Backend.Ast where

import Data.List (intercalate)

import Utilities.PrettyPrint

-- | Back-end type syntax:
--
-- > 𝑇 ::= int | nat | bool | 𝑥 | 𝑇 -> 𝑇 | ({𝑇, ...}*)
data Type
  = TBad
  | -- | The type of integers
    --
    -- > int
    TInt
  | -- | The type of natural numbers
    --
    -- nat
    TNat
  | -- | The type of booleans
    --
    -- > bool
    TBool
  | -- | A named type variable
    --
    -- > 𝑥
    TVar String
  | -- | Function types
    --
    -- > 𝑇 -> 𝑇
    Type :-> Type
  | -- | Tuple types
    --
    -- > (𝑇, ...)
    Tuple [Type]
  deriving (Eq, Ord, Read)

-- | Back-end pattern matching syntax:
--
-- > 𝑃 ::= _ | 𝑐
data Pattern
  = -- | Discarded pattern variable
    --
    -- >  _
    Wildcard
  | -- | Pattern constant
    --
    -- > 𝑐
    PCon Const
  deriving (Eq, Ord, Show, Read)

-- | Back-end statement syntax:
--
-- > 𝑆 ::= 𝑥 := 𝐸
-- >    | { 𝑆; ... }
-- >    | [ghost] var 𝑥 [: 𝑇] := 𝐸
-- >    | if 𝐸 { 𝑆 } else { 𝑆 }
-- >    | match 𝐸 { {case p => 𝑆 ...}* }
-- >    | while 𝐸 {invariant 𝐸 ...}* { 𝑆 }
-- >    | return {𝐸, ...}*
data Stmt
  = -- | Assignment statement
    --
    -- > 𝑥 := 𝐸
    Assign String Exp
  | -- | Block of statements
    --
    -- > { 𝑆; ... }
    Block [Stmt]
  | -- | Variable definition statement with optional type annotation
    --
    -- > [ghost] var x [: 𝑇] := 𝐸
    VarDef Bool String (Maybe Type) Exp
  | -- | Conditional if statement with optional else branch
    --
    -- > if 𝐸 { 𝑆 } [else { 𝑆 }]
    If Exp Stmt (Maybe Stmt)
  | -- | Match statement
    --
    -- > match 𝐸 { {case 𝑃 => 𝑆 ...}* }
    MatchStmt Exp [(Pattern, Stmt)]
  | -- | While loop with optional invariants
    --
    -- > while 𝐸 {invariant 𝐸 ...}*
    While Exp [Exp] Stmt
  | -- | Return statement
    --
    -- > return {𝐸, ...}*
    Return [Exp]
  deriving (Eq, Ord, Read)

-- | Back-end constants:
-- > c ::= true | false | n
data Const
  = -- | > true
    CTrue
  | -- | > false
    CFalse
  | -- | > n ∈ ℤ
    CNum Int
  deriving (Eq, Ord, Show, Read)

-- | Back-end expressions:
--
-- > 𝐸 ::= match 𝐸 { {case p => 𝐸 ...}* }
-- >    | if 𝐸 then 𝐸 else 𝐸
-- >    | exists {x [: 𝑇], ...}* :: 𝐸
-- >    | forall {x [: 𝑇], ...}* :: 𝐸
-- >    | 𝐸 in 𝐸
-- >    | {{𝐸, ...} *}
-- >    | 𝐸 <==> 𝐸 | 𝐸 ==> 𝐸
-- >    | 𝐸 && 𝐸 | 𝐸 || 𝐸
-- >    | 𝐸 == 𝐸 | 𝐸 != 𝐸
-- >    | 𝐸 >= 𝐸 | 𝐸 > 𝐸
-- >    | 𝐸 <= 𝐸 | 𝐸 < 𝐸
-- >    | 𝐸 + 𝐸 | 𝐸 - 𝐸
-- >    | 𝐸 * 𝐸 | 𝐸 / 𝐸 | 𝐸 % 𝐸
-- >    | !𝐸 | -𝐸
-- >    | ({𝐸, ...}*)
-- >    | * | x | c | f({𝐸, ...}*)
data Exp
  = -- COMPOUND EXPRESSIONS
    -- | > match 𝐸 { {case p => 𝐸 ...}* }
    Match Exp [(Pattern, Exp)]
  | -- | > if 𝐸 then 𝐸 else 𝐸
    IfElse Exp Exp Exp
  | -- PROPOSITIONAL QUANTIFIERS
    -- | > exists {x [: 𝑇], ...}* :: 𝐸
    Exists [(String, Maybe Type)] Exp
  | -- | > forall {x [: 𝑇], ...}* :: 𝐸
    Forall [(String, Maybe Type)] Exp
  | -- | > 𝐸 in 𝐸
    In Exp Exp
  | -- | > {{𝐸, ...} *}
    ESet [Exp]
  | -- BINARY OPERATORS
    -- Propositional logic
    -- | > 𝐸 <==> 𝐸
    Exp :<==> Exp
  | -- | > 𝐸 ==> 𝐸
    Exp :==> Exp
  | -- Boolean arithmetic
    -- | > 𝐸 && 𝐸
    Exp :&& Exp
  | -- | > 𝐸 || 𝐸
    Exp :|| Exp
  | -- Comparison
    -- | > 𝐸 == 𝐸
    Exp :== Exp
  | -- | > 𝐸 != 𝐸
    Exp :!= Exp
  | -- | > 𝐸 >= 𝐸
    Exp :>= Exp
  | -- | > 𝐸 > 𝐸
    Exp :> Exp
  | -- | > 𝐸 <= 𝐸
    Exp :<= Exp
  | -- | > 𝐸 < 𝐸
    Exp :< Exp
  | -- Arithmetic
    -- | > 𝐸 + 𝐸
    Exp :+ Exp
  | -- | > 𝐸 - 𝐸
    Exp :- Exp
  | -- | > 𝐸 * 𝐸
    Exp :* Exp
  | -- | > 𝐸 / 𝐸
    Exp :/ Exp
  | -- | > 𝐸 % 𝐸
    Exp :% Exp
  | -- UNARY OPERATORS
    -- | > !𝐸
    Not Exp
  | -- | > -𝐸
    Neg Exp
  | -- | > (𝐸, ... 𝐸)
    ETuple [Exp]
  | -- TERMINAL EXPRESSIONS
    -- | Variables
    --
    -- > x
    EVar String
  | -- | Constants
    --
    -- > c
    ECon Const
  | -- | Function calls
    --
    -- > f({𝐸, ...}*)
    Call String [Exp]
  deriving (Eq, Ord, Read)

-- | Back-end record type definition:
--
-- > 𝑇({x : 𝑇, ...}*)
data Cons = Cons String [(String, Type)] deriving (Eq, Ord, Read)

-- | Back-end Hoare triple syntax. The underscores between angle brackets on the left-hand side
-- are holes, which are filled with the appropriate keyword and return syntax based on how the
-- Hoare triple is used i.e., for a method, or for a pure function:
--
-- > H<_; _> ::= _ 𝑥[\<{𝑇, ...}>]({𝑥 : 𝑇, ...}*) _
-- >      {requires 𝐸\n...}*
-- >      {ensures 𝐸\n...}*
-- >      {decreases 𝐸\n...}*
data HoareWrap = HoareWrap
  { ghost :: Bool,
    name :: String,
    types :: [Type],
    params :: [(String, Type)],
    decreases :: [Exp],
    requires :: [Exp],
    ensures :: [Exp]
  }
  deriving (Eq, Ord, Read)

-- | Back-end function declaration syntax:
--
-- > F ::= H<[ghost] function, : 𝑇> 𝐸
data Function = Function
  { yields :: Type,
    funcHoare :: HoareWrap,
    funcBody :: Exp
  }
  deriving (Eq, Ord, Read)

-- | Method declaration syntax:
--
-- > M ::= H<lemma | method, returns ({ 𝑥 : 𝑇, ...}*)> { {𝑆; ...}* }
data Method = Method
  { methodReturns :: [(String, Type)],
    methodHoare :: HoareWrap,
    methodBody :: Stmt
  }
  deriving (Eq, Ord, Read)

-- | Back-end top-level declaration:
--
-- > 𝐷 ::= datatype 𝑥<{𝑇, ...}> = {Cons | ...}
-- >    | const 𝑥 := 𝐸
-- >    | type 𝑥 = 𝑇
-- >    | [ghost] function f({𝑥 : 𝑇, ...}*) : 𝑇 {requires 𝐸}* {ensures 𝐸}* { 𝐸 }
-- >    | (lemma | method) function f({𝑥 : 𝑇, ...}*) : 𝑇 {requires 𝐸}* {ensures 𝐸}* { 𝐸 }
data Decl
  = -- | > datatype 𝑥<{𝑇, ...}> = {Cons | ...}
    Datatype String [Type] [Cons]
  | -- | > const 𝑥 := 𝐸
    CDecl String Exp
  | -- | > type 𝑥 = Type
    TypeDecl String Type
  | -- [ghost] function f({𝑥 : 𝑇, ...}*) : 𝑇 {requires 𝐸}* {ensures 𝐸}* { 𝐸 }
    FDecl Function
  | -- (lemma | method) f({𝑥 : 𝑇, ...}*) returns ({𝑥 : 𝑇, ...}*)  {requires 𝐸 ...}* {ensures 𝐸 ...}* {decreases 𝐸 ...}* { {𝑆; ...}* }
    MDecl Method
  deriving (Eq, Ord, Read)

-- | Back-end program syntax:
--
-- > P ::= {𝐷\n ...}*
newtype Program = Program [Decl] deriving (Eq, Ord, Read)

-- | Unparser precedence order helper for binary operations.
-- It is used to cut down on the amount of parentheses generated in the back-end,
-- by not wrapping sub-tree binary expressions in parentheses when their operations have
-- higher precedence than or are associative with the parent.
(<.|.>) :: (Exp -> String) -> Exp -> Either Exp Exp -> String
(<.|.>) f e1 lre2 =
  let needParens =
        ( case (e1, lre2) of
            -- Equivalence is associative
            (_ :<==> _, Right (_ :<==> _)) -> False
            (_ :<==> _, Left (_ :<==> _)) -> False
            -- Implication is only right-associative
            (_ :==> _, Left (_ :==> _)) -> True
            (_ :==> _, Right (_ :==> _)) -> False
            -- Conjunction is associative
            (_ :&& _, Right (_ :&& _)) -> False
            (_ :&& _, Left (_ :&& _)) -> False
            -- Disjunction is associative
            (_ :|| _, Right (_ :|| _)) -> False
            (_ :|| _, Left (_ :|| _)) -> False
            -- Conjunction and disjunction are not associative
            (_ :&& _, Right (_ :|| _)) -> True
            (_ :&& _, Left (_ :|| _)) -> True
            (_ :|| _, Right (_ :&& _)) -> True
            (_ :|| _, Left (_ :&& _)) -> True
            -- Equality is not associative
            (_ :== _, Right (_ :== _)) -> True
            (_ :== _, Left (_ :== _)) -> True
            (_ :== _, Right (_ :!= _)) -> True
            (_ :== _, Left (_ :!= _)) -> True
            -- Addition is associative
            (_ :+ _, Right (_ :+ _)) -> False
            (_ :+ _, Left (_ :+ _)) -> False
            -- Addition is right-associative with subtraction
            (_ :+ _, Right (_ :- _)) -> False
            (_ :+ _, Left (_ :- _)) -> False
            -- Subtraction is not right-associative
            (_ :- _, Right (_ :- _)) -> True
            -- Subtraction is left-associative
            (_ :- _, Left (_ :- _)) -> False
            -- Subtraction is left-associative with addition
            (_ :- _, Left (_ :+ _)) -> False
            -- Subtraction is not right-associative with addition
            (_ :- _, Right (_ :+ _)) -> True
            -- Multiplication is associative
            (_ :* _, Right (_ :* _)) -> False
            (_ :* _, Left (_ :* _)) -> False
            -- Multiplication is right-associative with division
            (_ :* _, Right (_ :/ _)) -> False
            (_ :* _, Left (_ :/ _)) -> False
            -- Division is not right-associative
            (_ :/ _, Right (_ :/ _)) -> True
            -- Division is left-associative
            (_ :/ _, Left (_ :/ _)) -> False
            -- Division is left-associative with multiplication
            (_ :/ _, Left (_ :* _)) -> False
            -- Division is not right-associative with multiplication
            (_ :/ _, Right (_ :* _)) -> True
            -- Modulo is not associative
            (_ :% _, Left (_ :% _)) -> True
            (_ :% _, Right (_ :% _)) -> True
            _ -> e1 > either id id lre2
        )
      trans = (if needParens then ("(" ++) . (++ ")") else id) . f
   in either trans trans lre2

-- | Unparser precedence order helper for unary operations.
-- It is used to cut down on the amount of parentheses generated in the back-end,
-- by not wrapping sub-tree unary expressions in parentheses when their operations have
-- higher precedence than the parent.
(<.>) :: Exp -> Exp -> String
(<.>) e1 e2 =
  let needParens =
        ( case (e1, e2) of
            (Not {}, Not {}) -> False
            (Neg {}, Neg {}) -> True
            _ -> e1 > e2
        )
      trans = (if needParens then ("(" ++) . (++ ")") else id) . prettyPrint 0
   in trans e2

instance Show Type where
  show = prettyPrint 0

instance Show Cons where
  show = prettyPrint 0

instance Show Exp where
  show = prettyPrint 0

instance Show Program where
  show = prettyPrint 0

instance Show Method where
  show = prettyPrint 0

instance Show Function where
  show = prettyPrint 0

instance Show Stmt where
  show = prettyPrint 0

-- Unparser implementation for the Dafny back-end. Relevant for code generation.

-- Pretty printer
instance PrettyPrint Type where
  prettyPrint _ =
    let pp = prettyPrint 0
     in \case
          TBad -> "err"
          TInt -> "int"
          TNat -> "nat"
          TBool -> "bool"
          TVar x -> x
          t1 :-> t2 -> concat ["(" ++ pp t1 ++ ")", " -> ", "(" ++ pp t2 ++ ")"]
          Tuple ts -> "(" ++ intercalate ", " (map pp ts) ++ ")"

instance PrettyPrint Pattern where
  prettyPrint _ = \case
    Wildcard -> "_"
    PCon c -> prettyPrint 0 c

instance PrettyPrint Stmt where
  prettyPrint i s =
    let pp = prettyPrint i
        ind = indent i ""
        ind1 = indent (i + 1) ""
        s' = case s of
          Assign x e ->
            unwords [x, ":=", prettyPrint 0 e ++ ";"]
          Block ss ->
            if null ss
              then ""
              else
                "{\n"
                  ++ ind1
                  ++ intercalate ("\n" ++ ind1) (map (prettyPrint $ i + 1) ss)
                  ++ "\n"
                  ++ ind
                  ++ "}"
          VarDef g x mt e ->
            let t = maybe "" (\t' -> unwords [":", prettyPrint (i + 1) t']) mt
                g' = (["ghost" | g])
             in unwords (g' ++ ["var", x, t, ":=", prettyPrint (i + 1) e ++ ";"])
          If e s1 ms2 ->
            let s2 = maybe "" (\s2' -> "\n" ++ ind ++ unwords ["else", pp s2']) ms2
             in unwords ["if", prettyPrint 0 e, prettyPrint i s1]
                  ++ s2
          MatchStmt e cs ->
            let def (p, s'') = "\n" ++ ind ++ unwords ["case", prettyPrint i p, "=>", prettyPrint (i + 1) s'']
                cs' = map def cs
             in unwords ["match", prettyPrint (i + 1) e, "{"]
                  ++ concat cs'
                  ++ ("\n" ++ ind ++ "}")
          While e es s'' ->
            let e' = prettyPrint 0 e
                cons kw e'' = "\n" ++ ind1 ++ unwords [kw, prettyPrint (i + 1) e'']
                es' = concatMap (cons "invariant") es ++ " "
             in unwords ["while", e'] ++ es' ++ prettyPrint i s''
          Return es -> unwords ["return", intercalate ", " (map (prettyPrint i) es)] ++ ";"
     in s'

instance PrettyPrint Const where
  prettyPrint _ = \case
    CTrue -> "true"
    CFalse -> "false"
    CNum n -> show n

instance PrettyPrint Exp where
  prettyPrint i e =
    let pp = prettyPrint i
        tab = indent i
        quantifier q xs e' =
          let def (x, mt) =
                let t' = maybe "" ((" : " ++) . prettyPrint i) mt
                 in x ++ t'
              xs' = intercalate ", " $ map def xs
              e'' = pp e'
           in unwords [q, xs', "::", e'']
        bin e1 op e2 = unwords [(<.|.>) pp e (Left e1), op, (<.|.>) pp e (Right e2)]
        un op e' = unwords [op ++ e <.> e']
     in case e of
          ETuple ps -> "(" ++ intercalate ", " (map pp ps) ++ ")"
          EVar x -> x
          ECon c -> prettyPrint 0 c
          In e1 e2 -> bin e1 "in" e2
          ESet es -> unwords ["{", intercalate ", " $ map pp es, "}"]
          Exists xs e' -> quantifier "exists" xs e'
          Forall xs e' -> quantifier "forall" xs e'
          e1 :==> e2 -> bin e1 "==>" e2
          e1 :<==> e2 -> bin e1 "<==>" e2
          e1 :&& e2 -> bin e1 "&&" e2
          e1 :|| e2 -> bin e1 "||" e2
          Not e' -> un "!" e'
          Neg e' -> un "-" e'
          e1 :== e2 -> bin e1 "==" e2
          e1 :!= e2 -> bin e1 "!=" e2
          e1 :>= e2 -> bin e1 ">=" e2
          e1 :> e2 -> bin e1 ">" e2
          e1 :<= e2 -> bin e1 "<=" e2
          e1 :< e2 -> bin e1 "<" e2
          e1 :+ e2 -> bin e1 "+" e2
          e1 :- e2 -> bin e1 "-" e2
          e1 :* e2 -> bin e1 "*" e2
          e1 :/ e2 -> bin e1 "/" e2
          e1 :% e2 -> bin e1 "%" e2
          IfElse e1 e2 e3 -> unwords ["if", pp e1, "then", pp e2, "else", pp e3]
          Match e' cs ->
            let def (p, e'') = unwords [tab "case", prettyPrint i p, "=>", pp e'']
                cs' = map def cs
             in unwords ["match", pp e', "{\n"]
                  ++ unlines cs'
                  ++ ("\n" ++ tab "}")
          Call f es -> f ++ "(" ++ intercalate ", " (map pp es) ++ ")"

instance PrettyPrint Cons where
  prettyPrint i (Cons n fs) =
    let fdef (f, t) = unwords [f, ":", prettyPrint i t]
     in n ++ "(" ++ intercalate ", " (map fdef fs) ++ ")"

instance PrettyPrint Function where
  prettyPrint _ Function {yields, funcHoare, funcBody} = case funcHoare of
    HoareWrap {ghost, name, types, params, decreases, requires, ensures} ->
      let ps = intercalate ", " (map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) params)
          ts =
            if null types
              then ""
              else "<" ++ intercalate ", " (map (prettyPrint 0) types) ++ ">"
          header =
            unwords $
              ["ghost" | ghost]
                ++ [ "function",
                     name ++ ts ++ "(" ++ ps ++ ")",
                     ":",
                     prettyPrint 0 yields
                   ]
          pre = prop "requires" requires
          post = prop "ensures" ensures
          dec = prop "decreases" decreases
          props = intercalate "\n" (pre ++ post ++ dec)
          body = prettyPrint 1 funcBody
          prop kw = map (\e -> indent 1 $ unwords [kw, prettyPrint 2 e])
       in unlines [header, props ++ " {", body, "}"]

instance PrettyPrint Method where
  prettyPrint _ Method {methodReturns, methodHoare, methodBody} = case methodHoare of
    HoareWrap {ghost, name, types, params, decreases, requires, ensures} ->
      let ps = intercalate ", " (map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) params)
          ts =
            if null types
              then ""
              else "<" ++ intercalate ", " (map (prettyPrint 0) types) ++ ">"
          rps = map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) methodReturns
          method = if ghost then "lemma" else "method"
          header = unwords [method, name ++ ts ++ "(" ++ ps ++ ")", "returns", "(" ++ intercalate ", " rps ++ ")"]
          pre = prop "requires" requires
          post = prop "ensures" ensures
          dec = prop "decreases" decreases
          props = intercalate "\n" (pre ++ post ++ dec)
          body = prettyPrint 0 methodBody
          prop kw = map (\e -> indent 1 $ unwords [kw, prettyPrint 2 e])
       in intercalate "\n" [header, props, body]

instance PrettyPrint Decl where
  prettyPrint _ = \case
    Datatype s ts cs ->
      let ts' = intercalate ", " (map show ts)
          cs' = intercalate " | " (map show cs)
       in unwords ["datatype", s, "<" ++ ts' ++ ">", "=", cs']
    CDecl x e -> unwords ["const", x, ":=", show e]
    TypeDecl x t -> unwords ["type", x, "=", prettyPrint 0 t]
    FDecl f -> prettyPrint 0 f
    MDecl m -> prettyPrint 0 m

instance PrettyPrint Program where
  prettyPrint _ (Program ds) = intercalate "\n\n" (map (prettyPrint 0) ds)
