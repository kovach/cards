module Types where

import qualified Data.Map as M

{- TODO
  * state components
    * entity properties
    * rule list
    * predicates
  - state update
    * insert rules
    * update prop
  - syntax types
    - patterns
    - rhs
    - rules
  - syntax parser
  - lists?
  - GC
    - removal for
      - rules
      - predicates
-}

data Ref = R Int
  deriving (Eq, Ord)

instance Show Ref where
  show (R i) = "#" ++ show i

data Value =
  VInt Int
  | VString String
  | VRef Ref
  deriving (Show, Eq, Ord)

data Expr =
  EVal Value
  | EApp Expr Expr
  deriving (Show, Eq, Ord)

type Symbol = String

type Map = M.Map

-- HMM! Value or Expr?
type Binding = [(Symbol, Value)]

-- State of a monad
type Dict = Map Symbol Value
-- State of all monads
data Cache = C (Map Ref Dict)
  deriving (Show, Eq, Ord)

type Priority = Int
-- TODO
data Rule = Rule
  deriving (Show, Eq, Ord)

type RuleTuple = (Priority, (Ref, Rule, Binding))
data RuleBook = RB [RuleTuple]
  deriving (Show, Eq, Ord)

data Predicate = Pred
  { tag :: Symbol
  , arguments :: [Ref] }
  deriving (Show, Eq, Ord)

data Facts = Facts [Predicate]
  deriving (Show, Eq, Ord)


data SystemState = SystemState
  { counter :: Int
  , cache :: Cache
  , ruleBook :: RuleBook
  , facts :: Facts
  }
  deriving (Show, Eq, Ord)


insertRule :: RuleTuple -> RuleBook -> RuleBook
insertRule (priority, rule) (RB rules) = RB (insertOrdered priority rule rules)

insertOrdered priority rule [] = [(priority, rule)]
insertOrdered p1 r1 rest@((p2, r2) : _) | p1 <= p2 = (p1, r1) : rest
insertOrdered p1 r1 (pair2 : rest) = pair2 : insertOrdered p1 r1 rest

updateProp :: Ref -> Symbol -> Value -> Cache -> Cache
updateProp entity name val (C c) = C (M.adjust (M.insert name val) entity c)

-- SYNTAX --
-- syntax types
--  - patterns
--  - rhs
--  - rules

data Pattern
  -- a, b
  = Seq Pattern Pattern
  -- x
  | PFree Symbol
  -- x <- e
  | PBind Symbol Expr
  -- Pred a b c
  | PPred Predicate
  deriving (Show, Eq, Ord)

data LValue = 
data Action
  = Mutate LValue Value
  | ASeq Action Action
