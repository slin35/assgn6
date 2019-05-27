import Dict exposing (..)

-- type definitions
type ExprC
  = NumC Float
  | IdC String
  | StringC String
  | IfC ExprC ExprC ExprC
  | LamC (List String) ExprC
  | AppC ExprC (List ExprC)

type Value
  = NumV Float
  | BoolV Bool
  | StringV String
  | PrimV (Value -> Value -> Result String Value)
  | CloV (List String) ExprC Env

type alias Env = Dict String Value

topEnv : Env
topEnv = (fromList [("true", BoolV True),
                    ("false", BoolV False),
                    ("+", PrimV valAdd),
                    ("-", PrimV valSub),
                    ("*", PrimV valMult),
                    ("/", PrimV valDiv),
                    ("<=", PrimV valLeq),
                    ("equal?", PrimV valEqual)])


-- top-level functions

-- interprets an expression
interp : ExprC -> Env -> Result String Value
interp e env =
  case e of
    NumC n -> NumV n
    StringC str -> StringV str
    IdC id -> lookup id env
    IfC testt thenn elsee ->
      case interp testt env of
        BoolV bool ->
          if bool
          then interp thenn env
          else interp elsee env
        _ -> Err "test clause not boolean"
    LamC params body -> CloV params body env
    AppC fun app-args ->
      case (interp fun env) of
        PrimV op ->
          if length app-args == 2
          then op (interp (head app-args) env)
                  (interp (tail app-args) env)
          else Err "primitive operation takes two arguments"
        CloV clo-args body clo-env ->
          if length clo-args == length app-args
          then let
            argvals = map (\arg -> interp arg env) app-args
            let
              newEnv = extendEnv clo-env clo-args argvals
              in interp body newEnv
          else Err "arity mismatch"
        _ -> Err "not supported"


-- helper functions

-- returns id from environment
lookup : String -> Env -> Result String Value
lookup id env =
  case get id env of
    Just val ->
      Ok val
    Nothing -> 
      Err "unbound identifier"


-- takes env and list of string-value pairs and returns new env
-- with inserted/updated pairs
extendEnv : Env -> (List String) -> (List Value) -> Env
extendEnv env strs vals =
  let
    newBindings = map2 (,) strs vals
  in foldl (\(str, val) ->
            insert str val env)
      newBindings


-- primitive operations

-- adds two values
valAdd : Value -> Value -> Result String Value
valAdd l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (NumV lval + rval)
    _ ->
      Err "operand not a number"

-- subtracts two values
valSub : Value -> Value -> Result String Value
valSub l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (NumV lval - rval)
    _ ->
      Err "operand not a number"

-- multiplies two values
valMult : Value -> Value -> Result String Value
valMult l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (NumV lval * rval)
    _ ->
      Err "operand not a number"

-- divides two values
valDiv : Value -> Value -> Result String Value
valDiv l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      if rval == 0
      then Err "divide by zero" 
      else Ok (NumV lval - rval)
    _ ->
      Err "operand not a number"

-- compares two values
valLeq : Value -> Value -> Result String Value
valLeq l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (BoolV lval <= rval)
    _ ->
      Err "operand not a number"

-- checks equality of two values
valEqual : Value -> Value -> Result String Value
valEqual l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (BoolV lval == rval)
    (BoolV lval, BoolV rval) ->
      Ok (BoolV lval == rval)
    (StringV lval, StringV rval) ->
      Ok (BoolV lval == rval)
    _ ->
      Ok (BoolV False)