import Dict exposing (Dict, fromList, get)
import List exposing (head, tail, length)

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
topEnv = fromList [("true", BoolV True),
                   ("false", BoolV False),
                   ("+", PrimV valAdd),
                   ("-", PrimV valSub),
                   ("*", PrimV valMult),
                   ("/", PrimV valDiv),
                   ("<=", PrimV valLeq),
                   ("equal?", PrimV valEqual)]

-- top-level functions

-- interprets an expression
interp : ExprC -> Env -> Result String Value
interp e env =
  case e of
    NumC n -> Ok (NumV n)
    StringC str -> Ok (StringV str)
    IdC id -> lookup id env
    IfC test thn els ->
      case interp test env of
        Ok (BoolV bool) ->
          if bool
          then interp thn env
          else interp els env
        _ -> Err "test clause not boolean"
    LamC params body -> Ok (CloV params body env)
    AppC fun args ->
      case interp fun env of
        Ok (PrimV op) ->
          case args of 
            [ fst, snd ] ->
              case ((interp fst env), (interp snd env)) of
                (Ok left, Ok right) ->
                  op left right
                _ -> Err "error occurred"
            _ -> Err "primitive operation takes two arguments"      
        _ -> Err "improper application"


-- helper functions

-- returns id from environment
lookup : String -> Env -> Result String Value
lookup id env =
  case get id env of
    Just val ->
      Ok val
    Nothing -> 
      Err "unbound identifier"


-- primitive operations

-- adds two numbers
valAdd : Value -> Value -> Result String Value
valAdd l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (NumV (lval + rval))
    _ ->
      Err "operand not a number"

-- subtracts two numbers
valSub : Value -> Value -> Result String Value
valSub l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (NumV (lval - rval))
    _ ->
      Err "operand not a number"

-- multiplies two numbers
valMult : Value -> Value -> Result String Value
valMult l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (NumV (lval * rval))
    _ ->
      Err "operand not a number"

-- divides two numbers
valDiv : Value -> Value -> Result String Value
valDiv l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      if rval == 0
      then Err "divide by zero" 
      else Ok (NumV (lval / rval))
    _ ->
      Err "operand not a number"

-- compares two numbers
valLeq : Value -> Value -> Result String Value
valLeq l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (BoolV (lval <= rval))
    _ ->
      Err "operand not a number"

-- checks equality of two values
valEqual : Value -> Value -> Result String Value
valEqual l r =
  case (l, r) of
    (NumV lval, NumV rval) ->
      Ok (BoolV (lval == rval))
    (BoolV lval, BoolV rval) ->
      Ok (BoolV (lval == rval))
    (StringV lval, StringV rval) ->
      Ok (BoolV (lval == rval))
    _ ->
      Ok (BoolV False)