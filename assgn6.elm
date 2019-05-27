-- TO DO: test cases, interp

-- use this to print output after compilation
import Html exposing (text)

main =
  text "hello world"


-- type definitions
-- NOTE: on 'custom types' tutorial page, there are two approaches:
-- streamlined type defs (as seen below) or more verbose 'record' based style.
-- downside of streamlined version is you must use case statements to access fields.
type ExprC
  = NumC Float
  | IdC String
  | StringC String
  | IfC ExprC ExprC ExprC
  | LamC (List String) ExprC
  | AppC ExprC (List ExprC)

-- holding off on CloV for now
type Value
  = NumV Float
  | BoolV Bool
  | StringV String
  | PrimV (Value -> Value -> Result String Value)


-- primitive operations
valAdd : Value -> Value -> Result String Value
valAdd l r =
  case (l, r) of
    ((NumV lval), (NumV rval)) ->
      Ok (NumV (lval + rval))
    _ ->
      Err "operand not a number"

valSub : Value -> Value -> Result String Value
valSub l r =
  case (l, r) of
    ((NumV lval), (NumV rval)) ->
      Ok (NumV (lval - rval))
    _ ->
      Err "operand not a number"

valMult : Value -> Value -> Result String Value
valMult l r =
  case (l, r) of
    ((NumV lval), (NumV rval)) ->
      Ok (NumV (lval * rval))
    _ ->
      Err "operand not a number"

valDiv : Value -> Value -> Result String Value
valDiv l r =
  case (l, r) of
    ((NumV lval), (NumV rval)) ->
      if rval == 0
      then Err "divide by zero" 
      else Ok (NumV (lval - rval))
    _ ->
      Err "operand not a number"

valLeq : Value -> Value -> Result String Value
valLeq l r =
  case (l, r) of
    ((NumV lval), (NumV rval)) ->
      Ok (BoolV (lval <= rval))
    _ ->
      Err "operand not a number"

valEqual : Value -> Value -> Result String Value
valEqual l r =
  case (l, r) of
    ((NumV lval), (NumV rval)) ->
      Ok (BoolV (lval == rval))
    ((BoolV lval), (BoolV rval)) ->
      Ok (BoolV (lval == rval))
    ((StringV lval), (StringV rval)) ->
      Ok (BoolV (lval == rval))
    _ ->
      Ok (BoolV False)