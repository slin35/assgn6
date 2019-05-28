module UNIT_TEST exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main exposing (..)

suite : Test
suite =
    describe "tests"
        [
            test "interp NumC" <|
                \() ->
                    Expect.equal (Ok (NumV 5)) (interp (NumC 5) topEnv) 
            ,test "interp StringC" <|
                \() ->
                    Expect.equal (Ok (StringV "apple")) (interp (StringC "apple") topEnv) 
            , test "interp idC" <|
                \() ->
                    Expect.equal (Ok (BoolV True)) (interp (IdC "true") topEnv)
            , test "interp IfC" <|
                \() ->
                    Expect.equal (Ok (NumV 2)) (interp (IfC (IdC "true") (NumC 2) (NumC 1)) topEnv)
            , test "interp IfC2" <|
                \() ->
                    Expect.equal (Err "test clause not boolean") (interp (IfC (IdC "+") (NumC 2) (NumC 1)) topEnv)
            , test "interp LamC" <|
                \() ->
                    Expect.equal (Ok (CloV (["a", "b", "c"]) (NumC 5) topEnv)) (interp (LamC ["a", "b", "c"] (NumC 5)) topEnv)
            , test "interp AppC" <|
                \() ->
                    Expect.equal (Ok (NumV 3)) (interp (AppC (IdC "+") [(NumC 1), (NumC 2)]) topEnv)
  --          , test "interp AppC2" <| 
    --            \() ->
      --              Expect.equal (Ok (NumV 3)) (interp (AppC (LamC ["a", "b"] (AppC (IdC "+") [(IdC "a"), (IdC "b")])) [(NumC 1), (NumC 2)]) topEnv) 
        --    , test "interp AppC3" <| 
          --      \() ->
            --        Expect.equal (Err "arity mismatch") (interp (AppC (LamC ["a", "b"] (AppC (IdC "+") [(IdC "a"), (IdC "b")])) [(NumC 2)]) topEnv)
            , test "interp AppC4" <|
                \() ->
                    Expect.equal (Err "primitive operation takes two arguments") (interp (AppC (IdC "+") [(NumC 1), (NumC 2), (NumC 3)]) topEnv)
            , test "interp AppC5" <|
                \() ->
                    Expect.equal (Err "improper application") (interp (AppC (StringC "hello") [(NumC 1)]) topEnv)

            , test "lookup" <|
                \() ->
                    Expect.equal (Ok (PrimV valAdd)) (lookup "+" topEnv)

            , test "lookup2" <|
                \() ->
                    Expect.equal (Err "unbound identifier") (lookup "f" topEnv)

            ,test "valAdd" <|
                \() -> Expect.equal (Ok (NumV 3)) (valAdd (NumV 1) (NumV 2))
            , test "valAdd2" <|
                \() -> Expect.equal (Err "operand not a number") (valAdd (StringV "x") (NumV 2))

            , test "valSub" <|
                \() -> Expect.equal (Ok (NumV 3)) (valSub (NumV 5) (NumV 2))
            , test "valSub2" <|
                \() -> Expect.equal (Err "operand not a number") (valSub (StringV "x") (NumV 2))

            , test "valMult" <|
                \() -> Expect.equal (Ok (NumV 10)) (valMult (NumV 5) (NumV 2))
            , test "valMult2" <|
                \() -> Expect.equal (Err "operand not a number") (valMult (StringV "x") (NumV 2))

            , test "valDiv" <|
                \() ->
                    Expect.equal (Ok (NumV 5)) (valDiv (NumV 10) (NumV 2))
            , test "valDiv2" <|
                \() -> Expect.equal (Err "divide by zero") (valDiv (NumV 2) (NumV 0))
            , test "valDiv3" <|
                \() -> Expect.equal (Err "operand not a number") (valDiv (StringV "x") (NumV 2))

            , test "valLeq" <|
                \() ->
                    Expect.equal (Ok (BoolV False)) (valLeq (NumV 3) (NumV 2))
            , test "valLeq2" <|
                \() ->
                    Expect.equal (Ok (BoolV True)) (valLeq (NumV 2) (NumV 3))
            , test "valLeq3" <|
                \() -> Expect.equal (Err "operand not a number") (valLeq (StringV "x") (NumV 2))

            , test "valEqual" <|
                \() -> Expect.equal (Ok (BoolV True)) (valEqual (StringV "a") (StringV "a"))
            , test "valEqual2" <|
                \() -> Expect.equal (Ok (BoolV False)) (valEqual (StringV "a") (StringV "b"))
        ]
