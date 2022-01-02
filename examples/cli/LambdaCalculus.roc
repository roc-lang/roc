app "lambda"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout ]
    provides [ main ] to pf

Expr :
    [
        Var Str,
        Lambda { name : Str, body : Expr },
        Application { operator : Expr, operand : Expr },
    ]

Env : Dict Str Value

Value :
    [
        Number Nat,
        String Str,
        Closure { name : Str, body : Expr, env : Dict Str Value },
        Error Str,
    ]

eval : Expr, Env -> Value
eval = \expr, env ->
    when expr is
        Var name ->
            when Dict.get env name is
                Ok value ->
                    value

                Err _ ->
                    Error "\(name) is not defined"

        Lambda { name, body } ->
            Closure { name, body, env }

        Application { operator, operand } ->
            call (eval operator env) (eval operand env)

call : Value, Value -> Value
call = \func, argument ->
    when func is
        Closure { name, body, env } ->
            eval body (Dict.insert env name argument)

        _ ->
            Error "Only functions can be called"

main : Task {} *
main =
    x = Var "x"

    identity = Lambda { name: "x", body: x }

    y = Var "y"

    application = Application { operator: identity, operand: y }

    env : Env
    env =
        Dict.empty
            |> Dict.insert "y" (String "Hello Lambda Calculus")

    result =
        when eval application env is
            Number number ->
                Num.toStr number

            String str ->
                str

            Error msg ->
                "Error: \(msg)"

            Closure closure ->
                "Unexpected Closure: \(closure.name)"

    Stdout.line result
