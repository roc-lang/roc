# Regression for https://github.com/roc-lang/roc/issues/10311
app [main!] { pf: platform "../fx-open/platform/main.roc" }

Provider := {}.{
    run! : Provider => Try({}, [Failed])
    run! = |_| Ok({})

    is_failed : Provider, [Failed] -> Bool
    is_failed = |_, err| err == Failed
}

succeeds! : provider => Try(Bool, err)
    where [
        provider.run! : provider => Try({}, err),
        provider.is_failed : provider, err -> Bool,
    ]
succeeds! = |provider| {
    ProviderType : provider

    match ProviderType.run!(provider) {
        Ok({}) => Ok(True)
        Err(err) if ProviderType.is_failed(provider, err) => Ok(False)
        Err(err) => Err(err)
    }
}

main! = |_args| {
    _ = succeeds!(Provider.{})
    Ok({})
}
