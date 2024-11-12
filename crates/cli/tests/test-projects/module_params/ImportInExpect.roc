module []

https = \url -> "https://$(url)"

expect
    import Api { appId: "one", protocol: https }
    Api.baseUrl == "https://api.example.com/one"

expect
    import Api { appId: "two", protocol: https }
    Api.getUser 1 == "https://api.example.com/two/users/1"

expect
    import NoParams
    NoParams.hello == "hello!"
