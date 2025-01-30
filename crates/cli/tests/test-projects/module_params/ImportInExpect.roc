module []

https = \url -> "https://${url}"

expect
    import Api { app_id: "one", protocol: https }
    Api.base_url == "https://api.example.com/one"

expect
    import Api { app_id: "two", protocol: https }
    Api.get_user(1) == "https://api.example.com/two/users/1"

expect
    import NoParams
    NoParams.hello == "hello!"
