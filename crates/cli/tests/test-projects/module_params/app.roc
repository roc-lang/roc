app [main] {
    pf: platform "../test-platform-simple-zig/main.roc",
}

import Api { app_id: "one", protocol: https } as App1
import Api { app_id: "two", protocol: http } as App2
import Api { app_id: "prod_1", protocol: http } as Prod

https = \url -> "https://${url}"
http = \url -> "http://${url}"

users_app1 =
    # pass top-level fn in a module with params
    List.map([1, 2, 3], App1.get_user)

main =
    app3_id = "three"

    import Api { app_id: app3_id, protocol: https } as App3

    get_user_app3_nested = \user_id ->
        # use captured params def
        App3.get_user(user_id)

    users_app3_passed =
        # pass top-level fn in a nested def
        List.map([1, 2, 3], App3.get_user)

    """
    App1.baseUrl: ${App1.base_url}
    App2.baseUrl: ${App2.base_url}
    App3.baseUrl: ${App3.base_url}
    App1.getUser 1: ${App1.get_user(1)}
    App2.getUser 2: ${App2.get_user(2)}
    App3.getUser 3: ${App3.get_user(3)}
    App1.getPost 1: ${App1.get_post(1)}
    App2.getPost 2: ${App2.get_post(2)}
    App3.getPost 3: ${App3.get_post(3)}
    App1.getPosts [1, 2]: ${Inspect.to_str(App1.get_posts([1, 2]))}
    App2.getPosts [3, 4]: ${Inspect.to_str(App2.get_posts([3, 4]))}
    App2.getPosts [5, 6]: ${Inspect.to_str(App2.get_posts([5, 6]))}
    App1.getPostComments 1: ${App1.get_post_comments(1)}
    App2.getPostComments 2: ${App2.get_post_comments(2)}
    App2.getPostComments 3: ${App2.get_post_comments(3)}
    App1.getCompanies [1, 2]: ${Inspect.to_str(App1.get_companies([1, 2]))}
    App2.getCompanies [3, 4]: ${Inspect.to_str(App2.get_companies([3, 4]))}
    App2.getCompanies [5, 6]: ${Inspect.to_str(App2.get_companies([5, 6]))}
    App1.getPostAliased 1: ${App1.get_post_aliased(1)}
    App2.getPostAliased 2: ${App2.get_post_aliased(2)}
    App3.getPostAliased 3: ${App3.get_post_aliased(3)}
    App1.baseUrlAliased: ${App1.base_url_aliased}
    App2.baseUrlAliased: ${App2.base_url_aliased}
    App3.baseUrlAliased: ${App3.base_url_aliased}
    App1.getUserSafe 1: ${App1.get_user_safe(1)}
    Prod.getUserSafe 2: ${Prod.get_user_safe(2)}
    usersApp1: ${Inspect.to_str(users_app1)}
    getUserApp3Nested 3: ${get_user_app3_nested(3)}
    usersApp3Passed: ${Inspect.to_str(users_app3_passed)}
    """
