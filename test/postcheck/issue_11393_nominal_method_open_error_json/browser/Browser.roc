Browser :: [].{
    Page := { guid : Str }

    open : {} -> Page
    open = |{}| Browser.Page.{ guid: "page" }

    navigate : Page, Str -> Try(Str, [NavigateError(Str), ..e])
    navigate = |_page, url| Ok(Json.to_str(url))
}
