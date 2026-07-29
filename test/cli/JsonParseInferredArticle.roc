JsonParseInferredArticle :: [].{}

valid_json = "{\"title\":\"Pattern Matching\",\"author\":\"Mira\",\"views\":1420,\"tags\":[\"roc\",\"json\"]}"

missing_author_json = "{\"title\":\"Draft Notes\",\"views\":7,\"tags\":[\"demo\"]}"

broken_json = "{\"title\":\"Oops\",\"author\":\"Mira\",\"views\":nope,\"tags\":[]}"

summarize = |json| {
	match Json.parse(json) {
		Ok(article) => {
			tag_text = Str.join_with(article.tags, ", ")
			byline = Str.concat(article.title, Str.concat(" by ", article.author))
			"${byline}: ${U64.to_str(article.views)} views, tags: ${tag_text}"
		}

		Err(MissingRequiredField(field)) =>
			"missing required field: ${field}"

		Err(InvalidJson(message)) =>
			"invalid JSON: ${message}"
		}
}

expect {
	summarize(valid_json) == "Pattern Matching by Mira: 1420 views, tags: roc, json"
		and summarize(missing_author_json) == "missing required field: author"
			and summarize(broken_json) == "invalid JSON: Invalid JSON"
}
