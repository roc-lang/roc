platform ""
	requires {
		main! : () => {}
	}
	exposes [KeywordTags]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted {
		"roc_keyword_tags_send": KeywordTags.send!,
		"roc_keyword_tags_resolve": KeywordTags.resolve!,
		"roc_keyword_tags_payload": KeywordTags.payload!,
	}
	targets: {}

import KeywordTags

main_for_host! : () => {}
main_for_host! = main!
