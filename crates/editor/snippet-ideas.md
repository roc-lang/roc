# Snippet ideas

I think snippet insertion would make for an awesome demo that shows off the potential of the editor and a basic version would not be that difficult to implement.
With snippet insertion I mean the following:

say you have a list of Person records, people in scope
you press some keyboard shortcut
a text field pops up
you enter "sort"
we show autocomplete options like sort people by firstName, sort people by lastName and sort people by age. The recommendations can be that good because we know people is the only list in scope and we know which fields are in the Person record.
you navigate to sort people by age, press enter and we show in the autocomplete popup: sort people by age descending and sort people by age ascending.
you navigate to sort people by age ascending and press Enter
The correct Roc code is inserted
This is most useful for beginning Roc programmers, but I could see it saving time for experts as well.
Novice to expert programmers who are new to Roc can also perfectly describe what they want to happen but may not know the correct syntax, names of builtin functions...
Other useful snippet commands for beginning Roc programmers might be empty dict, lambda function, split strVal into chars...
Some more advanced snippets: post jsonVal to urlVal, connect to amazon S3, insert function of common algorithm like: sieve of erathostenes, greatest common divider...

This could remove the need for a lot of googling/stackoverflow, creating a delightful experience that sets us apart from other editors.
And contrary to stackoverflow/github copilot, snippets will be written by Roc experts or be easily editable by us. They can also be guaranteed to work for a specific Roc and library version because we update, version, and test them.

A nice goal to aim for is that the user never needs/wants to leave the editor to look things up.
We have way more context inside the editor so we should be able to do better than any general-purpose search engine.

I think the snippet insertion commands also set us up for quality interaction with users using voice input.

The CC0 license seems like a good fit for the snippets.

Fuzzy matching should be done to suggest a closest fuzzy match, so if the user types the snippet command `empty Map`, we should suggest `empty Dict`.

## Pure Text Snippets

Pure text snippets are not templates and do not contain typed holes.
Fish hooks are used when subvariants should be created e.g.: <collection> means this pure text snippets should be created for all Roc collections such as Dict, Set, List...

- command: empty <collection>
  - example: empty dict >> `{::}`
- command: <common algorithm>
  - example: sieve of erathostenes >> `inserts function for sieve of erathostenes`
  - common algorithms: sieve of erathostenes, greatest common divisor, prime factorisation, A* path finding, Dijkstra's algorithm, Breadth First Search...
- command: current date/datetime
  - example: current datetime >> `now <- Time.now\n`
- command: list range 1 to 5
  - example: [1, 2, 3, 4, 5]
- command: use commandline args
- command: post/get/put request
- command: extract float(s)/number/emal addresses  from string. regex match float/number/email address/...
- command: execute (bash) command/script
- command: cast/convert/parse list of x to list of y
- command: pattern match/ match/ switch/ case

## AST aware snippets

Snippets are inserted based on type of value on which the cursor is located.

- command: <all builtins for current type>
  - example:
    - We have the cursor like this `people|`
    - User presses snippet shortcut or dot key
    - We show  a list with all builtin functions for the List type
    - User chooses contains
    - We change code to `List.contains people |Blank`
- command: Str to chars/charlist

## Snippets with Typed Holes

- command: sort ^List *^ (by ^Record Field^) {ascending/descending}
  - example: sort people by age descending >> ...
- command: escape url
  - example: >> `percEncodedString = Url.percentEncode ^String^`
- command: list files in directory
  - example: >>

        ```
        path <- File.pathFromStr ^String^
        dirContents <- File.enumerateDir path
        ```

- command: remove/create file
- command: read/write from file
- command: concatenate strings
- command: trim (newlines) at end/start/right/left
- command: evaluate predicate for all in slice/list/array
- command: get element at index
- command: get char at index
- command: reverse stirng
- command: lambda/anonymous function
- we should auto create type hole commands for all builtins.
  - example: List has builtins reverse, repeat, len... generated snippet commands should be:
    - reverse list > List.reverse ^List *^
    - repeat list > List.repeat ^elem^ ^Nat^
    - len list (fuzzy matches should be length of list)
- append element to list

## fuzzy matching

 some pairs for fuzzy matching unit tests:

- hashmap > Dict
- map > map (function), Dict
- for > map, mapWithIndex, walk, walkBackwards, zip
- apply/for yield > map
- fold > walk, walkBackwards
- foldl > walkBackwards
- foldr > walk
- head > takeFirst
- filter > keepIf

## Inspiration

- [grepper](https://www.codegrepper.com/) snippet collection that embeds in google search results. See also this [collection of common questions](https://www.codegrepper.com/code-examples/rust).
- [github copilot](https://copilot.github.com/) snippet generation with machine learning
- [stackoverflow](https://stackoverflow.com)
- [rosetta code](http://www.rosettacode.org/wiki/Rosetta_Code) snippets in many different programming languages. Many [snippets](https://www.rosettacode.org/wiki/Category:Programming_Tasks) are programming contest style problems, but there also problems that demonstrate the use of JSON, SHA-256, read a file line by line...
- check docs of popular languages to cross reference function/snippet names for fuzzy matching
