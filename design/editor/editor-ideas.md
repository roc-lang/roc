# Editor Ideas

(For background, [this talk](https://youtu.be/ZnYa99QoznE?t=4790) has an overview of the design goals for the editor.)

Here are some ideas and interesting resources for the editor. Feel free to make a PR to add more!

## Sources of Potential Inspiration

These are potentially inspirational resources for the editor's design.

Nice collection of research on innovative editors, [link](https://futureofcoding.org/catalog/).

### Package-specific editor integrations

(Or possibly module-specific integrations, type-specific integrations, etc.)

- [What FP can learn from Smalltalk](https://youtu.be/baxtyeFVn3w) by [Aditya Siram](https://github.com/deech)
- [Moldable development](https://youtu.be/Pot9GnHFOVU) by [Tudor Gîrba](https://github.com/girba)
- [Unity game engine](https://unity.com/)
  - Scripts can expose values as text inputs, sliders, checkboxes, etc or even generate custom graphical inputs
  - Drag-n-drop game objects and component into script interfaces
- [How to Visualize Data Structures in VS Code](https://addyosmani.com/blog/visualize-data-structures-vscode/)

### Live Interactivity

- [Up and Down the Ladder of Abstraction](http://worrydream.com/LadderOfAbstraction/) by [Bret Victor](http://worrydream.com/)
- [7 Bret Victor talks](https://www.youtube.com/watch?v=PUv66718DII&list=PLS4RYH2XfpAmswi1WDU6lwwggruEZrlPH)
- [Against the Current](https://youtu.be/WT2CMS0MxJ0) by [Chris Granger](https://github.com/ibdknox/)
- [Sketch-n-Sketch: Interactive SVG Programming with Direct Manipulation](https://youtu.be/YuGVC8VqXz0) by [Ravi Chugh](http://people.cs.uchicago.edu/~rchugh/)
- [Xi](https://xi-editor.io/) modern text editor with concurrent editing (related to [Druid](https://github.com/linebender/druid))
- [Self](https://selflanguage.org/) programming language
- [Primitive](https://primitive.io/) code exploration in Virtual Reality
- [Luna](https://www.luna-lang.org/) language for interactive data processing and visualization
- [Hazel Livelits](https://hazel.org/papers/livelits-paper.pdf) interactive plugins, see GIF's [here](https://twitter.com/disconcision/status/1408155781120376833).
- [Thorough review](https://drossbucket.com/2021/06/30/hacker-news-folk-wisdom-on-visual-programming/) of pros and cons of text versus visual programming.

### Good error messages

- [https://twitter.com/firstdrafthell/status/1427364851593224197/photo/1] very clean error message layout
- [how to write good error message](https://twitter.com/vitalyf/status/1582270207229251585?s=20&t=MorLGshEbEVdRFJ10d2I4A)
- If the user explicitly allows it, we can keep record of which errors take a long time to fix. This way we know where to focus our efforts for improving error messages.

### Debugging

- [VS code debug visualization](https://marketplace.visualstudio.com/items?itemName=hediet.debug-visualizer)
- [Algorithm visualization for javascript](https://algorithm-visualizer.org)
- [godbolt.org Compiler Explorer](https://godbolt.org/)
- [whitebox debug visualization](https://vimeo.com/483795097)
- [Hest](https://ivanish.ca/hest-time-travel/) tool for making highly interactive simulations.
- [replit](https://replit.com/) collaborative browser based IDE.
- [paper](https://openreview.net/pdf?id=SJeqs6EFvB) on finding and fixing bugs automatically.
- [specialized editors that can be embedded in main editor](https://elliot.website/editor/)
- Say you have a failing test that used to work, it would be very valuable to see all code that was changed that was used only by that test.
e.g. you have a test `calculate_sum_test` that only uses the function `add`, when the test fails you should be able to see a diff showing only what changed for the function `add`. It would also be great to have a diff of [expression values](https://homepages.cwi.nl/~storm/livelit/images/bret.png) Bret Victor style. An ambitious project would be to suggest or automatically try fixes based on these diffs.
- I think it could be possible to create a minimal reproduction of a program / block of code / code used by a single test. So for a failing unit test I would expect it to extract imports, the platform, types and functions that are necessary to run only that unit test and put them in a standalone roc project. This would be useful for sharing bugs with library+application authors and colleagues, for profiling or debugging with all "clutter" removed.
- Ability to share program state at a breakpoint with someone else.
- For debugging we should aim for maximal useful observability. For example Rust's enum values can not be easily viewed in the CodeLLDB debugger, you actually need to call a print method that does pattern matching to be able to view useful information.
- We previously discussed recording full traces of programs so they do not have to be re-run multiple times in the debugging process. We should encourage roc developers to experiment with creating debugging representations of this AST+"execution trace", it could lead to some cool stuff.
- We previously mentioned showing expression values next to the code. I think when debugging it would be valuable to focus more on these valuas/data. A possible way to do this would be to create scrollable view(without need to jump between files) of inputs and outputs of user defined functions. Clicking on a function could then show the code with the expression values side by side. Having a good overview of how the values change could make it easy to find where exactly things go wrong.

- (Machine learning) algorithms to extract and show useful information from debug values.
- Ability to mark e.g. a specific record field for tracking(filter out the noise) that is being repeatedly updated throughout the program.
- Ability to collapse/fold debug output coming from specific line.
- search bar to search through printed logs
- Turn an error listed in the console into editable section of code for easy quick fixing.
- Clickable backtrace of functions, user defined functions should be made extra visible.
- VR debugging: render massive curved screen with rectangle showing code (and expression values) for every function in call stack.
- Node and wire diagram of all modules(and functions?) used by a specific test. This will be much more digestible than a node and wire diagram of the whole project.
- Ability to generate project(folder) with code used by test and all else removed. Speedy build times and no distractions.
- After encountering an error with a stacktrace: highlight all lines in code editor that occurred in the stacktrace.
- [Nice visualization of intermediate values](https://twitter.com/ryrobes/status/1582968511713792000?s=20&t=WVj2tP5YwW6_MR5ndU_F4g)
- [Property probes](https://roc.zulipchat.com/#narrow/stream/257722-editor/topic/Property.20probes/near/305671704)

### Testing

- [Wallaby.js](https://wallabyjs.com/) could serve as inspiration for live gutters showing tested / untested / passing / failing code based on tests, combined with time travel debugging (inline runtime values / inline error reports / inline code coverage); could be useful for debugging as well

### Cool regular editors

- [Helix](https://github.com/helix-editor/helix) modal (terminal, for now) editor in rust. Good UX.
- [Kakoune](https://kakoune.org/) editor with advanced text selection and manipulation features.

### Structured Editing

- [Greenfoot](https://www.youtube.com/watch?v=uUVA7nTh0XY)
- [Deuce](http://ravichugh.github.io/sketch-n-sketch/) (videos on the right) by [Ravi Chugh](http://people.cs.uchicago.edu/~rchugh/) and others
- [Fructure: A Structured Editing Engine in Racket](https://youtu.be/CnbVCNIh1NA) by Andrew Blinn
- [Hazel: A Live FP Environment with Typed Holes](https://youtu.be/UkDSL0U9ndQ) by [Cyrus Omar](https://web.eecs.umich.edu/~comar/)
- [Dark Demo](https://youtu.be/QgimI2SnpTQ) by [Ellen Chisa](https://twitter.com/ellenchisa)
- [Introduction to JetBrains MPS](https://youtu.be/JoyzxjgVlQw) by [Kolja Dummann](https://www.youtube.com/channel/UCq_mWDvKdXYJJzBmXkci17w)
- [Eve](http://witheve.com/)
  - code editor as prose writer
  - live preview
  - possible inspiration for live interactivity as well
- [Unreal Engine 4](https://www.unrealengine.com/en-US/)
  - [Blueprints](https://docs.unrealengine.com/en-US/Engine/Blueprints/index.html) visual scripting (not suggesting visual scripting for Roc)

- [Live Programming](https://www.microsoft.com/en-us/research/project/live-programming/?from=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fprojects%2Fliveprogramming%2Ftypography.aspx#!publications) by [Microsoft Research] it contains many interesting research papers.
- [Math Inspector](https://mathinspector.com/), [github](https://github.com/MathInspector/MathInspector)
- [Lamdu](http://www.lamdu.org/) live functional programming.
- [Sourcetrail](https://www.sourcetrail.com/) nice tree-like source explorer.
- [Unisonweb](https://www.unisonweb.org), definition based [editor](https://twitter.com/shojberg/status/1364666092598288385) as opposed to file based.
- [Utopia](https://utopia.app/) integrated design and development environment for React. Design and code update each other, in real time.
- [Paredit](https://calva.io/paredit/) structural clojure editing, navigation and selection. [Another overview](http://danmidwood.com/content/2014/11/21/animated-paredit.html)
- [tylr](https://tylr.fun/) projectional editor UX that helps you make it easier to do edits that are typically difficult with projectional editors but are easy with classic editors.

### Project exploration

- Tree view or circle view (like Github Next) of project where exposed values and functions can be seen on hover.

#### Inspiration

- [Github Next](https://next.github.com/projects/repo-visualization) each file and folder is visualised as a circle: the circle’s color is the type of file, and the circle’s size represents the size of the file. Side note: a cool addition to this might be to use heatmap colors for the circles; circles for files that have had lots of commits could be more red, files with few commits would be blue.
- [AppMap](https://appland.com/docs/appmap-overview.html) records code execution traces, collecting information about how your code works and what it does. Then it presents this information as interactive diagrams that you can search and navigate. In the diagrams, you can see exactly how functions, web services, data stores, security, I/O, and dependent services all work together when application code runs.
- [Discussion on flow based ( nodes and wires) programming](https://marianoguerra.github.io/future-of-coding-weekly/history/weekly/2022/08/W1/thinking-together.html#2022-07-25T00:47:49.408Z) if the wires are a mess, is your program a mess?

### Voice Interaction Related

- We should label as many things as possible and expose jumps to those labels as shortkeys.
- Update without user interaction. e.g. autosave.
- Could be efficient way to communicate with smart assistant.
- You don't have to remember complex keyboard shortcuts if you can describe actions to execute them. Examples:
  - Add latest datetime package to dependencies.
  - Generate unit test for this function.
  - Show edit history for this function.
  - Adjusting settings: switch to light theme, increase font size...
- Use (context specific) voice command state machine to assist Machine Learning voice recognition model.
- Nice special use case: using voice to code while on treadmill desk.
- Use word embeddings to find most similar voice command to recorded input in vector space.

#### Useful voice commands

- clear all breakpoints
- increase/decrease font size
- switch to dark/light/high-contrast mode
- open/go to file "Main"(fuzzy matching)
- go to function "foo"
- go to definition
- show all references(uses) of this function/type/...
- show history timeline of this function/file
- show recent projects
- generate unit test for this function
- generate unit test for this function based on debug trace (input and output is recorded and used in test)
- who wrote this line (git blame integration)
- search documentation of library X for Foo
- show example of how to use library function Foo
- open google/github/duckduckgo search for error...
- show editor plugins for library X
- commands to control log filtering
- collapse all arms of when
- "complex" filtered search:  search for all occurrences of `"#` but ignore all like `"#,`
- color this debug print orange
- remove unused imports

#### Inspiration

- Voice control and eye tracking with [Talon](https://github.com/Gauteab/talon-tree-sitter-service)
- [Seminar about programming by voice](https://www.youtube.com/watch?v=G8B71MbA9u4)
- [Talon voice commands in elm](https://github.com/Gauteab/talon-tree-sitter-service)
- Mozilla DeepSpeech model runs fast, works pretty well for actions but would need additional training for code input.
    Possible to reuse [Mozilla common voice](https://github.com/common-voice/common-voice) for creating more "spoken code" data.
- [Voice Attack](https://voiceattack.com/) voice recognition for apps and games.
- [OpenAI whisper](https://github.com/openai/whisper) excellent open source voice recognition model.

### Beginner-focused Features

- Show Roc cheat sheet on start-up.
- Plugin that translates short pieces of code from another programming language to Roc. [Relevant research](https://www.youtube.com/watch?v=xTzFJIknh7E). Someone who only knows the R language could get started with Roc with less friction if they could quickly define a list R style (`lst <- c(1,2,3)`) and get it translated to Roc.
- Being able to asses or ask the user for the amount of experience they have with Roc would be a valuable feature for recommending plugins, editor tips, recommending tutorials, automated error search (e.g searching common beginner errors first), ... .
- Adjust UI based on beginner/novice/expert?
- Click to explain type annotation

### Productivity features

- When refactoring;
  - Cutting and pasting code to a new file should automatically add imports to the new file and delete them from the old file.
  - Ability to link e.g. variable name in comments to actual variable name. Comment is automatically updated when variable name is changed.
  - When updating dependencies with breaking changes; show similar diffs from github projects that have successfully updated that dependency.
  - AST backed renaming, changing variable/function/type name should change it all over the codebase.
- Automatically create all "arms" when pattern matching after entering `when var is` based on the type.
  - All `when ... is` should be updated if the type is changed, e.g. adding Indigo to the Color type should add an arm everywhere where `when color is` is used.
- When a function is called like `foo(false)`, the name of the boolean argument should be shown automatically; `foo(`*is_active:*`false)`. This should be done for booleans and numbers.
- Suggest automatically creating a function if the compiler says it does not exist.
- Integrated search:
  - Searchbar for examples/docs. With permission search strings could be shared with the platform/package authors so they know exactly what their users are struggling with.
- Show productivity/feature tips on startup. Show link to page with all tips. Allow not seeing tips next time.
- Search friendly editor docs inside the editor. Offer to send search string to Roc maintainers when no results, or if no results were clicked.
- File history timeline view. Show timeline with commits that changed this file, the number of lines added and deleted as well as which user made the changes. Arrow navigation should allow you to quickly view different versions of the file.
- Suggested quick fixes should be directly visible and clickable. Not like in vs code where you put the caret on an error until a lightbulb appears in the margin which you have to click for the fixes to appear, after which you click to apply the fix you want :( . You should be able to apply suggestions in rapid succession. e.g. if you copy some roc code from the internet you should be able to apply 5 import suggestions quickly.
- Regex-like find and substitution based on plain english description and example (replacement). i.e. replace all `[` between double quotes with `{`. [Inspiration](https://alexmoltzau.medium.com/english-to-regex-thanks-to-gpt-3-13f03b68236e).
- Show productivity tips based on behavior. i.e. if the user is scrolling through the error bar and clicking on the next error several times, show a tip with "go to next error" shortcut.
- Command to "benchmark this function" or "benchmark this test" with flamegraph and execution time per line.
- Instead of going to definition and having to navigate back and forth between files, show an editable view inside the current file. See [this video](https://www.youtube.com/watch?v=EenznqbW5w8)
- When encountering an unexpected error in the user's program we show a button at the bottom to start an automated search on this error. The search would:
  - look for similar errors in github issues of the relevant libraries
  - search stackoverflow questions
  - search a local history of previously encountered errors and fixes
  - search through a database of our Zulip questions
  - ...
- smart insert: press a shortcut and enter a plain english description of a code snippet you need. Examples: "convert string to list of chars", "sort list of records by field foo descending", "plot this list with date on x-axis"...
- After the user has refactored code to be simpler, try finding other places in the code base where the same simplification can be made.
- Show most commonly changed settings on first run so new users can quickly customize their experience. Keeping record of changed settings should be opt-in.
- Detection of multiple people within same company/team working on same code at the same time (opt-in).
- Autocorrect likely typos for stuff like `-<` when not in string.
- If multiple functions are available for import, use function were types would match in insertion position.
- Recommend imports based on imports in other files in same project.
- Machine Learning model to determine confidence in a possible auto import. Automatically add the import if confidence is very high.
- Ability to print logs in different color depending on which file they come from.
- Clicking on a log print should take you to the exact line of code that called the log function
- When detecting that the user is repeating a transformation such as replacing a string in a text manually, offer to do the replacement for all occurrences in this string/function/file/workspace.
- Auto remove unused imports? Perhaps save the removed imports on a scratchpad for easy re-enabling.
- It should be easy to toggle search and replace to apply to the whole project.
- Taking into account the eye position with eye tracking could make commands very powerful/accurate. e.g.: make `Num *` a `List (Num *)`, use eye position to determine which `Num *`.
- Feature to automatically minimize visibility(exposing values/functions/...) based on usage in tests. Suggested changes can be shown to the user for fine-grained control.
- Locally record file/function navigation behavior to offer suggestions where to navigate next. With user permission, this navigation behavior can be shared with their team so that e.g. new members get offered useful suggestions on navigating to the next relevant file.
- Intelligent search: "search this folder for <term>", "search all tests for <term>"
- Show some kind of warning if path str in code does not exist locally.
- repl on panic/error: ability to inspect all values and try executing some things at the location of the error.
- show values in memory on panic/error
- automatic clustering of (text) search results in groups by similarity
- fill screen with little windows of clustered search results
- clustering of examples similar to current code
- ability to easily screenshot a subwindow -> create static duplicate of subwindow
- Show references is a common editor feature, often I only want to see non-recursive references in the case of a recursive function.
- ability to add error you were stuck on but have now solved to error database, to help others in the future.
- For quick navigation and good overview: whole file should be shown as folded tree showing only top level defs. Hovering with mouse should allow you to show and traverse the branches, with a click to keep this view. See also ginkowriter.
- clicking on any output should take you to the place in the code where that output was printed and/or calculated.
- ability to edit printed output in such a way that the appropriate changes are made in the code that produced it. Example: edit json key in output-> code is changed to print this new key.
- Idea for the free/legacy editing mode: ability to turn any node into a text buffer node as opposed to the whole file. If we can parse the updated text, it will be converted back into an AST node.
- Similar to predicting next file that will be accessed; put recently accessed folders/files (that are not in a tab) in a subwindow of tree file viewer.
- Automatically keep private local database of terminal output(errors that happened) and actions that were required to solve them. So we can show this to the user if this error pops up again.
- Similarly; errors file in special dir where people can add errors and some text. The editor will check this dir if errors pop up so that this text can be shown. That way developers know what to do when they see an error that someone else has seen before. The text could be something like: "you need to change this setting to prevent this error".
- When user is implementing something that is available in the stdlib; show a notification with the relevant stdlib function.
- Custom commands/aliases for a specific project. For example for navigation; e.g. go to "cli tests"(alias) which is defined to go to `crates/cli/tests/somefile.roc`
- Tool that changes code with duplications to use single source of truth. The reverse operation would also be nice, when changes need to be made for a single case.
- Ability to search all values of expressions of a run. Search would take you to the line of code that produced the value.
- Ability to link to other comments to prevent from having to repeat identical comments or having to update all of them when a change is necessary.
- In the file explorer, auto-close subtree after unused for certain time.
- Ability to right click error message > "create github issue".
- Record (locally) all steps performed by editor + logs and make them searchable for user. The user should be able to scrub through these steps.
- clustering of search results (rg like) based on statistics of result; amount of spaces, avg word length, amount of numbers...
- Search engine to answer questions about your code base, logs, all intermediary values. Examples: search for "xy" in a string, search for xy in a variable name, show all intermediary values of index variable...
- When a stacktrace is shown in the logs, highlight all lines of code in the editor that are in the stacktrace.
- Double-click to select all child expressions of parent: [example](https://twitter.com/disconcision/status/1587156531203678208?s=20&t=GySrwPVnMB6rDKFqRuUcBw).
- We should encourage users to ask editor something in the command window; e.g. "hide all types", "List docs", "switch light mode"... If the questions can not be matched to existing actions, the user should be given the option to share the question with editor developers so the action can be created or mapped to an existing action.

#### Autocomplete

- Use more space for autocomplete options:
  - Multiple columns. Columns could have different sources, i.e. middle column suggests based on current folder, left column on whole project, right column on github.
  - show cell with completion + auto import suggestion
- Webcam based eye tracking for quick selection.
- Machine Learning:
  - GPT-3 can generate correct python functions based on a comment describing the functionality, video [here](https://www.youtube.com/watch?v=utuz7wBGjKM). It's possible that training a model using ast's may lead to better results than text based models.
- Current autocomplete lacks flow, moving through suggestions with arrows is slow. Being able to code by weaving together autocomplete suggestions laid out in rows using eye tracking, that could flow.
- It's possible that with strong static types, pure functions and a good search algorithm we can develop a more reliable autocomplete than one with machine learning.
- When ranking autocomplete suggestions, take into account how new a function is. Newly created functions are likely to be used soon.
- Ability to autocomplete file paths based on folders inside project workspace.
- Allow custom message to be shown when function is autocompleted; e.g. when using sinRadians: "sinTurns is better for high performance."

#### Productivity Inspiration

- [Kite](https://www.kite.com/) AI autocomplete and doc viewer.
- [Tabnine](https://www.tabnine.com/) AI autocomplete.
- [Codota](https://www.codota.com) AI autocomplete and example searching.
- [Github copilot](https://copilot.github.com/) AI autocomplete.
- [Aroma](https://ai.facebook.com/blog/aroma-ml-for-code-recommendation) showing examples similar to current code.
- [MISM](https://arxiv.org/abs/2006.05265) neural network based code similarity scoring.
- [Inquisitive code editor](https://web.archive.org/web/20221206102415/https://austinhenley.com/blog/inquisitivecodeeditor.html) Interactive bug detection with doc+test generation.
- [NextJournal](https://nextjournal.com/blog/command-bar) Discoverable commands and shortcuts.
- [Code Ribbon](https://web.archive.org/web/20230209062246/https://austinhenley.com/blog/coderibbon.html) fast navigation between files. Feature suggestion: top and down are filled with suggested files, whereas left and right are manually filled.
- [Automatic data transformation based on examples](https://youtu.be/Ej91F1fpmEw). Feature suggestion: use in combination with voice commands: e.g. "only keep time from list of datetimes".
- [Codesee](https://www.codesee.io/) code base visualization.
- [Loopy](https://dl.acm.org/doi/10.1145/3485530?sid=SCITRUS) interactive program synthesis.
- [bracket guides](https://mobile.twitter.com/elyktrix/status/1461380028609048576)
- [Frugel](https://github.com/cdfa/frugel) error-tolerant live programming
- [Barliman](https://www.youtube.com/watch?v=er_lLvkklsk) a smart editor capable of program synthesis: given part of a program and a set of tests to pass, Barliman attempts to complete the program for you.
- [UI fuzzer](https://www.fuzzmap.io/?welcome=1) fuzzes UI actions and builds a beautiful state machine like visual.
- [flowistry](https://github.com/willcrichton/flowistry) show relevant code based on current selection and fade out everything else.

### Non-Code Related Inspiration

- [Scrivner](https://www.literatureandlatte.com/scrivener/overview) writing app for novelists, screenwriters, and more
- Word processors (Word, Google Docs, etc)
  - Comments that are parallel to the text of the document.
  - Comments can act as discussions and not just statements.
  - Easy tooling around adding tables and other stylised text
- Excel and Google Sheets
  - Not sure, maybe something they do well that we (code editors) could learn from

## Machine Learning Ideas

- Ability to record all changes to abstract syntax tree with user permission.
  - I think it is possible to create powerful automatic error resolution by having a dataset available of ast's with a specific error and the subsequent transformation that fixed the error.
  - Users with large private code bases could (re)train a publicly available error recovery model to experience benefits without having to share their code.
  - It could be useful to a user who is creating a function to show them the most similar function (type signature, name, comment) in a public+their private database. Say I was using a web framework and I just created a function that has a multipart form as argument, it would be great to have an example instantly available.
    - A simpler start for this idea without user data gathering: how the user a code snippet that is most similar to what they are currently writing. Snippets can be aggregated from examples, tests, docstrings at zero cost to the package/platform authors.
    - Train ML model to rank snippets by likely usefulness to user.
    - See [codata](https://www.codota.com/code/java/classes/okhttp3.OkHttpClient) for inspiration on a snippet/example finder.
- Fuzzy natural language based setting adjustment in search bar or with voice input: increase font size, enable autosave, switch to light theme...
- Detect deviation of best practices, example case: alert developer when they are defining a color inline (rgb(30,30,30)) while all colors have been previously imported from a single file. See also [Codota](https://www.codota.com).
- It would be valuable to record the user's interactions with the editor when debugging as well as the AST. On enough data we could train a model to perform a bunch of debugging steps and show values of the most important variables in relation to the bug. Having assistance in finding the exact code that causes the problem could be super valuable. There could be sensitive data, so it should only be recorded and or shared for open source codebases with permissive licenses and with explicit user permission.
- To allow for more privacy; data gathering can be kept only local or only shared within a team/company. Say we offer the ability to save the changes made after an error occurred. Another developer in the company who encounters this error could be notified someone has previously encountered this error along with their changes made after the error. Optionally, the first developer's name can be shown (only within team/company) so the second developer can quickly ask for help.
- Smart assistant that attempts to "place debug prints"/"show relevant info" based on the error message.
- To get training data for machine learning model: ability to enter a text description for every code edit.
- Chatbot that can answer questions about the code base.
- Smart navigation assistant to help you navigate with fuzzy text: take me to the false-interpreter's platform.
- select some code (x='a'\ny='b') > Open transform command window > Type: concat chars > AI adds line: `concatenated = Char.concat 'a' 'b'`
- Use terminal output to predict suggestions for files to navigate to.

## Testing

- From Google Docs' comments, adding tests in a similar manner, where they exists in the same "document" but parallel to the code being written
  - Makes sense for unit tests, keeps the test close to the source
  - Doesn't necessarily make sense for integration or e2e testing
  - Maybe easier to manually trigger a test related to exactly what code you're writing
- Ability to generate unit tests for a selected function in context menu
  - A table should appear to enter input and expected output pairs quickly
- Ability to "record" unit tests
  - Select a function to record.
  - Do a normal run, and save the input and output of the selected function.
  - Generate a unit test with that input-output pair
- [vitest](https://twitter.com/antfu7/status/1468233216939245579) only run tests that could possibly have changed (because the code they test/use has changed)
- Ability to show in sidebar if code is tested by a test. Clicking on the test in the sidebar should bring you to that test.

### Inspiration

- [Haskell language server plugin](https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin/README.md) evaluate code in comments, to test and document functions and to quickly evaluate small expressions.
- [Hazel live test](https://mobile.twitter.com/disconcision/status/1459933500656730112)

## Documentation

- Ability to see module as it would be presented on a package website.
  - Modern editors may guide developers to the source code too easily.
    The API and documentation are meant to interface with humans.
- [DocC](https://developer.apple.com/videos/play/wwdc2021/10166/) neat documentation approach for swift.
- Make it easy to ask for/add examples and suggest improvements to a project's docs.
- Library should have cheat sheet with most used/important docs summarized.
- With explicit user permission, anonymously track viewing statistics for documentation. Can be used to show most important documentation, report pain points to library authors.
- Easy side-by-side docs for multiple versions of library.
- ability to add questions and answers to library documentation

## Tutorials

- Inclusion of step-by-step tutorials in Roc libraries, platforms or business specific code.
- Having to set up your own website for a tutorial can be a lot of work, making it easy to make quality tutorials would make for a more delightful experience.

## High performance

### Inspiration

- [10x editor](https://www.10xeditor.com/) IDE/Editor targeted at the professional developer with an emphasis on performance and scalability.

## Positive feedback

- It's nice to enhance the feeling of reward after completing a task, this increases motivation.
- Great for tutorials and the first run of the editor.
- Suggestions of occasions for positive feedback:
  - Being able to compile successfully after starting out with more than X errors.
  - Making a test succeed after repeated failures.
- Positive feedback could be delivered with messages and/or animations. Animations could be with fireworks, flying roc logo birds, sounds...
- The intensity of the message/animation could be increased based on the duration/difficulty of the task.
- Suggest to search for help or take a break after being stuck on a test/compile errors... for some time. A search could be done for group chats for relevant libraries.

### Inspiration

- [Duolingo](https://www.duolingo.com) app to learn languages
- [Khan academy](https://www.khanacademy.org/) free quality education for everyone

## Accessibility

  - Visual Impairments
      No Animation is most benign form of cognitive disability but really important base line of people with tense nerve system.
      Insensitivity to certain or all colors.
      Need of high contrast
      Or Everything Magnified for me with no glasses.
      Or Total blindness where we need to trough sound to communicate to the user
      Screen readers read trees of labeled elements. Each OS has different APIs, but I think they are horrible. Just close your eyes and imagine listening to screen reader all day while you are using these majestic machines called computers.
      But blind people walk with a tool and they can react much better to sound/space relations than full on visual majority does. They are acute to sound as a spatial hint. And a hand for most of them is a very sensitive tool that can make sounds in space.
      Imagine if every time the user doesn't want to rely on shining rendered pixels on the screen for a feedback from machine, we make a acoustic room simulation, where with moving the "stick", either with mouse or with key arrows, we bump into one of the objects and that produces certain contextually appropriate sound (clean)*ding*

      On the each level of abstraction they can make sounds more deeper, so then when you type letters you feel like you are playing with the sand (soft)*shh*. We would need help from some sound engineer about it, but imagine moving down, which can be voice triggered command for motion impaired, you hear (soft)*pup* and the name of the module, and then you have options and commands appropriate for the module, they could map to those  basic 4 buttons that we trained user on, and he would shortcut all the soft talk with click of a button. Think of the satisfaction when you can skip the dialog of the game and get straight into action. (X) Open functions! Each function would make a sound and say its name, unless you press search and start searching for a specific function inside module, if you want one you select or move to next.
    - Related idea: Playing sounds in rapid succession for different expressions in your program might be a high throughput alternative to stepping through your code line by line. I'd bet you quickly learn what your program should sound like. The difference in throughput would be even larger for those who need to rely on voice transcription.

  - Motor impairments
      [rant]BACKS OF CODERS ARE NOT HEALTHY! We need to change that![/neverstop]
      Too much mouse waving and sitting for too long is bad for humans.
      Keyboard is basic accessibility tool but
      Keyboard is also optional, some people have too shaky hands even for keyboard.
      They rely on eye tracking to move mouse cursor around.
      If we employ *some* voice recognition functions we could make same interface as we could do for consoles where 4+2 buttons and directional pad would suffice.
      That is 10 phrases that need to be pulled trough as many possible translations so people don't have to pretend that they are from Maine or Texas so they get voice recognition to work. Believe me I was there with Apple's Siri :D That is why we have 10 phrases for movement and management and most basic syntax.
  - Builtin fonts that can be read more easily by those with dyslexia.
  - [Cross-platform UI accessibility WIP](UI accessibility infrastructure across platforms and programming languages https://github.com/AccessKit/accesskit)
  - beware of eye strain for eye tracking features

## UX testing

- knowledge gap exercise: walk through the process of someone using your product for the first time, and imagine that you're sat next to them as they do it.
What do you feel the need to say to them? Which knowledge gaps are you filling with your voice? Most of these will feel intuitive—write them down.

## Builtin docutorial

- interactive code examples
- [Notebook](https://jupyter.org/try-jupyter/retro/notebooks/?path=notebooks/Intro.ipynb) like format? Could support images, svg, sound, math... .
- Should support those new to programming and experienced individuals. Probably need custom version for new programmers (need to go deep into simple things).
- Support ability to explore both broadly (many topics) and deeply (extensive detail).
- Write three versions of every "page" for "never programmed before", novice and pro?
- Should be completely navigable with the keyboard:
  - Down arrow to go deeper into topic. It may start with a simple overview and examples; it then expands with further detailed information such as links to reference material, explanations, examples, and quizzes.
  - Right arrow to go to next topic.
- Sort topics in order of importance to know.
- Docutorial should educate users about roc and about using the editor.
- At least some of this content should be bundled in the nightly so that it easily works offline.

## General Thoughts/Ideas

- Nice backtraces that highlight important information
- Ability to show import connection within project visually
  - This could be done by drawing connections between files or functions in the tree view. This would make it easier for people to get their bearings in new big projects.
- Connections could also be drawn between functions that call each other in the tree view. The connections could be animated to show the execution flow of the program.
- Ability to inline statements contained in called functions into the callee function for debugging.
  - The value of expressions can be shown at the end of the line like in the [Inventing on Principle talk](https://youtu.be/8QiPFmIMxFc?t=1181)
  - This would give a clear overview of the execution and should make it easy to pinpoint the line where the bug originates.
  - That specific line can then be right clicked to go to the actual function.
  - Having to jump around between different functions and files is unnecessary and makes it difficult to see the forest through the trees.
- "Error mode" where the editor jumps you to the next error
  - Similar in theory to diff tools that jump you to the next merge conflict
- dependency recommendation
- Command to change the file to put all exposed functions at the top of the file, private functions below. Other alternative; ability to show a "file explorer" that shows exposed functions first, followed by private functions.
- We could provide a more expansive explanation in errors that can benefit from it. This explanation could be folded(shown on click) by default in the editor.
- Code coverage visualization: allow to display code in different color when it is covered by test.
- Make "maximal privacy version" of editor available for download, next to regular version. This version would not be capable of sharing any usage/user data.
- Live code view with wasm editor. This saves bandwidth when pairing.
- [Gingkowriter](https://gingkowriter.com/) structured writing app.
- Performance improvement recommendation: show if code is eligible for tail call optimization or can do in place mutation.
- very small error squiggles should be made more visible
