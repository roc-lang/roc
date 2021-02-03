(For background, [this talk](https://youtu.be/ZnYa99QoznE?t=4790) has an overview of the design goals for the editor.)

# Editor Ideas

Here are some ideas and interesting resources for the editor. Feel free to make a PR to add more!

## Sources of Potential Inspiration

These are potentially inspirational resources for the editor's design.

### Package-specific editor integrations

(Or possibly module-specific integrations, type-specific integrations, etc.)

* [What FP can learn from Smalltalk](https://youtu.be/baxtyeFVn3w) by [Aditya Siram](https://github.com/deech)
* [Moldable development](https://youtu.be/Pot9GnHFOVU) by [Tudor Gîrba](https://github.com/girba)
* [Unity game engine](https://unity.com/)
    * Scripts can expose values as text inputs, sliders, checkboxes, etc or even generate custom graphical inputs
    * Drag-n-drop game objects and component into script interfaces
* [How to Visualize Data Structures in VS Code](https://addyosmani.com/blog/visualize-data-structures-vscode/)

### Live Interactivity

* [Up and Down the Ladder of Abstraction](http://worrydream.com/LadderOfAbstraction/) by [Bret Victor](http://worrydream.com/)
* [7 Bret Victor talks](https://www.youtube.com/watch?v=PUv66718DII&list=PLS4RYH2XfpAmswi1WDU6lwwggruEZrlPH)
* [Against the Current](https://youtu.be/WT2CMS0MxJ0) by [Chris Granger](https://github.com/ibdknox/)
* [Sketch-n-Sketch: Interactive SVG Programming with Direct Manipulation](https://youtu.be/YuGVC8VqXz0) by [Ravi Chugh](http://people.cs.uchicago.edu/~rchugh/)
* [Xi](https://xi-editor.io/) modern text editor with concurrent editing (related to [Druid](https://github.com/linebender/druid))
* [Self](https://selflanguage.org/) programming language
* [Primitive](https://primitive.io/) code exploration in Virtual Reality

### Debugging

* [VS code debug visualization](https://marketplace.visualstudio.com/items?itemName=hediet.debug-visualizer)
* [Algorithm visualization for javascript](https://algorithm-visualizer.org)
* [godbolt.org Compiler Explorer](https://godbolt.org/)

### Structured Editing

* [Greenfoot](https://www.youtube.com/watch?v=uUVA7nTh0XY)
* [Deuce](http://ravichugh.github.io/sketch-n-sketch/) (videos on the right) by [Ravi Chugh](http://people.cs.uchicago.edu/~rchugh/) and others
* [Fructure: A Structured Editing Engine in Racket](https://youtu.be/CnbVCNIh1NA) by Andrew Blinn
* [Hazel: A Live FP Environment with Typed Holes](https://youtu.be/UkDSL0U9ndQ) by [Cyrus Omar](https://web.eecs.umich.edu/~comar/)
* [Dark Demo](https://youtu.be/QgimI2SnpTQ) by [Ellen Chisa](https://twitter.com/ellenchisa)
* [Introduction to JetBrains MPS](https://youtu.be/JoyzxjgVlQw) by [Kolja Dummann](https://www.youtube.com/channel/UCq_mWDvKdXYJJzBmXkci17w)
* [Eve](http://witheve.com/)
    * code editor as prose writer
    * live preview
    * possible inspiration for live interactivity as well
*  [Unreal Engine 4](https://www.unrealengine.com/en-US/)
    * [Blueprints](https://docs.unrealengine.com/en-US/Engine/Blueprints/index.html) visual scripting (not suggesting visual scripting for Roc)

* [Live Programing](https://www.microsoft.com/en-us/research/project/live-programming/?from=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fprojects%2Fliveprogramming%2Ftypography.aspx#!publications) by [Microsoft Research] it contains many interesting research papers.

### Productivity features

* When refactoring; 
    - Cutting and pasting code to a new file should automatically add imports to the new file and delete them from the old file.
    - Ability to link e.g. variable name in comments to actual variable name. Comment is automatically updated when variable name is changed. 
* Automatically create all "arms" when pattern matching after entering `when var is` based on the type.
    - All `when ... is` should be updated if the type is changed, e.g. adding Indigo to the Color type should add an arm everywhere where `when color is` is used. 
* When a function is called like `foo(false)`, the name of the boolean argument should be shown automatically; `foo(`*is_active:*`false)`. This should be done for booleans and numbers.
* Suggest automatically creating a function if the compiler says it does not exist.
* Integrated search:
    * Searchbar for examples/docs. With permission search strings could be shared with the platform/package authors so they know exactly what their users are struggling with.


### Non-Code Related Inspiration

* [Scrivner](https://www.literatureandlatte.com/scrivener/overview) writing app for novelists, screenwriters, and more
* Word processors (Word, Google Docs, etc)
    * Comments that are parallel to the text of the document.
    * Comments can act as discussions and not just statements.
    * Easy tooling around adding tables and other stylised text
* Excel and Google Sheets
    * Not sure, maybe something they do well that we (code editors) could learn from


## Machine Learning Ideas

* Ability to record all changes to abstract syntax tree with user permission.
    * I think it is possible to create powerful automatic error resolution by having a dataset available of ast's with a specific error and the subsequent transformation that fixed the error.
    * GPT-3 can generate correct python functions based on a comment describing the functionality, video [here](https://www.youtube.com/watch?v=utuz7wBGjKM). It's possible that training a model using ast's may lead to better results than text based models.
    * Users with large private code bases could (re)train a publicly available error recovery model to experience benefits without having to share their code.
    * It could be useful to a user who is creating a function to show them the most similar function (type signature, name, comment) in a public+their private database. Say I was using a web framework and I just created a function that has a multipart form as argument, it would be great to have an example instantly available.
        * A simpler start for this idea without user data gathering: how the user a code snippet that is most similar to what they are currently writing. Snippets can be aggregated from examples, tests, docstrings at zero cost to the package/platform authors.
* Voice input:
    * Good for accessibility.
    * https://www.youtube.com/watch?v=Ffa3cXM7bjc is interesting for inspiration.
    * Could be efficient way to communicate with smart assistant.
    * Describe actions to execute them, examples:
        * Add latest datetime package to dependencies.
        * Generate unit test for this function.
        * Show edit history for this function.


## Testing
    
* From Google Docs' comments, adding tests in a similar manner, where they exists in the same "document" but parallel to the code being written
    * Makes sense for unit tests, keeps the test close to the source
    * Doesn't necessarily make sense for integration or e2e testing
    * Maybe easier to manually trigger a test related to exactly what code you're writing
* Ability to generate unit tests for a selected function in context menu
    * A table should appear to enter input and expected output pairs quickly
* Ability to "record" unit tests
    * Select a function to record.
    * Do a normal run, and save the input and output of the selected function.
    * Generate a unit test with that input-output pair


## General Thoughts/Ideas

Thoughts and ideas possibly taken from above inspirations or separate.

* ACCESSIBILITY!!!
* Nice backtraces that highlight important information
* Ability to show import connection within project visually
    * This could be done by drawing connections between files or functions in the tree view. This would make it easier for people to get their bearings in new big projects.
* Connections could also be drawn between functions that call each other in the tree view. The connections could be animated to show the execution flow of the program.
* Ability to inline statements contained in called functions into the callee function for debugging.
    * The value of expressions can be shown at the end of the line like in the [Inventing on Principle talk](https://youtu.be/8QiPFmIMxFc?t=1181)
    * This would give a clear overview of the execution and should make it easy to pinpoint the line where the bug originates.
    * That specific line can then be right clicked to go to the actual function.
    * Having to jump around between different functions and files is unnecessary and makes it difficult to see the forest through the trees.
* "Error mode" where the editor jumps you to the next error
    * Similar in theory to diff tools that jump you to the next merge conflict
* dependency recommendation
