(For background, [this talk](https://youtu.be/ZnYa99QoznE?t=4790) has an overview of the design goals for the editor.)

# Editor Ideas

Here are some ideas and interesting resources for the editor. Feel free to make a PR to add more!

## Sources of Potential Inspiration

These are potentially inspirational resources for the editor's design.

Nice collection of research on innovative editors, [link](https://futureofcoding.org/catalog/).

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
* [Luna](https://www.luna-lang.org/) language for interactive data processing and visualization

### Debugging

* [VS code debug visualization](https://marketplace.visualstudio.com/items?itemName=hediet.debug-visualizer)
* [Algorithm visualization for javascript](https://algorithm-visualizer.org)
* [godbolt.org Compiler Explorer](https://godbolt.org/)
* Say you have a failing test that used to work, it would be very valuable to see all code that was changed that was used only by that test.
e.g. you have a test `calculate_sum_test` that only uses the function `add`, when the test fails you should be able to see a diff showing only what changed for the function `add`. It would also be great to have a diff of [expression values](https://homepages.cwi.nl/~storm/livelit/images/bret.png) Bret Victor style. An ambitious project would be to suggest or automatically try fixes based on these diffs.
+ [whitebox debug visualization](https://vimeo.com/483795097)


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
* [Math Inspector](https://mathinspector.com/), [github](https://github.com/MathInspector/MathInspector)
* [Lamdu](http://www.lamdu.org/) live functional programming.
* [Sourcetrail](https://www.sourcetrail.com/) nice tree-like source explorer.
* [Unisonweb](https://www.unisonweb.org), definition based [editor](https://twitter.com/shojberg/status/1364666092598288385) as opposed to file based.

### Voice Interaction Related

* We should label as many things as possible and expose jumps to those labels as shortkeys.
* Update without user interaction. e.g. autosave.
* Could be efficient way to communicate with smart assistant.
* You don't have to remember complex keyboard shortcuts if you can describe actions to execute them. Examples:
    * Add latest datetime package to dependencies.
    * Generate unit test for this function.
    * Show edit history for this function.
    * Adjusting settings: switch to light theme, increase font size...
* Use (context specific) voice command state machine to assist Machine Learning voice recognition model.
* Nice special use case: using voice to code while on treadmill desk. 

    
#### Inspiration

* Voice control and eye tracking with [Talon](https://github.com/Gauteab/talon-tree-sitter-service)
* [Seminar about programming by voice](https://www.youtube.com/watch?v=G8B71MbA9u4)
* [Talon voice commands in elm](https://github.com/Gauteab/talon-tree-sitter-service)
* Mozilla DeepSpeech model runs fast, works pretty well for actions but would need additional training for code input.
    Possible to reuse [Mozilla common voice](https://github.com/common-voice/common-voice) for creating more "spoken code" data.

### Productivity features

* When refactoring; 
    - Cutting and pasting code to a new file should automatically add imports to the new file and delete them from the old file.
    - Ability to link e.g. variable name in comments to actual variable name. Comment is automatically updated when variable name is changed.
    - When updating dependencies with breaking changes; show similar diffs from github projects that have succesfully updated that dependency.
    - AST backed renaming, changing variable/function/type name should change it all over the codebase.  
* Automatically create all "arms" when pattern matching after entering `when var is` based on the type.
    - All `when ... is` should be updated if the type is changed, e.g. adding Indigo to the Color type should add an arm everywhere where `when color is` is used. 
* When a function is called like `foo(false)`, the name of the boolean argument should be shown automatically; `foo(`*is_active:*`false)`. This should be done for booleans and numbers.
* Suggest automatically creating a function if the compiler says it does not exist.
* Integrated search:
    * Searchbar for examples/docs. With permission search strings could be shared with the platform/package authors so they know exactly what their users are struggling with.
* Show productivity/feature tips on startup. Show link to page with all tips. Allow not seeing tips next time.
* Search friendly editor docs inside the editor. Offer to send search string to Roc maintainers when no results, or if no results were clicked.
* File history timeline view. Show timeline with commits that changed this file, the number of lines added and deleted as well as which user made the changes. Arrow navigation should allow you to quickly view different versions of the file.
* Suggested quick fixes should be directly visible and clickable. Not like in vs code where you put the caret on an error until a lightbulb appears in the margin which you have to click for the fixes to apppear, after which you click to apply the fix you want :( .
* Regex-like find and substitution based on plain english description and example (replacement). i.e. replace all `[` between double quotes with `{`. [Inspiration](https://alexmoltzau.medium.com/english-to-regex-thanks-to-gpt-3-13f03b68236e).
* Show productivity tips based on behavior. i.e. if the user is scrolling through the error bar and clicking on the next error several times, show a tip with "go to next error" shortcut.
#### Autocomplete

- Use more space for autocomplete options:
   * Multiple columns. Columns could have different sources, i.e. middle column suggests based on current folder, left column on whole project, right column on github.
   * show cell with completion + auto import suggestion
- Webcam based eye tracking for quick selection.
- Machine Learning:
   * GPT-3 can generate correct python functions based on a comment describing the functionality, video [here](https://www.youtube.com/watch?v=utuz7wBGjKM). It's possible that training a model using ast's may lead to better results than text based models.

#### Productivity Inspiration

* [Kite](https://www.kite.com/) AI autocomplete and doc viewer.
* [Tabnine](https://www.tabnine.com/) AI autocomplete.
* [Codota](https://www.codota.com) AI autocomplete and example searching.

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
    * Users with large private code bases could (re)train a publicly available error recovery model to experience benefits without having to share their code.
    * It could be useful to a user who is creating a function to show them the most similar function (type signature, name, comment) in a public+their private database. Say I was using a web framework and I just created a function that has a multipart form as argument, it would be great to have an example instantly available.
        * A simpler start for this idea without user data gathering: how the user a code snippet that is most similar to what they are currently writing. Snippets can be aggregated from examples, tests, docstrings at zero cost to the package/platform authors.
        * See [codata](https://www.codota.com/code/java/classes/okhttp3.OkHttpClient) for inspiration on a snippet/example finder.
* Fuzzy natural language based setting adjustment in search bar or with voice input: increase font size, enable autosave, switch to light theme...
* Detect deviation of best practices, example case: alert developer when they are defining a color inline (rgb(30,30,30)) while all colors have been previously imported from a single file. See also [Codota](https://www.codota.com).


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

## Documentation

* Ability to see module as it would be presented on a package website.
    * Modern editors may guide developers to the source code too easily.
    The API and documentation are meant to interface with humans.

## General Thoughts/Ideas

Thoughts and ideas possibly taken from above inspirations or separate.

* ACCESSIBILITY === EMPATHY
   * Visual Imapirments 
      No Animation is most benign form of cognitive disabity but really important base line of people with tense nerve system.
      Insensitivity to certain or all colors.
      Need of highcontrast
      Or Everything Magnified for me with no glasses. 
      Or Total blindness where we need to trough sound to communicate to the user
      Screen readers read trees of labeled elements. Each platform has different apis, but I think they are horrible. Just close your eyes and imagine listening to screen reader all day while you are using this majectic machines called computers.
      But blind people walk with a tool and they can react much better to sound/space relations than full on visal majority does. They are acute to sound as a spatial hint. And a hand for most of them is a very sensitive tool that can make sounds in space.
      Imagine if everytime for the user doesnt want to rely on shining rendered pixels on the screen for a feedback from machine, we make a accoustic room simulation, where with moving the "stick", either with mouse or with key arrows, we bump into one of the objects and that produces certain contextually appropriate sound (clean)*ding*
      
      On the each level of abstraction they can make sounds more deeper, so then when you type letters you feel like you are playing with the sand (soft)*shh*. We would need help from some sound engeneer about it, but imagine moving down, which can be voice trigered command for motion impaired, you hear (soft)*pup* and the name of the module, and then you have options and commands appropriate for the module, they could map to those  basic 4 buttons that we trained user on, and he would shortcut all the soft talk with click of a button. Think of the satisfaction when you can skip the dialog of the game and get straight into action. (X) Open functions! each function would make a sound and say its name, unless you press search and start searching for a specific function inside module, if you want one you select or move to next.
      
   * Motor impariments
      [rant]BACKS OF CODERS ARE NOT HEALTHY! We need to change that![/neverstop]
      Too much mouse waving and sitting for too long is bad for humans.
      Keyboard is basic accessability tool but 
      Keyboard is also optional, some people have too shaky hands even for keyboard. 
      They rely on eye tracking to move mouse cursor arond.
      If we employ _some_ voice recognition functions we could make same interface as we could do for consoles where 4+2 buttons and directional pad would suffice.
      That is 10 phrases that need to be pulled trough as many possible translations so people don't have to pretend that they are from Maine or Texas so they get voice recognition to work. Believe me I was there with Apple's Siri :D That is why we have 10 phrases for movement and management and most basic syntax.
      

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
