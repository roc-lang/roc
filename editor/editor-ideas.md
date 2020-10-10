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

### Debugging

* [VS code debug visualization](https://marketplace.visualstudio.com/items?itemName=hediet.debug-visualizer)
* [Algorithm visualization for javascript](https://algorithm-visualizer.org)

### Structured Editing

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

### Non-Code Related Inspiration

* [Scrivner](https://www.literatureandlatte.com/scrivener/overview) writing app for novelists, screenwriters, and more
* Word processors (Word, Google Docs, etc)
    * Comments that are parallel to the text of the document.
    * Comments can act as discussions and not just statements.
    * Easy tooling around adding tables and other stylised text
* Excel and Google Sheets
    * Not sure, maybe something they do well that we (code editors) could learn from

## General Thoughts/Ideas

Thoughts and ideas possibly taken from above inspirations or separate.

* ACCESSIBILITY!!!
* From Google Docs' comments, adding tests in a similar manner, where they exists in the same "document" but parallel to the code being written
    * Makes sense for unit tests, keeps the test close to the source
    * Doesn't necessarily make sense for integration or e2e testing
    * Maybe easier to manually trigger a test related to exactly what code you're writing
* "Error mode" where the editor jumps you to the next error
    * Similar in theory to diff tools that jump you to the next merge conflict
* dependency recommendation
