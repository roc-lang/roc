
# Meeting notes

- 18/1/2022 2pm GMT

## issue 2368

- How is AST data accessible to plugins?
- How does plugin UI work (UI components, vector level, pixel-level)?
- Given a selected expression, how do we show all plugins available that can visualize this
  expression or have an interactive widget to alter the expression (color picker). What does
  this API look like?
- use type driven UX? https://pchiusano.github.io/2013-09-10/type-systems-and-ux-example.html

## ideas

- Several "zoom levels" in the editor should show/hide context-appropriate views/buttons/functionality:
  + zoomed out view should show type defs and function defs with folded body
  + zooming in on function should unfold/show function body
  + Traditional IDE's like ecplise can show an overwhelming amount of possible buttons/actions and views. Zoom levels can be used to prevent this excess of available options.

- There should be a single editable text field to alter AST. This could be the same text field for entering commands, pressing a certain key could switch between command/plain text input into AST. Current part of AST that is being edited is highlighted.

- Hovering over expression should show button on left sidebar that allows you to pop out a pinned view of the expression.
  This pinned view could for example show all variants in a large type definition.
  Hovering over a type in a place other than the definition could show all variants, this hover view should also be capable of being pinned and moved around as desired.

- UI interaction specification from which we can generate both e.g. a window with clickable buttons 'previuous' and `next` that also supports the voice commands `previuous` and `next`.

Next actions to take:
- Zeljko: draft UI interaction in figma
- Anton: draft plugin API in roc
