
# Open Questions

Should the editor organize all UI into a tree for easy altering/communication with plugins?

# Why make a new editor?

- Complete freedom and control to create a delightful roc editing, debugging, and execution monitoring experience. Everything can be optimized for this, there is no need to accommodate other use cases.
- So that plugins can be developed that can ship with roc packages. This allows all plugins to look and behave the same on all operating systems.
  - Why plugins:
    - To make it easier and more enjoyable to achieve your goal with the package.
    - To provide transparency for how the package works.
    - Opportunity to innovate on user experience with minimal friction install.
- Not limited by the language server protocol (LSP).
- To create a foundation that allows for easy experimentation.

# Why make a projectional editor


- It requires a lot less work to communicate with the compiler because we have a valid AST at all time.
- Similarly, we never have to deal with partial expressions that have not been fully typed out yet.
- The user never has to fiddle with formatting.
- It allows plugins to work with typed values instead of: a string that is connected with a typed value and where any changes to the typed value would have to produce a string that is sensibly formatted similar the formatting of the original string.

