# Who is the editor for?

Roc developers; beginners, experts, and everything in between.

# Why make a new editor?

- Complete freedom and control to create a delightful roc editing, debugging, and execution monitoring experience. Everything can be optimized for this, there is no need to accommodate other use cases.
- So that plugins can be developed that can ship with roc packages. This allows all plugins to look and behave the same on all operating systems.
  - Why plugins:
    - To make it easier and more enjoyable to achieve your goal with the package.
    - To provide transparency for how the package works.
    - Opportunity to innovate on user experience with minimal friction install.
- Not limited by the language server protocol (LSP).
- To create a foundation that allows for easy experimentation.

# What will the editor do?

- Edit roc code.
- Debug roc programs.
- Allow everyone to make a plugin. Plugins can be standalone or come bundled with a package.
- Search and view documentation.
- Run tests with a nice UI.
- Make googling and stackoverflow redundant. You should be able to find the answer to your question with the editor.
- Accommodate those with disabilities.
- Provide traditional LSP functionality (without the actual language server); autocomplete, go to def, find references, rename, show type...

# General goals

- Follow UX best practices. Make the user feel comfortable, don't overload them with information or functionality. Offer a gentle learning curve. The editor should be usable without a tutorial.
- Maximal simplicity, strive for easy maintenance.
- Be respectful and transparent towards user privacy. Plugins should have permissions in accordance with this requirement.
- Editor code should be well-documented. Vital components should have live visualization to make it easy to understand and debug the editor.

# Non-goals

- Be a general-purpose editor for other programming languages.
- Take safe, familiar choices.
- Connect with LSP.
- Sell user data.
