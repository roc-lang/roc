# Who is the editor for?

Roc developers; beginners, experts, and everything in between.

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
