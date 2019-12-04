# Not ready to be shared yet!

Roc is a language for building reliable applications on top of fast platforms.

## Applications and Platforms

Applications are often built on a *framework.* Typically, both application and framework are written in the same language.
* [Rails](https://rubyonrails.org/) applications are written in Ruby, and so is Rails.
* [Angular](https://angularjs.org/) applications are written in TypeScript, and so is Angular.
* [Phoenix](https://phoenixframework.org/) applications are written in Elixir, and so is Phoenix.

Some programs support plugins. Often the plugins are written in the same language as the underlying program.
* [Sublime Text](https://www.sublimetext.com/) plugins are written in Python, and so is Sublime.
* [Webpack](https://webpack.js.org/) plugins are written in JavaScript, and so is Webpack.
* [Leiningen](https://leiningen.org/) plugins are written in Clojure, and so is Leiningen.

All of these can be considered examples of a platform/application relationship. There is an underlying platform, and many applications are built on top of it. (Plugins are a type of application in this sense.)

Sometimes, platforms and their applications are written in different languages.

* [Neovim](https://neovim.io/) is written in C for performance, and its plugins can be written in languages such as Python, JS, and Ruby.
* [NGINX](https://www.nginx.com/) is written in C for performance, and its plugins can be written in a [subset of JavaScript](https://www.nginx.com/blog/introduction-nginscript/).
* [Unity](https://unity.com/) is written in C++ for performance, and Unity applications (such as games) can be written in C#, Boo, or a JavaScript dialect called UnityScript.

Like in the previous examples, application authors building on these platforms get to use high-level languages with automatic memory management. They make no ergonomics sacrifices, and may not even be aware that the underlying platform is written in a lower-level language.

By using systems-level programming languages like C and C++, platform authors sacrifice development speed, but unlock the highest possible performance characteristics. This is a tradeoff many platform authors are happy to accept, for the sake of having applications built on their platforms run very fast.

## Roc's Design

Roc is designed to make the "systems-level platform, higher-level application" experience as nice as possible.

* **Application** authors code exclusively in Roc. It's a language designed for nice ergonomics. The syntax resembles Ruby or CoffeeScript, and it has a fast compiler with full type inference.
* **Platform** authors code almost exclusively in Rust, except for the thin Roc API they expose to application authors. Roc application code compiles to Rust behind the scenes, so production builds of Roc apps benefit from the Rust compiler's whole-program optimizations. Roc application authors do not even need to know Rust exists.

Every Roc application is built on top of exactly one Roc platform. There is no such thing as a Roc application that runs without a platform, and there is no default platform. You must choose one!

The core Roc language and standard library include no I/O operations, which gives platform authors complete control over which effects they want to support. Some of the implications of this include:

* A high-performance build tool (or text editor) written in Rust can be a Roc platform with a strong plugin security model. For example, it could expose only operations allowing plugin authors to modify the contents of certain files, rather than allowing plugins arbitrary read/write access to the entire filesystem.
* A VR or [Arduino](https://www.arduino.cc/) platform can expose uncommon I/O operations supported by that hardware, while omitting common I/O operations that are unsupported (such as reading keyboard input from a terminal that doesn't exist). 
* A high-performance Web server written in Rust can be a Roc platform where all I/O operations are implemented in terms of Streams or Observables rather than a more traditional asynchronous abstraction like Futures or Promises. This would mean all code in that platform's ecosystem would be necessarily built on a common streaming abstraction.

Each Roc platform gets its own separate package repository, with packages built on top of the API that platform exposes. This means each platform has its own ecosystem where everything is built on top of the same shared set of platform-specific primitives.

## Project Goals

Roc is in the extremely early stages of development. It barely does anything yet. With any luck, it will support doing something useful in 2020.

Besides the above language design, a separate goal is for Roc to ship with an ambitiously boundary-pushing graphical editor. Not like "an IDE," but rather something that makes people say "I have never seen anything remotely like this outside of Bret Victor demos."

One of the reasons this editor is coupled with the language itself is to allow package authors to include custom editor tooling inside packages.

A trivial example: suppose I'm writing a Roc app for an Arduino platform. I install a platform-specific package for displaying text on a grid of LEDs. Because I've installed this package, at the call site where I call the function to specify the color of the text on the LEDs, my Roc editor displays an inline color picker. As I move a slider around to try out different colors, not only does my code change to reflect that value in realtime, but the physical LEDs in my room change color in realtime as well. As the application author, all I did to get that experience was to install the "text on an LED grid" package, nothing else.

The goal is for this to be one of the most trivial, bare minimum examples of what the editor experience would be like. Hopefully, people in the future will look back on this example and say "that's so embarassingly basic; why didn't you talk about one of the *actually great* things in the seamless editor plugin ecosystem?"

Finally, some implementation goals:

* The web server for the package manager is written in Roc (with an underlying Rust platform for the web server, for example [warp](https://github.com/seanmonstar/warp)).
* The editor plugins are written in Roc (with an underlying Rust platform for the editor itself, for example using [gfx-hal](https://github.com/gfx-rs/gfx)).
* The CLI (for building Roc projects on CI platforms) has its user interface written in Roc (with an underlying Rust platform for fast compilation and basic CLI interactions).

It's an ambitious project! It'll take a long time to get where it's going, but hopefully it'll be worth the wait.
