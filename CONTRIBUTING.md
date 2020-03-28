Tips for contributors
---------------------

* In your local repository, ensure that everything compiles by **running
  `cmake . && make`** (this will launch byte compilation of lisp files and regression
  tests).
* You are then ready to make a **pull request**. Please make pull requests
  **against `master`**.
* When a [github] pull request is opened, checks will automatically run via
  GitHub Actions. Please check that your pull request is successfully compiled
  and tested on all Emacs and Ledger versions configured.

Glossary
--------

**[Markdown]**: A typesetter format that produces *html* files from *.md* files.
Note that GitHub automatically renders *.md* files.

**[Texinfo]**: GNU documentation typesetter that produces *html* and *pdf* files
from the `doc/\*.texi` files.

Source tree
-----------

The source tree can be confusing to a new developer. Here is a selective
orientation:

**./README.md**: user readme file in markdown format, also used as the project
description on GitHub.

**./LICENSE.md**: the [GPLv2] license.

**./*.el**: the [Emacs] ledger-mode lisp code.

**./doc/**: documentation, and tools for generating documents such as the *pdf*
manual.

**./test/**: regression tests.

**./tools/**: some tools, mostly small scripts, to aid development and CI.


[Markdown]: https://daringfireball.net/projects/markdown/
[Texinfo]: http://www.gnu.org/software/texinfo/
[git-flow]: http://nvie.com/posts/a-successful-git-branching-model/
[README.md]: https://github.com/ledger/ledger-mode/blob/master/README.md
[Emacs]: http://www.gnu.org/software/emacs/
[GPLv2]: http://www.gnu.org/licenses/gpl-2.0.html
[github]: https://github.com/ledger/ledger-mode/
