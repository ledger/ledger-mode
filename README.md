[![Build Status](https://github.com/ledger/ledger-mode/workflows/CI/badge.svg)](https://github.com/ledger/ledger-mode/actions)
[![License](https://img.shields.io/badge/license-GPL--2.0-blue.svg?style=flat)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![MELPA](http://melpa.org/packages/ledger-mode-badge.svg)](http://melpa.org/#/ledger-mode)
[![MELPA Stable](https://stable.melpa.org/packages/ledger-mode-badge.svg)](https://stable.melpa.org/#/ledger-mode)

ledger-mode
===========

This Emacs library provides a major mode for editing files in the
format used by the [`ledger`](https://github.com/ledger/ledger)
command-line accounting system.

It also provides automated support for some `ledger` workflows, such
as reconciling transactions, or running certain reports.

Installation
=============

If you choose not to use one of the convenient
packages in [MELPA][melpa] or [MELPA Stable][melpa-stable], you'll need to
add the directory containing `ledger-mode.el` to your `load-path`, and
then `(require 'ledger-mode)`.

Configuring completion
======================

Earlier `ledger-mode` versions had an always-on `TAB` completion system,
but now the code uses the standard Emacs `completion-at-point` system
for compatibility with all completion UIs, e.g. `company` or `helm`.

See the "Adding Transactions" section of the ledger-mode Info manual for
more information.

Getting started
===============

`ledger-mode` will automatically associate itself with `.ledger` files when
installed as a package. `ledger-mode` includes documentation in info format,
accessible through Emacs with <kbd>C-h i</kbd>. The info chapter includes a
quick demo as well as more extensive documentation.

Related packages
================

In-buffer checking of formatting and balancing of transactions is available
built-in for Emacs version 26 and later using `flymake-mode`. For flycheck users
(and users of Emacs 25 and earlier), [`flycheck-ledger`][flycheck-ledger] is
available.




[melpa]: https://melpa.org
[melpa-stable]: https://stable.melpa.org
[flycheck-ledger]: https://github.com/purcell/flycheck-ledger
