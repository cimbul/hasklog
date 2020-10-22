Hasklog |ci-badge|_ |ghc-badge|_ |stack-badge|_ |license-badge|_
=======

A toy Prolog interpreter/compiler written in Haskell.

This is less a real Prolog implementation than an excuse for me to play with logic programming, Haskell, and compiler design. It does not conform to ISO Prolog, and in fact completely omits side effects and higher-order predicates like *findall/3* and *call/1*.

For usage instructions and technical information, see the `Manual <doc/Manual.rst>`_.

.. |ci-badge| image:: https://travis-ci.org/cimbul/hasklog.svg
.. _ci-badge: https://travis-ci.org/cimbul/hasklog
.. |ghc-badge| image:: https://img.shields.io/badge/dynamic/yaml.svg?label=ghc&query=ghc&url=https%3A%2F%2Fraw.githubusercontent.com%2Fcimbul%2Fhasklog%2Fmain%2F.travis.yml
.. _ghc-badge: .travis.yml
.. |stack-badge| image:: https://img.shields.io/badge/dynamic/yaml.svg?label=stack&query=resolver&url=https%3A%2F%2Fraw.githubusercontent.com%2Fcimbul%2Fhasklog%2Fmain%2Fstack.yaml
.. _stack-badge: stack.yaml
.. |license-badge| image:: https://img.shields.io/github/license/cimbul/hasklog.svg
.. _license-badge: LICENSE
