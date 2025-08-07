(lang dune 2.8)

(name pyml)

(license BSD-2-Clause)

(maintainers "Seb Hinderer <sebastien.hinderer@inria.fr>")

(authors "Thierry Martinez <martinez@nsup.org>")

(source
 (github ocamllibs/pyml))

(bug_reports "http://github.com/ocamllibs/pyml/issues")

(homepage "http://github.com/ocamllibs/pyml")

(documentation "https://github.com/ocamllibs/pyml")

(generate_opam_files true)

(package
 (name pyml)
 (synopsis "OCaml bindings for Python")
 (description "OCaml bindings for Python 2 and Python 3")
 (depends
  (ocaml (>= 4.11.0))
  (stdcompat (>= 18))
  (conf-python-3-dev :with-test))
 (depopts utop))
