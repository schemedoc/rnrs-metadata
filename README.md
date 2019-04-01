# Scheme RnRS metadata

This repo does fully automatic extraction from the unmodified TeX
sources of the Scheme RnRS documents. If you run `./download.rkt` and
then `./parse.rkt` you will obtain exactly the same result as I did.

* `download.rkt` -- download and extract rNrs TeX files
* `parse.rkt` -- parse metadata from extracted TeX files
* `r?rs/*.tex` -- unmodified TeX sources extracted by `download.rkt`
* `r?rs-args.scm` -- procedure and macro signatures extracted by `parse.rkt`
