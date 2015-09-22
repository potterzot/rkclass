Contributing
============

Thank you for your interest in contributing to `rkclass`! To contribute, [open an issue](https://github.com/potterzot/rkclass/issues/new) to discuss, or just [submit a pull request](https://github.com/potterzot/rkclass/compare/). Please try to follow the package coding style, which follows Hadley Wickham's [style guide](http://adv-r.had.co.nz/Style.html), in particular:

* Variable and function names should be clear and use underscores, like `vcov_kclass`, not `vcov.kclass` or `vcovkclass`. (You'll find some of these warts in the current code base.)
* In the case of **S3** methods, names will include periods or have to conform to the standard method name, like `estfun.kclass`, but in general try to use full words rather than abbreviations.
* Please document functions with [Roxygen](https://github.com/klutometis/roxygen).
