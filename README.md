##rkclass: Implementation of K-class regression estimators in R

This package implements the general class of linear instrumental variables regression methods known as "k-class" estimators. Instrumental variables regressions are often used in causal inference of the type discussed in Angrist and Pischke's [Mostly Harmless Econometrics](http://www.mostlyharmlesseconometrics.com/) and [Mastering 'Metrics](http://masteringmetrics.com/).

My initial interest in instrumental variables regression began while I was working as a research assistant at the National Bureau of Economic Research (NBER) on the [Moving to Opportunity](http://www.nber.org/mtopublic/) project. I was given the task of trying to implement bekker-adjusted standard errors for some of the coefficients. Pursuit of this led me into the depths of Stata's Mata programming and obscure help files detailing the calculation of robust standard errors under clustering. Days later when I resurfaced I had very nearly, but not quite, been able to at least replicate the results we obtained using `ivreg2`. Years later, I find myself unable to shake the bug that I never was able to complete, and hence this package was born.

###Installation

For now `rkclass` is not on CRAN. You can instead install it via github:

    library(devtools)
    devtools::install_github("potterzot/rkclass")

###Contributing

This package needs substantial work to be as fully functioning as `ivreg2` in Stata is, and all contributions are warmly welcome! There are a few great ways that you could contribute:

* If you don't know something, please let me know by [opening an issue](https://github.com/potterzot/rkclass/issues/new) or sending me an email. This will help make me aware of problems, things that need more explanation, etc...
* Feature requests. If there is something you'd really like to see added that isn't, and you don't feel up to the task of adding it yourself, please open and issue and request a feature. I'd like to create a roadmap of features to add.
* If you spot some errors, you can either [open an issue](https://github.com/potterzot/rkclass/issues/new) or edit the file directly and then submit a pull request.
* If you submit a pull request with substantive code updates (which would be AWESOME!), please do your best to follow a similar coding style, document new functions, etc... I'm happy to help provide some suggestions if needed.

###Use

*Be warned that `rkclass` is still under development. The results for standard regressions should be correct (and are tested against `AER::ivreg`, but other methods may not be available and function calls may change.*

If instrumental variables regressions (2SLS, LIML, FULLER) include many instruments, the standard errors can be incorrect. Hansen, Hausman, and Newey [1] generalized Bekker's [2] standard error adjustments for use in multiple situations other than just LIML.

Use syntax follows the syntax from `AER::ivreg`, which looks like `kclass(formula, data, ...)`. The returned result is a model object that has many (hopefully all) of the associated methods, including `summary`, `print`, etc... For more usage examples see the getting started vignette by typing `vignette('kclass-intro')`. For a more in depth look, see `vignette('AngristKrueger1991')`, which provides a replication of some of the results of that paper.

###Acknowledgements
Many resources were helpful in the development of `rkclass`. Of special note are:

* Hadley Wickham's [R Packages](http://r-pkgs.had.co.nz/), a fantastic guide to R package development and writing R code in general.

* Matthieu Stigler's [RCompAngrist](https://github.com/MatthieuStigler/RCompAngrist) repository on github, which implements a kclass method.

* [Applied Economics with R](https://cran.r-project.org/web/packages/AER/index.html), a package developed in support of the 2008 book of the same name by Christian Kleiber and Achim Zeileis.

* Stata's [ivreg2](https://ideas.repec.org/c/boc/bocode/s425401.html) package, written by Christopher Baum, Mark Schaffer, and Steve Stillman [1], which provides helpful documentation and a whole suite of methods to test against.  

Much of the theory behind the code is from the following sources:

- Bekker, Paul A. "Alternative approximations to the distributions of instrumental variable estimators." *Econometrica: Journal of the Econometric Society* (1994): 657-681.

- Davidson, Russell, and James G. MacKinnon. "Estimation and inference in econometrics." OUP Catalogue (1993).

- Hansen, Christian, Jerry Hausman, and Whitney Newey. "Estimation with many instrumental variables." *Journal of Business & Economic Statistics* 26.4 (2008).

###References
[1] Baum, C.F., Schaffer, M.E., Stillman, S. (2007), "ivreg2: Stata module for extended instrumental variables/2SLS, GMM and AC/HAC, LIML and k-class regression," http://ideas.repec.org/c/boc/bocode/s425401.html.

[2] Hansen, C., J. A. Hausman, and W. Newey (2008), "Estimation with Many Instrumental Variables,"" Journal of Business and Economic Statistics, 26:4. DOI: 10.1198/073500108000000024.

[3] Bekker, P. A. (1994), "Alternative Approximations to the Distributions of Instrumental Variables Estimators," Econometrica, 63, 657–681.

[4] Angrist, J., and Krueger, A. (1991), “Does Compulsory School Attendance Affect Schooling and Earnings,” Quarterly Journal of Economics, 106, 979–1014.

