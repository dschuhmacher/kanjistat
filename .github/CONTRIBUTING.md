# Contributing to kanjistat

You can contribute to kanjistat by reporting problems, helping to improve the user experience or adding new functionality and data.

Generally speaking, any contribution related to statistical aspects of Japanese kanji (and possibly hiragana/katakana) is very welcome. Functionality that targets other aspects of the Japanese language, such as tokenization and word level analysis of texts, is probably better placed elsewhere.

The following gives some guidelines for contributions. These are largely taken from the [tidyverse](https://www.tidyverse.org/) collection of packages, as the kanjistat package follows a (somewhat less strict) tidyverse philosophy.


## Smaller changes

You can fix typos, spelling mistakes or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not an `.Rd` file. You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it is usually a good idea to [file an issue](https://github.com/dschuhmacher/kanjistat/issues) first to make sure that there is sufficient need or interest for your suggestion. Sometimes there is another way of doing things that is already in place or there might be a negative effect on existing code that you are not aware of.

If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help to write a unit test, if needed).



### Pull request process

* Fork the package and clone onto your computer. This can be done conveniently from R via
 `usethis::create_from_github("dschuhmacher/kanjistat", fork = TRUE)`.

* Install all development dependences with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 

* Create a Git branch for your pull request (PR). From R, you can say `usethis::pr_init("brief-description-of-change")`.

* Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

* For user-facing changes, add a bullet to the top of `NEWS.md`.

### Code style

* New code should follow the tidyverse [style guide](https://style.tidyverse.org). 

* We use [roxygen2](https://cran.r-project.org/package=roxygen2) with [Markdown syntax](https://roxygen2.r-lib.org/articles/rd-formatting.html) for documentation.  

* We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.


## Attribution

These guidelines are adapted from the [tidyverse](https://www.tidyverse.org/) collection, mostly from [purrr](https://purrr.tidyverse.org/CONTRIBUTING.html).
