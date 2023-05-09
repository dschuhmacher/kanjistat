test_that("construct well-formed single kanjimat object", {
  skip_on_cran() # not automatically done, since not a snapshot test, but one which is rather flaky
  skip_on_ci()
  skip_if_not(capabilities("cairo")) # kanjimat_ref_fuji.rds was constructed with cairo
  expect_equal( kanjimat(kanji = "藤", family = "wqy-microhei", size = 64), kanjimat_ref_fuji,
                ignore_attr = c("kanjistat_version", "Rversion", "platform") )
  # kanjimat_ref_fuji is readRDS'ed in helper.R
})


test_that("construct well-formed list of multiple kanjimat objects", {
  skip_on_cran() # not automatically done, since not a snapshot test, but one which is rather flaky
  skip_on_ci()
  skip_if_not(capabilities("cairo")) # kanjimat_ref_fuji.rds was constructed with cairo
  currentlist <- kanjimat(kanji = "鶏処藤", family = "wqy-microhei", size = 64)
  
  expect_type(currentlist, "list")
  expect_length(currentlist, 3)
  
  classes_ref <- rep("kanjimat", 3)
  attr(classes_ref, "names") <- c("kmat09d8f", "kmat051e6", "kmat085e4") # these are from the right codepoints
  expect_equal( sapply(currentlist, class), classes_ref )
  
  expect_equal(currentlist[[3]] , kanjimat_ref_fuji,
               ignore_attr = c("call", "kanjistat_version", "Rversion", "platform") )
  # kanjimat_ref_fuji is readRDS'ed in helper.R
  # we bluntly exclude call here, otherwise diff
  # "kanjimat(kanji = \"鶏処藤\", family = \"wqy-microhei\", size = 64)" (actual)
  # "kanjimat(kanji = \"藤\", family = \"wqy-microhei\", size = 64)"  (expected)
})


test_that("plot kanjimat object", {
  skip_on_cran() 
  skip_on_ci()
  skip_if_not(capabilities("cairo"))
  kanjimat_to_png <- function(kanji) {
    path <- tempfile("fuji", fileext = ".png")
    png(path, width = 64, height = 64)
    plot(kanji)
    dev.off()
    return(path)
  }
  expect_snapshot_file( kanjimat_to_png(kanjimat_ref_fuji), "fuji.png" )
})


test_that("print kanjimat object", {
  withr::local_options(width = 68)
  expect_snapshot( print(kanjimat_ref_fuji) )
})

