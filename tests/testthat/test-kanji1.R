test_that("lookup readmean", {
  withr::local_options(width = 68)
  expect_snapshot( lookup(c("晴", "曇", "雨")) )
  expect_snapshot( lookup(c("膵天𤢖")) )   # in particular this line may change with kanjidic updates
})

test_that("lookup basic and morphologic", {
  tib1 <- lookup(c("憬", "学", "閲"), what="basic")
  expect_equal( tib1, kbase[c(2126, 17,1957),])
  tib2 <- lookup(c("憬", "学", "閲"), what="morphologic")
  expect_equal( tib2, kmorph[c(2126, 17,1957),])
})


test_that("kanjiplot", {
#  for later: testthat::expect_snapshot_file has no pdf support (yet?!)
#  (the test always fails although the pdfs look the same, probably some time stamp or similar)
#  save_pdf <- function(kanji) {
#    path <- tempfile("koucha", fileext = ".pdf")
#    plotkanji(kanji, device="pdf", family = "wqy-microhei", factor=10, height = 1.8, file=path)
#    return(path)
#  }
#  expect_snapshot_file(save_pdf("紅"), "koucha.pdf")
  skip_if_not(capabilities("cairo")) # for svg so that it runs and for png for reproducibility
  skip_on_cran()
  skip_on_ci()  # (it seems on some systems a border is added!!?? --> fix)
  save_svg <- function(kanji) {
    path <- tempfile("koucha", fileext = ".svg")
    plotkanji(kanji, device = "svg", family = "wqy-microhei", factor = 10, height = 1.8,
              filename=path, antialias = "gray")
    return(path)
  }
  expect_snapshot_file(save_svg("紅"), "koucha.svg")
  
  save_png <- function(kanji) {
    path <- tempfile("koucha", fileext = ".png")
    plotkanji(kanji, device = "png", family = "wqy-microhei", factor = 5, height = 64,
              filename = path, antialias = "gray")
    return(path)
  }
  expect_snapshot_file(save_png("紅"), "koucha.png")
}) 
