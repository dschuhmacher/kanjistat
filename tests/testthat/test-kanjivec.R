test_that("construct well-formed single kanjivec object", {
  skip_on_cran() # not automatically done, since not a snapshot test, but one which is rather flaky
  expect_equal( kanjivec(kanji = "藤", database="_data_and_more_snaps/", flatten="intelligent", simplify=TRUE), kanjivec_ref_fuji,
                ignore_attr = c("call", "kanjistat_version") ) 
  # res <- kanjivec(kanji = "藤", flatten="intelligent", simplify=TRUE)
  # saveRDS(res, "kanjivec_ref_fuji.rds")
  # kanjivec_ref_fuji is readRDS'ed in helper.R
  # with this kanji flattening for leaf children is also tested (parents g7 and g8),
  # but flattening of inner node children does not occur
})


test_that("construct well-formed list of multiple kanjivec objects", {
  skip_on_cran() # not automatically done, since not a snapshot test, but one which is rather flaky
  currentlist <- kanjivec(kanji = "藤藤藤", database="_data_and_more_snaps/", flatten="intelligent", simplify=TRUE)
    # should still give list of length 3
  expect_type(currentlist, "list")
  expect_length(currentlist, 3)
  
  classes_ref <- rep("kanjivec", 3)
  attr(classes_ref, "names") <- c("kvec085e4", "kvec085e4", "kvec085e4") # these are from the right codepoints
  expect_equal( sapply(currentlist, class), classes_ref )
  
  expect_equal(currentlist[[2]] , kanjivec_ref_fuji,
               ignore_attr = c("call", "kanjistat_version") )
  # kanjimat_ref_fuji is readRDS'ed in helper.R
  # we bluntly exclude call here, otherwise diff
  # "kanjimat(kanji = \"鶏処藤\", family = \"wqy-microhei\", size = 64)" (actual)
  # "kanjimat(kanji = \"藤\", family = \"wqy-microhei\", size = 64)"  (expected)
})


test_that("stroketree and strokedend produced with various options of flatten in kanjivec", {
  skip_on_cran() # not automatically done, since not a snapshot test, but one which is rather flaky
  current_all <- kanjivec(kanji = "郵", database="_data_and_more_snaps/", flatten=TRUE, simplify=TRUE)
  current_inner <- kanjivec(kanji = "郵", database="_data_and_more_snaps/", flatten="inner", simplify=TRUE)
  current_outer <- kanjivec(kanji = "郵", database="_data_and_more_snaps/", flatten="leaves", simplify=TRUE)
  current_none <- kanjivec(kanji = "郵", database="_data_and_more_snaps/", flatten=FALSE, simplify=TRUE)
  # stroketrees
  expect_equal( current_all$stroketree, treedend_ref_yuumail$stroketree_all)
  expect_equal( current_inner$stroketree, treedend_ref_yuumail$stroketree_inner)
  expect_equal( current_outer$stroketree, treedend_ref_yuumail$stroketree_outer)
  expect_equal( current_none$stroketree, treedend_ref_yuumail$stroketree_none)
  # strokedends
  expect_equal( current_all$strokedend, treedend_ref_yuumail$strokedend_all)
  expect_equal( current_inner$strokedend, treedend_ref_yuumail$strokedend_inner)
  expect_equal( current_outer$strokedend, treedend_ref_yuumail$strokedend_outer)
  expect_equal( current_none$strokedend, treedend_ref_yuumail$strokedend_none)
})
# yuumail <- list()
# fall <- kanjivec(kanji = "郵", flatten=TRUE, save=FALSE, simplify=TRUE)
# finner <- kanjivec(kanji = "郵", flatten="inner", save=FALSE, simplify=TRUE)
# fouter <- kanjivec(kanji = "郵", flatten="leaves", save=FALSE, simplify=TRUE)
# fnone <- kanjivec(kanji = "郵", flatten=FALSE, save=FALSE, simplify=TRUE)
# yuumail$stroketree_all <- fall$stroketree
# yuumail$strokedend_all <- fall$strokedend
# yuumail$stroketree_inner <- finner$stroketree
# yuumail$strokedend_inner <- finner$strokedend
# yuumail$stroketree_outer <- fouter$stroketree
# yuumail$strokedend_outer <- fouter$strokedend
# yuumail$stroketree_none <- fnone$stroketree
# yuumail$strokedend_none <- fnone$strokedend
# saveRDS(yuumail, file="kanjivec_flatten_ref_yuumail.rds")
# plot(fnone, type="dend", family="hiragino_sans")
# str(fnone$stroketree)
# str(fnone$strokedend)
# plot(finner, type="dend", family="hiragino_sans")
# str(finner$stroketree)
# str(finner$strokedend)
# plot(fouter, type="dend", family="hiragino_sans")
# str(fouter$stroketree)
# str(fouter$strokedend)
# plot(fall, type="dend", family="hiragino_sans")
# str(fall$stroketree)
# str(fall$strokedend)


test_that("plot kanjivec object, type kanji", {
  skip_if_not(capabilities("cairo"))
  kanjivec_kanji_to_jpg <- function(kanji) {
    path <- tempfile("fuji_kanji", fileext = ".jpg")
    jpeg(path, width = 480, height = 480)
    par(mai=rep(0.05,4))
    plot(kanji, type = "kanji", seg_depth = 4)
    dev.off()
    return(path)
  }
  expect_snapshot_file( kanjivec_kanji_to_jpg(kanjivec_ref_fuji), "fuji_kanji.jpg" )
})

test_that("plot kanjivec object, type dend", {
  skip_if_not(capabilities("cairo"))
  kanjivec_dend_to_jpg <- function(kanji) {
    path <- tempfile("fuji_dend", fileext = ".jpg")
    jpeg(path, width = 480, height = 480)
    par(mai=rep(0.05,4))
    plot(kanji, type = "dend", family = "wqy-microhei")
    dev.off()
    return(path)
  }
  expect_snapshot_file( kanjivec_dend_to_jpg(kanjivec_ref_fuji), "fuji_dend.jpg" )
})


test_that("print kanjivec object", {
  withr::local_options(width = 68)
  expect_snapshot( print(kanjivec_ref_fuji) )
})


test_that("str for kanjivec object", {
  withr::local_options(width = 68)
  expect_snapshot( str(kanjivec_ref_fuji) )
})

# stroketree_to_dend and plotdend are "implicitly tested" by the tests for kanjivec and plot.kanjivec
