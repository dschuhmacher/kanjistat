# extensive tests of convert_kanji
# 
test_that("convert_kanji, single entry, simplify", {
  expect_equal( convert_kanji(25)[1:3], list(index=25, character="山", hexmode=as.hexmode("5c71")) )
  expect_equal( convert_kanji(list(25))[1:3], list(index=25, character="山", hexmode=as.hexmode("5c71")) )
  #
  expect_equal( convert_kanji(25, "index"), 25 )
  expect_equal( convert_kanji(25, "character"), "山" )
  expect_equal( convert_kanji(25, "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji(list(25), "index"), 25 )
  expect_equal( convert_kanji(list(25), "character"), "山" )
  expect_equal( convert_kanji(list(25), "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji("山", "index"), 25 )
  expect_equal( convert_kanji("山", "character"), "山" )
  expect_equal( convert_kanji("山", "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji(list("山"), "index"), 25 )
  expect_equal( convert_kanji(list("山"), "character"), "山" )
  expect_equal( convert_kanji(list("山"), "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji(as.hexmode("5c71"), "index"), 25 )
  expect_equal( convert_kanji(as.hexmode("5c71"), "character"), "山" )
  expect_equal( convert_kanji(as.hexmode("5c71"), "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji(list(as.hexmode("5c71")), "index"), 25 )
  expect_equal( convert_kanji(list(as.hexmode("5c71")), "character"), "山" )
  expect_equal( convert_kanji(list(as.hexmode("5c71")), "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji("0x5c71", "index"), 25 )
  expect_equal( convert_kanji("0x5c71", "character"), "山" )
  expect_equal( convert_kanji("0x5c71", "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji(list("0x5c71"), "index"), 25 )
  expect_equal( convert_kanji(list("0x5c71"), "character"), "山" )
  expect_equal( convert_kanji(list("0x5c71"), "hexmode"), as.hexmode("5c71") )
  expect_equal( convert_kanji(kanjivec_ref_fuji, "index"), 1032 )
  expect_equal( convert_kanji(kanjivec_ref_fuji, "character"), "藤" )
  expect_equal( convert_kanji(kanjivec_ref_fuji, "hexmode"), as.hexmode("85e4") )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji), "index"), 1032 )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji), "character"), "藤" )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji), "hexmode"), as.hexmode("85e4") )
  skip_if_not_installed("kanjistat.data")
    # output = "all"
    expect_equal( convert_kanji("藤"), list(index=1032, character="藤",
                                                hexmode=as.hexmode("85e4"), kanjivec=kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    # test only the first one also with list
    expect_equal( convert_kanji(1032, "kanjivec"), kanjivec_ref_fuji,
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list(1032), "kanjivec"), kanjivec_ref_fuji,
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji("藤", "kanjivec"), kanjivec_ref_fuji,
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(as.hexmode("85e4"), "kanjivec"), kanjivec_ref_fuji,
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji("0x85e4", "kanjivec"), kanjivec_ref_fuji,
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(kanjivec_ref_fuji, "kanjivec"), kanjivec_ref_fuji,
                  ignore_attr=c("call", "kanjistat_version") )
})


test_that("convert_kanji, multiple entries, simplify", {
  ll <- convert_kanji(c(100,101))
  expect_equal( lapply(ll, \(x){x[1:3]}), list(list(index=c(100), character="高", hexmode=as.hexmode("9ad8")),
                                               list(index=c(101), character="当", hexmode=as.hexmode("5f53"))) )
  #
  expect_equal( convert_kanji(c(100,101), "index"), c(100,101) )
  expect_equal( convert_kanji(c(100,101), "character"), c("高","当") )
  expect_equal( convert_kanji(c(100,101), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list(100,101), "index"), c(100,101) )
  expect_equal( convert_kanji(list(100,101), "character"), c("高","当") )
  expect_equal( convert_kanji(list(100,101), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(c("高","当"), "index"), c(100,101) )
  expect_equal( convert_kanji(c("高","当"), "character"), c("高","当") )
  expect_equal( convert_kanji(c("高","当"), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list("高","当"), "index"), c(100,101) )
  expect_equal( convert_kanji(list("高","当"), "character"), c("高","当") )
  expect_equal( convert_kanji(list("高","当"), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(as.hexmode(c("9ad8","5f53")), "index"), c(100,101) )
  expect_equal( convert_kanji(as.hexmode(c("9ad8","5f53")), "character"), c("高","当") )
  expect_equal( convert_kanji(as.hexmode(c("9ad8","5f53")), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list(as.hexmode("9ad8"), as.hexmode("5f53")), "index"), c(100,101) )
  expect_equal( convert_kanji(list(as.hexmode("9ad8"), as.hexmode("5f53")), "character"), c("高","当") )
  expect_equal( convert_kanji(list(as.hexmode("9ad8"), as.hexmode("5f53")), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(c("0x9ad8", "0x5f53"), "index"), c(100,101) )
  expect_equal( convert_kanji(c("0x9ad8", "0x5f53"), "character"), c("高","当") )
  expect_equal( convert_kanji(c("0x9ad8", "0x5f53"), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list("0x9ad8", "0x5f53"), "index"), c(100,101) )
  expect_equal( convert_kanji(list("0x9ad8", "0x5f53"), "character"), c("高","当") )
  expect_equal( convert_kanji(list("0x9ad8", "0x5f53"), "hexmode"), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "index"), c(1032, 1032) )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "character"), c("藤", "藤") )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "hexmode"), as.hexmode(c("85e4", "85e4")) )
  skip_if_not_installed("kanjistat.data")
    # output = "all"
    sublist <- list(index=1032, character="藤", hexmode=as.hexmode("85e4"), kanjivec=kanjivec_ref_fuji)
    expect_equal( convert_kanji(c("藤","藤")), list(sublist, sublist),
                  ignore_attr=c("call", "kanjistat_version") )
    # test only the second one also with list
    expect_equal( convert_kanji(c(1032,1032), "kanjivec"), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(c("藤","藤"), "kanjivec"), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list("藤","藤"), "kanjivec"), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(as.hexmode(c("85e4","85e4")), "kanjivec"), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(c("0x85e4","0x85e4"), "kanjivec"), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "kanjivec"), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
})


test_that("convert_kanji, single entry, don't simplify", {
  expect_equal( convert_kanji(25, simplify=FALSE)[1:3],
                list(index=25, character="山", hexmode=as.hexmode("5c71")) )
  expect_equal( convert_kanji(list(25), simplify=FALSE)[[1]][1:3], list(index=25, character="山", hexmode=as.hexmode("5c71")) )
  #
  expect_equal( convert_kanji(25)[1:3], list(index=25, character="山", hexmode=as.hexmode("5c71")) )
  expect_equal( convert_kanji(25, "index", simplify=FALSE), 25 )
  expect_equal( convert_kanji(25, "character", simplify=FALSE), "山" )
  expect_equal( convert_kanji(25, "hexmode", simplify=FALSE), as.hexmode("5c71") )
  expect_equal( convert_kanji(list(25), "index", simplify=FALSE), list(25) )
  expect_equal( convert_kanji(list(25), "character", simplify=FALSE), list("山") )
  expect_equal( convert_kanji(list(25), "hexmode", simplify=FALSE), list(as.hexmode("5c71")) )
  expect_equal( convert_kanji("山", "index", simplify=FALSE), 25 )
  expect_equal( convert_kanji("山", "character", simplify=FALSE), "山" )
  expect_equal( convert_kanji("山", "hexmode", simplify=FALSE), as.hexmode("5c71") )
  expect_equal( convert_kanji(list("山"), "index", simplify=FALSE), list(25) )
  expect_equal( convert_kanji(list("山"), "character", simplify=FALSE), list("山") )
  expect_equal( convert_kanji(list("山"), "hexmode", simplify=FALSE), list(as.hexmode("5c71")) )
  expect_equal( convert_kanji(as.hexmode("5c71"), "index", simplify=FALSE), 25 )
  expect_equal( convert_kanji(as.hexmode("5c71"), "character", simplify=FALSE), "山" )
  expect_equal( convert_kanji(as.hexmode("5c71"), "hexmode", simplify=FALSE), as.hexmode("5c71") )
  expect_equal( convert_kanji(list(as.hexmode("5c71")), "index", simplify=FALSE), list(25) )
  expect_equal( convert_kanji(list(as.hexmode("5c71")), "character", simplify=FALSE), list("山") )
  expect_equal( convert_kanji(list(as.hexmode("5c71")), "hexmode", simplify=FALSE), list(as.hexmode("5c71")) )
  expect_equal( convert_kanji("0x5c71", "index", simplify=FALSE), 25 )
  expect_equal( convert_kanji("0x5c71", "character", simplify=FALSE), "山" )
  expect_equal( convert_kanji("0x5c71", "hexmode", simplify=FALSE), as.hexmode("5c71") )
  expect_equal( convert_kanji(list("0x5c71"), "index", simplify=FALSE), list(25) )
  expect_equal( convert_kanji(list("0x5c71"), "character", simplify=FALSE), list("山") )
  expect_equal( convert_kanji(list("0x5c71"), "hexmode", simplify=FALSE), list(as.hexmode("5c71")) )
  expect_equal( convert_kanji(kanjivec_ref_fuji, "index", simplify=FALSE), 1032 )
  expect_equal( convert_kanji(kanjivec_ref_fuji, "character", simplify=FALSE), "藤" )
  expect_equal( convert_kanji(kanjivec_ref_fuji, "hexmode", simplify=FALSE), as.hexmode("85e4") )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji), "index", simplify=FALSE), list(1032) )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji), "character", simplify=FALSE), list("藤") )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji), "hexmode", simplify=FALSE), list(as.hexmode("85e4")) )
  skip_if_not_installed("kanjistat.data")
  # output = "all"
    expect_equal( convert_kanji(list("藤"), simplify=FALSE)[[1]],
                                list(index=1032, character="藤",
                                     hexmode=as.hexmode("85e4"), kanjivec=kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
  # test only the first one also *without* list
    expect_equal( convert_kanji(1032, "kanjivec", simplify=FALSE), kanjivec_ref_fuji,
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list(1032), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list("藤"), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list(as.hexmode("85e4")), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list("0x85e4"), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list(kanjivec_ref_fuji), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
})


test_that("convert_kanji, multiple entries, don't simplify", {
  ll <- convert_kanji(list(100,101), simplify=FALSE)
  expect_equal( lapply(ll, \(x){x[1:3]}), list(list(index=c(100), character="高", hexmode=as.hexmode("9ad8")),
                                               list(index=c(101), character="当", hexmode=as.hexmode("5f53"))) )
  #
  expect_equal( convert_kanji(c(100,101), "index", simplify=FALSE), c(100,101) )
  expect_equal( convert_kanji(c(100,101), "character", simplify=FALSE), c("高","当") )
  expect_equal( convert_kanji(c(100,101), "hexmode", simplify=FALSE), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list(100,101), "index", simplify=FALSE), list(100,101) )
  expect_equal( convert_kanji(list(100,101), "character", simplify=FALSE), list("高","当") )
  expect_equal( convert_kanji(list(100,101), "hexmode", simplify=FALSE), list(as.hexmode("9ad8"), as.hexmode("5f53")) )
  expect_equal( convert_kanji(c("高","当"), "index", simplify=FALSE), c(100,101) )
  expect_equal( convert_kanji(c("高","当"), "character", simplify=FALSE), c("高","当") )
  expect_equal( convert_kanji(c("高","当"), "hexmode", simplify=FALSE), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list("高","当"), "index", simplify=FALSE), list(100,101) )
  expect_equal( convert_kanji(list("高","当"), "character", simplify=FALSE), list("高","当") )
  expect_equal( convert_kanji(list("高","当"), "hexmode", simplify=FALSE), list(as.hexmode("9ad8"), as.hexmode("5f53")) )
  expect_equal( convert_kanji(as.hexmode(c("9ad8","5f53")), "index", simplify=FALSE), c(100,101) )
  expect_equal( convert_kanji(as.hexmode(c("9ad8","5f53")), "character", simplify=FALSE), c("高","当") )
  expect_equal( convert_kanji(as.hexmode(c("9ad8","5f53")), "hexmode", simplify=FALSE), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list(as.hexmode("9ad8"), as.hexmode("5f53")), "index", simplify=FALSE), list(100,101) )
  expect_equal( convert_kanji(list(as.hexmode("9ad8"), as.hexmode("5f53")), "character", simplify=FALSE), list("高","当") )
  expect_equal( convert_kanji(list(as.hexmode("9ad8"), as.hexmode("5f53")), "hexmode", simplify=FALSE), list(as.hexmode("9ad8"), as.hexmode("5f53")) )
  expect_equal( convert_kanji(c("0x9ad8", "0x5f53"), "index", simplify=FALSE), c(100,101) )
  expect_equal( convert_kanji(c("0x9ad8", "0x5f53"), "character", simplify=FALSE), c("高","当") )
  expect_equal( convert_kanji(c("0x9ad8", "0x5f53"), "hexmode", simplify=FALSE), as.hexmode(c("9ad8", "5f53")) )
  expect_equal( convert_kanji(list("0x9ad8", "0x5f53"), "index", simplify=FALSE), list(100,101) )
  expect_equal( convert_kanji(list("0x9ad8", "0x5f53"), "character", simplify=FALSE), list("高","当") )
  expect_equal( convert_kanji(list("0x9ad8", "0x5f53"), "hexmode", simplify=FALSE), list(as.hexmode("9ad8"), as.hexmode("5f53")) )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "index", simplify=FALSE), list(1032, 1032) )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "character", simplify=FALSE), list("藤", "藤") )
  expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "hexmode", simplify=FALSE), list(as.hexmode("85e4"), as.hexmode("85e4")) )
  skip_if_not_installed("kanjistat.data")
    # output = "all"
    sublist <- list(index=1032, character="藤", hexmode=as.hexmode("85e4"), kanjivec=kanjivec_ref_fuji)
    expect_equal( convert_kanji(list("藤","藤"), simplify=FALSE), list(sublist, sublist),
                  ignore_attr=c("call", "kanjistat_version") )
    # test only the second one also *without* list
    expect_equal( convert_kanji(list(1032,1032), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(c("藤","藤"), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list("藤","藤"), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list(as.hexmode("85e4"), as.hexmode("85e4")), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list("0x85e4","0x85e4"), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
    expect_equal( convert_kanji(list(kanjivec_ref_fuji, kanjivec_ref_fuji), "kanjivec", simplify=FALSE), list(kanjivec_ref_fuji, kanjivec_ref_fuji),
                  ignore_attr=c("call", "kanjistat_version") )
})


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
  skip_on_os("windows")
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
