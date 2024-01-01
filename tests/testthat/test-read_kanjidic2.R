# system.time( kdic <- read_kanjidic2("../kanjidic2-2023-365.xml") )
# kanjidic2list_ref_ichi <- kdic[[76]]
# kanjidic2list_ref_hagi <- kdic[[13045]]
# save(kanjidic2list_ref_ichi, kanjidic2list_ref_hagi, compress="xz",
#      file="tests/testthat/_data_and_more_snaps/kanjidic2_listref_ichihagi.rda")

test_that("read_kanjidic2 ichi and hagi", {
  # kanjistat_options does not work like base::options yet
  # (we cannot toss the whole old option list at it)
  oldverb <- kanjistat_options(verbose=FALSE)$verbose
  on.exit(kanjistat_options(verbose=oldverb))
  # at least when testing locally my config file is read
  
  expect_warning( kdic <- read_kanjidic2(file.path("_data_and_more_snaps/kanjidic2-2023-365_ichihagi.xml")) )
  # read_kanjidic2 should toss a warning because we do not supply a complete kanjidic2 file
  
  expect_equal(kdic[[1]], kanjidic2list_ref_ichi)
  expect_equal(kdic[[2]], kanjidic2list_ref_hagi)
})
