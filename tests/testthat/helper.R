kanjimat_ref_fuji <- readRDS(file.path("_data_and_more_snaps/kanjimat_ref_fuji.rds"))
kanjivec_ref_fuji <- readRDS(file.path("_data_and_more_snaps/kanjivec_ref_fuji.rds"))
treedend_ref_yuumail <- readRDS(file.path("_data_and_more_snaps/kanjivec_flatten_ref_yuumail.rds"))
kanjidic2_listref_ichihagi <- load(file.path("_data_and_more_snaps/kanjidic2_listref_ichihagi.rda"))
# it is a known "issue" of testthat that test_path() does not work in helper files
# https://github.com/r-lib/testthat/issues/1270
# (there are two workarounds there, file.path is the easier one and works (always?))