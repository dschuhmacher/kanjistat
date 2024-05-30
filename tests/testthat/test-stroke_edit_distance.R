# contains indirectly also tests for values of dstrokedit

test_that("sedist, same as Yencken, type does not matter", {
  skip_if_not_installed("kanjistat.data")
    ind1 <- 384L
    ind2 <- c(93L, 413L, 786L, 895L, 1020L, 1137L, 1282L, 1359L, 1433L, 1642L, 1959L)
    res <- sedist(ind1, ind2)
    expect_equal(res, matrix(dstrokedit[ind1,ind2], 1, length(ind2)), tolerance = 1e-6)
    expected <- structure(c(0.25, 0.25, 0.33333333333333331, 0.30769230769230771, 
                            0.30769230769230771, 0.41666666666666669, 0.41666666666666669, 
                            0.25, 0.41666666666666669, 0.41666666666666669, 0.41666666666666669
                           ), dim = c(1L, 11L))
    expect_equal(res, expected)
    expect_equal(sedist(ind1, ind2, type="before_slash"), expected)
    expect_equal(sedist(ind1, ind2, type="first"), expected)
})


test_that("sedist, different from Yencken, type does matter", {
  ind1 <- 500L
  ind2 <- c(551L, 994L, 1003L, 1244L, 1435L, 1459L, 1494L, 1506L, 1509L, 1525L, 1924L)
  expected0 <- c(0.388889, 0.444444, 0.333333, 0.500000, 0.444444, 0.444444,
                 0.333333, 0.333333, 0.333333, 0.444444, 0.444444)  # dstrokedit
  expected1 <- c(0.44444444444444442, 0.5, 0.33333333333333331, 0.55555555555555558, 
                 0.44444444444444442, 0.5, 0.33333333333333331, 0.33333333333333331, 
                 0.33333333333333331, 0.5, 0.5)  # sedist full and before_slash (not the same in general)
  expected2 <- c(0.27777777777777779, 0.27777777777777779, 0.33333333333333331, 
                 0.33333333333333331, 0.44444444444444442, 0.3888888888888889, 
                 0.33333333333333331, 0.33333333333333331, 0.2857142857142857, 
                 0.5, 0.44444444444444442)  # sedist first
  expect_equal(dstrokedit[ind1,ind2], expected0)
  #
  skip_if_not_installed("kanjistat.data")
    expect_equal( as.vector(sedist(ind1, ind2, type="full")), expected1)
    expect_equal( as.vector(sedist(ind1, ind2, type="before_slash")), expected1)
    expect_equal( as.vector(sedist(ind1, ind2, type="first")), expected2)
})
# first entry checked manually using strokesigraw and historical stroke type data by Lars Yencken
# 観 4,2,3,4,3,4,2b,3a,2b,2b,2b,3,11a,2a,2a,2a,4,19  (500)
# 積 4,2,3,4,1/5,2,3a,2,2,3,11a,2a,2a,2a,4,1 (551)
# 覧 3,2b,3a,11b,2b,3a,2b,4,2,2,3,11a,2a,2a,2a,4,19 (994)
# 奮 2,4,5,4,3,4,2b,3a,2b,2b,2b,3,11a,3a,2a,2a (1003)
# 輩 4,2c,2c,6,3,2b,2b,2b,2,3,11a,2a,2a,2,3 (1244)
# 偵 4,3,3a,2b,3,11a,2a,2a,2a,4,1 (1435)
# 賢 3,2b,3a,11b,2b,3a,2b,15,5,3,11a,2a,2a,2a,4,1 (1459)
# 勧 4,2,3,4,3,4,2b,3a,2b,2b,2b,17,4 (1494)
# 歓 4,2,3,4,3,4,2b,3a,2b,2b,2b,4,7a,4,5 (1506)
# 顧 2,11c,2b,4,4,3,4,2b,3a,2b,2b,2b,2,4,3,11a,2a,2a,2a,4,1 (1509)
# 償 4,3,3a,1,4,1,7b,3,11b,2b,3,11a,2a,2a,2a,4,1 (1525)
# 循 4,4,3,4,4,2,3a,3,11a,2a,2a,2a
