test_that("points_from_svg vs points_from_svg2", {
  # skip_on_cran()  # for time reasons
  # using 藤 = kvec[[1032]]
  strokes <- get_strokes(kanjivec_ref_fuji, simplify=FALSE)  # mind the 1-stroke kanji
  bez <- sapply(as.list(strokes), \(x) {attr(x, "d")})
  allpoints1 <- matrix(0,0,2)
  allpoints2 <- matrix(0,0,2)
  for (st in bez) {
    temp1 <- points_from_svg(st, point_density = 30/109, eqspaced=TRUE, factors = c(1,1))
    temp1 <- rescale_points(temp1, a=c(1,-1)/109, b=c(0,1))
    allpoints1 <- rbind(allpoints1, temp1)
  }
  for (st in bez) {
    temp2 <- points_from_svg2(st, point_density = 30/109, eqspaced=TRUE, factors = c(1,1))
    temp2 <- rescale_points(temp2, a=c(1,-1)/109, b=c(0,1))
    allpoints2 <- rbind(allpoints2, temp2)
  }
  expect_equal(allpoints1, allpoints2)
})
