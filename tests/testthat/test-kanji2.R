test_that("kmatdist and kmatdistmat", {
  # skip_on_cran()  # for time reasons
  a <- fivetrees3[[1]]
  a$matrix <- a$matrix/sum(a$matrix)
  b <- fivetrees3[[2]]
  b$matrix <- b$matrix/sum(b$matrix)
  expect_equal( kmatdist(a, b, p=3, type="balanced"),
                kmatdist(a, b, p=3, C=NULL, type="unbalanced") )
  
  expected <- matrix(c(59.796621052, 106.925146724, 84.093191121, 58.295190935), 2, 2)
  #expected <- matrix(c(59.796621052, 106.925146724, 90.645523346, 78.740449660, 73.544728407, 
  #                     84.093191121,  58.295190935, 90.418273922, 88.735625210, 88.447797318,
  #                     72.245283205,  82.814584050, 65.103037171, 76.489776496, 76.125152008, 
  #                     76.974990999, 93.2878192764, 82.024968236, 67.540180668, 72.286240529,
  #                     75.055336714, 107.156421842, 92.141977374, 74.640717198, 57.702524509), 5, 5)
  expect_equal( kmatdistmat(fivetrees1[1:2], fivetrees2[1:2]), expected )
  
  expect_equal( kmatdistmat(fivetrees2[1:2], p=2, C=0.25, type="unbalanced"),
                kmatdistmat(fivetrees2[1:2], fivetrees2[1:2], p=2, C=0.25, type="unbalanced") )
})


test_that("kanjidist and kanjidistmat", {
  expected <- matrix(c(0.07291957120, 0.12190813256, 0.16364668794, 0.18819748900, 
                       0.20123926422, 0.06585906715), 2, 3)
  expect_equal( kanjidistmat(fivebetas[1:2], fivebetas[3:5]), expected )
})
