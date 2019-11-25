context("computing euclidean distances")

test_that("arguments check out", {
  expect_error(euclidean(c('1', '2'), c('4', '5'))) # arguments are not numeric
  expect_error(euclidean(1:4, 1:3)) # arguments of different length
})

test_that("euclidean distances are calculated", {
  expect_equal(1, euclidean(c(1,1), c(1,2)))
  expect_equal(sqrt(2), euclidean(c(1,1), c(2,2)))
  expect_equal(18.1659, round(euclidean(1:10, 10:1), 4))
})
