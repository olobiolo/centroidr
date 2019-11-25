context('computing centroids')

v <- 1:9
m <- matrix(v, 3,3)
m1c <- m[, 1, drop = F]
m1r <- m[1, , drop = F]
mNA <- rbind(m, rep(NA, 3)); for (i in 1:ncol(m)) mNA[, i] <- magic::shift(mNA[, i], i)
d <- as.data.frame(m)
names(d) <- c('one', 'two', 'three')
d$cat <- letters[1:3]

d1 <- cbind(d, group = 'A')
d2 <- cbind(d, group = 'B')
d2[1:3] <- d2[1:3] * 5
dd <- rbind(d1, d2)

test_that("arguments check out", {
  expect_error(centroid('3.456')) # non-numeric argument
  expect_warning(centroid(numeric(0))) # argument of length 0
  expect_warning(centroid(3.456)) # argument of length 1
  expect_warning(centroid(matrix(0, 0, 0))) # empty matrix
  expect_warning(centroid(m1r)) # matrix with 1 row
  expect_warning(centroid(m1c)) # matrix with 1 column
})


test_that('centroids are calculated properly', {
  expect_equal(centroid(v), 5)
  expect_equal(centroid(m), c(2,5,8))
  expect_equal(centroid(m1c), 2)
  expect_equal(centroid(m1r), c(1,4,7))
  expect_equal(centroid(m), centroid(mNA))
})

test_that('columns of a data frame are properly chosen', {
  expect_equivalent(centroid(d), centroid(m)) # no column specification takes all numeric columns
  expect_identical(centroid(d), centroid(d, one, two, three)) # columns specified as bare names
  expect_identical(centroid(d), centroid(d, 'one', 'two', 'three')) # columns specified by quoted names
  expect_identical(centroid(d), centroid(d, 1, 2, 3)) # columns specified by their indices
})

test_that('column specification errors', {
  expect_error(centroid(d, one, 'two')) # columns specified differently
  expect_error(centroid(d, one, 2)) # columns specified differently
  expect_error(centroid(d, 1, 'two')) # columns specified differently
  expect_error(centroid(d, c('one', 'two'))) # columns specified as vector
  expect_error(centroid(d, 1, 4)) # non-numeric column passed
})

test_that("grouped data frames are handled correctly", {
  expect_equivalent(centroid(dd), c(6,15,24))
  # expect_equivalent(centroid(dplyr::group_by(dd, group)), data.frame(one = c(2, 10),
  #                                                                    two = c(5, 25),
  #                                                                    three = c(8, 40),
  #                                                                    group = c('A', 'B')))
})
