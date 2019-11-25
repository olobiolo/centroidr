context('computing centroid distances')

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
  expect_error(cendist("3.456")) # argument is not numeric
  expect_warning(cendist(m1c)) # matrix with one column
})

test_that('centroid distances are calculated properly', {
  expect_equal(cendist(v), -4:4)
  expect_equal(cendist(m), c(1.73205080756888, 0, 1.73205080756888))
  expect_equal(cendist(m1c), -1:1)
  expect_equal(cendist(m1r), 0)
  #expect_identical(cendist(m), cendist(mNA))
})

test_that('columns of a data frame are properly chosen', {
  expect_identical(cendist(d), cendist(m)) # no column specification takes all numeric columns
  expect_identical(cendist(d), cendist(d, one, two, three)) # columns specified as bare names
  expect_identical(cendist(d), cendist(d, 'one', 'two', 'three')) # columns specified by quoted names
  expect_identical(cendist(d), cendist(d, 1, 2, 3)) # columns specified by their indices
})

test_that('column specification errors', {
  expect_error(cendist(d, one, 'two')) # columns specified differently
  expect_error(cendist(d, one, 2)) # columns specified differently
  expect_error(cendist(d, 1, 'two')) # columns specified differently
  expect_error(cendist(d, c('one', 'two'))) # columns specified as vector
})

test_that("grouped data frames are handled correctly", {
  expect_equal(round(cendist(dd), 5), c(20.85665, 19.28730, 17.74824, 12.12436, 19.28730, 27.33130))
  # expect_equal(cendist(dplyr::group_by(dd, group)), data.frame(one = c(1.732051, 8.660254),
  #                                                              two = c(0, 0),
  #                                                              three = c(1.732051, 8.660254),
  #                                                              group = c('A', 'B')))
})
