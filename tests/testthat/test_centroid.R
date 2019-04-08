context('computing centroids')

v <- 1:9
m <- matrix(v, 3,3)
m1c <- m[, 1, drop = F]
m1r <- m[1, , drop = F]
mNA <- rbind(m, rep(NA, 3)); for (i in 1:ncol(m)) mNA[, i] <- magic::shift(mNA[, i], i)
d <- as.data.frame(m)
names(d) <- c('one', 'two', 'three')
d$cat <- letters[1:3]



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
})
