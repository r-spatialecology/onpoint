context("test-rlabel_local")

pattern <- spatstat.core::runifpoint(n = 250,
                                     win = spatstat.geom::owin(c(0, 100), c(0, 100)))

spatstat.geom::marks(pattern) <- runif(n = 250, min = 10, max = 120)

test_that("rlabel_local returns nsim raster", {

  null_model <- rlabel_local(X = pattern, distance = 25, nsim = 19,
                             drop = FALSE)

  expect_length(null_model, n = 19)
})

test_that("rlabel_local randomizes marks", {

  null_model <- rlabel_local(X = pattern, distance = 25, nsim = 19,
                             drop = FALSE)

  marks_original <- spatstat.geom::marks(pattern)

  check <- any(vapply(X = null_model,
                      FUN = function(x) all(spatstat.geom::marks(x) == marks_original),
                      FUN.VALUE = logical(1)))

  expect_false(check)
})

test_that("rlabel_local return only ppp", {

  null_model <- rlabel_local(X = pattern, distance = 25, nsim = 1,
                             drop = TRUE)

  expect_s3_class(object = null_model, class = "ppp")

})

test_that("rlabel_local returns error", {


  expect_error(rlabel_local(X = pattern, distance = 5, nsim = 19,
                            drop = FALSE),
               regexp = "Not all points have at least one neighbour within the specified distance.")

  expect_error(rlabel_local(X = spatstat.geom::unmark(pattern),
                            distance = 5, nsim = 19,
                            drop = FALSE),
               regexp = "Please provide pattern with numeric marks.")

})
