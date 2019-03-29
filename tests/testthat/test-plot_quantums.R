context("test-plot_quantums")

test_that("plot_quantums returns ggplot", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  csr_envelope <- spatstat::envelope(pattern,
                                     fun = spatstat::pcf, nsim = 9,
                                     funargs = list(divisor = "d",
                                                    correction = "Ripley"))

  plot <- plot_quantums(csr_envelope, ylab = "g(r)")

  expect_is(plot, "ggplot")
  expect_true(plot$labels$y == "g(r)")
  expect_length(plot$layers, n = 4)
})

test_that("plot_quantums returns ggplot (only quantum)", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  csr_envelope <- spatstat::envelope(pattern,
                                     fun = spatstat::pcf, nsim = 9,
                                     funargs = list(divisor = "d",
                                                    correction = "Ripley"))

  plot <- plot_quantums(csr_envelope, ylab = "g(r)", full_fun = FALSE)

  expect_is(plot, "ggplot")
  expect_length(plot$layers, n = 1)
})

test_that("plot_quantums returns ggplot only function", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  csr_envelope <- spatstat::envelope(pattern,
                                     fun = spatstat::pcf, nsim = 9,
                                     funargs = list(divisor = "d",
                                                    correction = "Ripley"))

  plot <- plot_quantums(csr_envelope, ylab = "g(r)", quantum = FALSE)

  expect_is(plot, "ggplot")
  expect_length(plot$layers, n = 3)
})

test_that("plot_quantums creates labels if not provided", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  csr_envelope <- spatstat::envelope(pattern,
                                     fun = spatstat::pcf, nsim = 9,
                                     funargs = list(divisor = "d",
                                                    correction = "Ripley"))

  expect_warning(plot_quantums(csr_envelope,
                              labels = c("clustering", "segregation")),
                 regexp = "Not enough labels provided - using 'clustering', 'randomness' and 'segregation'.")
})

test_that("plot_quantums returns error", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  expect_error(plot_quantums(pattern, ylab = "g(r)",
                            labels = c("clustering", "segregation")),
               regexp = "Please provide envelope or data frame.")
})