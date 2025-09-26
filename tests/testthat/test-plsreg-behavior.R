test_that("plsreg1: basic shape & predictions", {
  skip_if_not_installed("plsdepot")
  set.seed(123)
  n <- 60; p <- 6; h <- 2
  # Correlated predictors
  Z <- matrix(rnorm(n*3), n, 3)
  X <- cbind(Z %*% matrix(c(1,0.5,0.2, 0.5,1,0.3), 3, 2), matrix(rnorm(n*(p-2)), n, p-2))
  colnames(X) <- paste0("x", 1:p)
  beta <- c(1.5, -0.7, rep(0, p-2))
  y <- as.vector(X %*% beta + rnorm(n, sd = 0.3))
  fit <- plsdepot::plsreg1(X, y, comps = h, crosval = FALSE)
  expect_s3_class(fit, "plsreg1")
  expect_true(is.list(fit))
  # required components
  expect_true(all(c("x.scores","x.loads","y.scores","y.loads","std.coefs","reg.coefs","y.pred","resid","R2") %in% names(fit)))
  # dimensions
  expect_equal(nrow(fit$x.scores), n)
  expect_equal(ncol(fit$x.scores), h)
  expect_equal(nrow(fit$x.loads), p)
  expect_equal(ncol(fit$x.loads), h)
  expect_equal(length(fit$y.loads), h)  # vector for univariate y
  expect_equal(length(fit$y.pred), n)
  expect_equal(length(fit$resid), n)
  # coefficients: intercept + p
  expect_equal(length(fit$reg.coefs), p + 1)
  # reconstruct predictions from coefficients
  intercept <- unname(fit$reg.coefs[1])
  slopes <- unname(fit$reg.coefs[-1])
  y_hat2 <- as.vector(drop(intercept + X %*% slopes))
  expect_equal(y_hat2, as.vector(fit$y.pred), tolerance = 1e-6)
  # R-squared sanity
  expect_true(is.numeric(fit$R2))
  expect_true(all(fit$R2 > 0) && all(fit$R2 <= 1))
})

test_that("plsreg2: basic shapes & predictions", {
  skip_if_not_installed("plsdepot")
  set.seed(456)
  n <- 80; p <- 5; q <- 2; h <- 2
  # Predictors
  X <- matrix(rnorm(n*p), n, p); colnames(X) <- paste0("x", 1:p)
  B <- matrix(c(1.2, -0.8, 0.5, rep(0, p-3)), p, q)
  Y <- X %*% B + matrix(rnorm(n*q, sd = 0.4), n, q)
  colnames(Y) <- c("y1","y2")
  fit <- plsdepot::plsreg2(X, Y, comps = h, crosval = FALSE)
  expect_s3_class(fit, "plsreg2")
  expect_true(is.list(fit))
  expect_true(all(c("x.scores","x.loads","y.scores","y.loads","reg.coefs","y.pred","resid","VIP","Q2","Q2cum") %in% names(fit)))
  # dimensions
  expect_equal(nrow(fit$x.scores), n)
  expect_equal(ncol(fit$x.scores), h)
  expect_equal(nrow(fit$y.scores), n)
  expect_equal(ncol(fit$y.scores), h)
  expect_equal(nrow(fit$x.loads), p)
  expect_equal(ncol(fit$x.loads), h)
  expect_equal(nrow(fit$y.loads), q)
  expect_equal(ncol(fit$y.loads), h)
  expect_equal(nrow(fit$y.pred), n)
  expect_equal(ncol(fit$y.pred), q)
  expect_equal(nrow(fit$resid), n)
  expect_equal(ncol(fit$resid), q)
  # VIP dims: p x h
  expect_equal(nrow(fit$VIP), p)
  expect_equal(ncol(fit$VIP), h)
  # reg.coefs: (p + 1) rows by q columns (last row is intercept)
  expect_equal(nrow(fit$reg.coefs), p + 1)
  expect_equal(ncol(fit$reg.coefs), q)
  # reconstruct predictions: X %*% slopes + intercept
  slopes <- fit$reg.coefs[1:p, , drop = FALSE]
  intercept <- fit$reg.coefs[p + 1, , drop = FALSE]
  Yhat2 <- X %*% slopes + matrix(rep(intercept, each = n), nrow = n)
  rownames(Yhat2)<-1:nrow(Yhat2)
  expect_equal(as.matrix(fit$y.pred), as.matrix(Yhat2), tolerance = 1e-6)
})

test_that("plsregda: not available in plsdepot", {
  skip_if_not_installed("plsdepot")
  expect_false("plsregda" %in% getNamespaceExports("plsdepot"))
  skip("plsregda is not exported by plsdepot; nothing to test here.")
})
