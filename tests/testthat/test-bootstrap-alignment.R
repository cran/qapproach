test_that("bootstrap alignment adapts to the number of perspectives", {
  expect_identical(
    qapproach:::.bootstrap_alignment_method(1L),
    "orthogonal_procrustes"
  )
  expect_identical(qapproach:::.bootstrap_alignment_method(2L), "qindtest")
  expect_identical(qapproach:::.bootstrap_alignment_method(3L), "qindtest")
  expect_identical(
    qapproach:::.bootstrap_alignment_method(4L),
    "orthogonal_procrustes"
  )
})

test_that("bootstrap progress is explicitly suppressible", {
  expect_identical(formals(qaboots)$progress, quote(interactive()))
  expect_identical(
    formals(bootstrap_consensus_priority_scores)$progress,
    quote(interactive())
  )
  expect_identical(formals(qapproach::validate)$progress, quote(interactive()))
  expect_null(formals(qaboots)$seed)
  expect_null(formals(bootstrap_consensus_priority_scores)$seed)
  expect_null(formals(validate)$seed)
  expect_null(formals(qapproach)$distribution_repair_seed)
})

test_that("bootstrap seeds are locally scoped", {
  data("lipset", package = "qmethod", envir = environment())
  fit <- suppressWarnings(suppressMessages(qapproach(
    lipset$ldata,
    nfactors = 4L,
    repair_distributions = FALSE,
    distribution_repair_seed = 42L
  )))

  set.seed(90210L)
  seed_before <- .Random.seed
  invisible(try(suppressWarnings(suppressMessages(qaboots(
    fit, steps = 1L, method = "manual", seed = 42L,
    max_batch_steps = 1L, max_attempt_multiplier = 1L, progress = FALSE
  ))), silent = TRUE))
  expect_identical(.Random.seed, seed_before)

  set.seed(90210L)
  seed_before <- .Random.seed
  invisible(try(suppressWarnings(suppressMessages(qaboots(
    fit, steps = 1L, method = "manual", seed = NULL,
    max_batch_steps = 1L, max_attempt_multiplier = 1L, progress = FALSE
  ))), silent = TRUE))
  expect_false(identical(.Random.seed, seed_before))
})

test_that("qindtest fallback conditions are recognized", {
  condition <- paste(
    "qindtest alignment failed; the batch was repeated with orthogonal",
    "Procrustes alignment: number of items to replace is not a multiple",
    "of replacement length"
  )
  expect_identical(
    qapproach:::.qmethod_condition_classification(condition, "message"),
    "qindtest alignment fallback"
  )
})

test_that("orthogonal Procrustes recovers a rotated target", {
  target <- matrix(
    c(
      0.8, 0.1, -0.3, 0.4,
      -0.2, 0.7, 0.5, -0.1,
      0.3, -0.4, 0.6, 0.2,
      0.1, 0.3, -0.2, 0.9,
      -0.5, 0.2, 0.1, 0.4,
      0.4, -0.6, 0.3, 0.1
    ),
    nrow = 6L,
    ncol = 4L
  )
  decomposition <- qr.Q(qr(matrix(seq_len(16L), nrow = 4L)))
  rotated <- target %*% t(decomposition)
  aligned <- qapproach:::.orthogonal_procrustes_loadings(rotated, target)

  expect_equal(aligned$loadings, target, tolerance = 1e-10)
  expect_lt(aligned$residual, 1e-10)
  expect_true(all(aligned$congruence > 0.999999))
})

test_that("four-perspective bootstraps use package Procrustes alignment", {
  data("lipset", package = "qmethod", envir = environment())
  fit <- suppressWarnings(suppressMessages(qapproach(
    lipset$ldata,
    nfactors = 4L,
    repair_distributions = FALSE,
    distribution_repair_seed = 42L
  )))
  set.seed(42L)
  batch <- qapproach:::.run_aligned_bootstrap_with_diagnostics(
    fit$`Q method results`$dataset,
    fit$`Q method results`,
    nsteps = 3L
  )

  expect_identical(
    batch$alignment_method,
    "orthogonal Procrustes (qapproach implementation)"
  )
  expect_length(batch$result$full.bts.res, 4L)
  expect_identical(
    batch$result$indet.tests$method,
    "orthogonal Procrustes (qapproach implementation)"
  )
  expect_true(is.data.frame(batch$result$indet.tests$summary))
})

test_that("qindtest alignment failures are excluded from valid iterations", {
  data("lipset", package = "qmethod", envir = environment())
  fit <- suppressWarnings(suppressMessages(qapproach(
    lipset$ldata,
    nfactors = 2L,
    repair_distributions = FALSE,
    distribution_repair_seed = 42L
  )))
  set.seed(42L)
  invisible(capture.output(
    batch <- qapproach:::.run_aligned_bootstrap_with_diagnostics(
      fit$`Q method results`$dataset,
      fit$`Q method results`,
      nsteps = 3L
    )
  ))

  invalid <- batch$alignment_diagnostics$invalid_iterations
  expect_true(length(invalid) >= 1L)
  expect_true(all(vapply(
    batch$result$full.bts.res,
    function(factor) all(is.na(factor$zsc[, invalid, drop = FALSE])),
    logical(1)
  )))
})
