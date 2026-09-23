test_that("factor distributions contain every statement", {
  for (n in c(3L, 5L, 17L, 22L, 30L)) {
    distribution <- qapproach::distributiondetermination(n)
    expect_length(distribution$ranking, n)
    expect_equal(sum(distribution$distr), n)
    expect_true(all(diff(distribution$ranking) >= 0))
  }
})

test_that("optional scree plots do not write by default", {
  expect_null(formals(qapproach)$screeplot_file)
  expect_false("create_screeplot" %in% names(formals(qapproach)))
  expect_false("figures_path" %in% names(formals(qapproach)))
})

test_that("scree-plot writing preserves graphics parameters and working directory", {
  data("lipset", package = "qmethod", envir = environment())
  output <- tempfile(fileext = ".pdf")
  current_device_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(current_device_file)
  on.exit(grDevices::dev.off(), add = TRUE)
  active_device <- grDevices::dev.cur()
  working_directory <- getwd()
  graphics_parameters <- graphics::par(c("mar", "mfrow", "xpd"))

  suppressWarnings(suppressMessages(qapproach(
    lipset$ldata,
    nfactors = 2,
    screeplot_file = output,
    repair_distributions = FALSE,
    distribution_repair_seed = 42L
  )))

  expect_true(file.exists(output))
  expect_gt(file.info(output)$size, 0)
  expect_identical(grDevices::dev.cur(), active_device)
  expect_identical(getwd(), working_directory)
  expect_equal(graphics::par(c("mar", "mfrow", "xpd")), graphics_parameters)
})

test_that("consensus priority scores use the fixed normal-CDF scale", {
  scores <- qapproach::cpscores(
    matrix(c(-1, 0, 1, -2, 0, 2), nrow = 3), c(2, 1)
  )
  expect_length(scores, 3L)
  expect_true(all(scores > 0 & scores < 1))
  expect_equal(unname(scores), stats::pnorm(c(-4 / 3, 0, 4 / 3)))
})

test_that("implementation helpers are not exported", {
  exported <- getNamespaceExports("qapproach")
  expect_false(any(startsWith(exported, ".")))
})

test_that("automatic factor-selection diagnostics have a public result path", {
  data("lipset", package = "qmethod", envir = environment())
  fit <- suppressWarnings(suppressMessages(qapproach(
    lipset$ldata,
    nfactors = "criteria",
    repair_distributions = FALSE,
    distribution_repair_seed = 42L
  )))

  expect_true("factor_selection" %in% names(fit))
  expect_false("factor selection" %in% names(fit))
  expect_type(fit$diagnostics$factor_selection, "list")
})

test_that("automatic selection permits only the narrow single-agreement exception", {
  north_america <- rbind(
    c(0,0,-1,0,0,-1,1,2,-1,0), c(2,-1,-3,-1,-1,0,-1,3,-1,-1),
    c(-1,0,1,0,0,0,0,2,0,0), c(1,1,1,-2,-2,1,2,1,-1,0),
    c(0,0,0,-3,0,-2,-1,1,0,1), c(2,-3,0,3,1,0,-1,0,0,0),
    c(-1,1,2,1,2,2,0,-1,0,-3), c(-2,-1,-2,-1,-3,-3,1,1,1,-2),
    c(0,-2,-2,-1,-1,-1,0,-2,-2,-2), c(-2,0,2,1,-1,-2,2,-1,0,3),
    c(1,1,1,0,0,2,0,0,1,-1), c(-1,0,-1,2,0,3,0,-1,1,2),
    c(3,2,3,0,2,0,3,0,3,1), c(0,2,-1,1,1,1,-2,0,2,1),
    c(0,3,0,2,1,0,-2,0,2,2), c(1,-1,0,-2,-2,1,1,-2,-2,-1),
    c(-3,-2,0,0,3,-1,-3,-3,-3,0)
  )
  rownames(north_america) <- paste0("sdg", seq_len(nrow(north_america)))
  colnames(north_america) <- paste0("ranking_", seq_len(ncol(north_america)))

  selection <- qapproach:::.optimize_factor_selection(
    north_america, min_load_perc = 0
  )

  expect_equal(selection$nfactors, 3L)
  expect_equal(selection$achieved_load_perc, 0.6)
  expect_true(selection$single_loading_exception_used)
  expect_equal(unname(selection$factor_loadings), c(3, 2, 1))
  expect_equal(
    unname(selection$opposing_loadings[selection$factor_loadings == 1L]), 0
  )
  expect_true(selection$diagnostics$single_loading_exception$used)
})

test_that("not_agreeing preserves statement names for one returned ranking", {
  statement_names <- paste0("sdg", 1:3)
  dataset <- matrix(
    c(-1, 0, 1, 1, 0, -1), nrow = 3,
    dimnames = list(statement_names, c("agreeing_id", "undecided_id"))
  )
  result <- list(`Q method results` = list(
    dataset = dataset,
    flagged = matrix(
      c(TRUE, FALSE), ncol = 1,
      dimnames = list(colnames(dataset), "flag_f1")
    ),
    loa = matrix(
      c(0.8, 0.1), ncol = 1,
      dimnames = list(colnames(dataset), "F1")
    )
  ))

  returned <- not_agreeing(result)
  expect_identical(names(returned), statement_names)
  expect_identical(rownames(returned), "undecided_id")

  returned_with_status <- not_agreeing(result, status = TRUE)
  expect_identical(names(returned_with_status), c("Status", statement_names))
  expect_identical(rownames(returned_with_status), "undecided_id")
})

test_that("analysis summaries include result tables and CSV exports", {
  data("lipset", package = "qmethod", envir = environment())
  fit <- suppressWarnings(suppressMessages(qapproach(
    lipset$ldata,
    nfactors = 2,
    repair_distributions = FALSE,
    distribution_repair_seed = 42L
  )))

  expect_false("Consensus target adjusted" %in% names(fit$summary))
  fit$summary$`Consensus target adjusted` <- FALSE
  original_cp_scores <- fit$`cp-scores`
  export_path <- tempfile("qapproach-summary-", fileext = ".csv")
  result_summary <- qapproach::summary(
    fit, print_table = FALSE, file = export_path
  )
  expect_named(result_summary, c(
    "Analysis summary", "Group perspectives", "Consensus priority scores"
  ))
  expect_false(
    "Consensus target adjusted" %in% names(result_summary$`Analysis summary`)
  )
  expect_match(
    result_summary$`Analysis summary`$`cp-score range`,
    "^\\[[0-9]+\\.[0-9]{2}, [0-9]+\\.[0-9]{2}\\]$"
  )
  expect_equal(
    fit$`cp-scores`, stats::pnorm(fit$`weighted z-scores`)
  )
  expect_equal(nrow(result_summary$`Group perspectives`), 2L)
  expect_equal(nrow(result_summary$`Consensus priority scores`), 1L)
  expect_equal(
    unname(unlist(result_summary$`Consensus priority scores`[, -1L])),
    round(unname(fit$`cp-scores`), 2L)
  )
  expect_identical(
    fit$`cp-scores`,
    original_cp_scores
  )
  expect_true(file.exists(attr(result_summary, "csv_file")))
  csv_lines <- readLines(attr(result_summary, "csv_file"))
  expect_true(any(grepl("Analysis summary", csv_lines, fixed = TRUE)))
  expect_true(any(grepl("Group perspectives", csv_lines, fixed = TRUE)))
  expect_true(any(grepl("Consensus priority scores", csv_lines, fixed = TRUE)))
})
