test_that("validation table helpers use a descriptive input name", {
  expect_identical(names(formals(validation_cps))[1L], "validation")
  expect_identical(names(formals(validation_perspectives))[1L], "validation")
  expect_identical(names(formals(validation_means))[1L], "validation")
  expect_null(formals(validation_cps)$file)
  expect_null(formals(validation_perspectives)$file)
  expect_null(formals(validation_means)$file)
  expect_identical(formals(validation_cps)$sort_by, "cp-scores")
  expect_identical(formals(validation_means)$sort_by, "cp-scores")
  expect_true("decreasing" %in% names(formals(validation_cps)))
  expect_true("decreasing" %in% names(formals(validation_means)))
})

test_that("validation CSV helper writes to the supplied path", {
  workdir <- tempfile("validation-output-")
  dir.create(workdir)
  old_workdir <- setwd(workdir)
  on.exit(setwd(old_workdir), add = TRUE)
  table <- data.frame(Statement = "A", Probability = 0.75)
  supplied <- file.path(workdir, "validation-table")
  written <- qapproach:::.write_validation_csv(table, supplied)
  expect_true(file.exists(written))
  expect_identical(
    normalizePath(dirname(written), winslash = "/", mustWork = TRUE),
    normalizePath(
      workdir,
      winslash = "/",
      mustWork = TRUE
    )
  )
  imported <- utils::read.csv(written, check.names = FALSE)
  expect_equal(imported, table)
  custom <- file.path(workdir, "other", "table.csv")
  expect_identical(
    qapproach:::.write_validation_csv(table, custom),
    normalizePath(custom)
  )
})

test_that("validation_cps CSV uses plain probability column names", {
  raw_table <- data.frame(
    Statement = "A",
    cps = 0.75,
    `Bootstrap mean` = 0.74,
    Bias = -0.01,
    `Bootstrap SE` = 0.02,
    `Score CI lower` = 0.70,
    `Score CI upper` = 0.78,
    `Median rank` = 1,
    `Rank CI lower` = 1,
    `Rank CI upper` = 2,
    `P(top 1)` = 0.8,
    `P(top 3)` = 0.95,
    `P(bottom 1)` = 0.01,
    check.names = FALSE
  )
  validation <- list(
    `consensus priority score stability` = list(
      `validation table` = raw_table
    )
  )
  workdir <- tempfile("validation-cps-")
  dir.create(workdir)
  old_workdir <- setwd(workdir)
  on.exit(setwd(old_workdir), add = TRUE)

  path <- file.path(workdir, "cps.csv")
  validation_cps(validation, print_table = FALSE, file = path)
  exported_names <- names(utils::read.csv(path, check.names = FALSE))

  expect_true(all(c("P top 1", "P top 3", "P bottom 1") %in% exported_names))
  expect_false(any(grepl("^P\\(", exported_names)))
  expect_true("Weighted z-score" %in% exported_names)
})

test_that("validation tables accept any returned column for sorting", {
  raw_table <- data.frame(
    Statement = c("B", "A"), cps = c(0.4, 0.8),
    `Weighted z-score` = stats::qnorm(c(0.4, 0.8)),
    `Bootstrap mean` = c(0.45, 0.75), Bias = c(0.05, -0.05),
    `Bootstrap SE` = c(0.03, 0.02),
    `Score CI lower` = c(0.3, 0.7), `Score CI upper` = c(0.5, 0.9),
    `Median rank` = c(2, 1), `Rank CI lower` = c(1, 1),
    `Rank CI upper` = c(2, 2), check.names = FALSE
  )
  validation <- list(
    `consensus priority score stability` = list(`validation table` = raw_table)
  )
  default <- validation_cps(validation, print_table = FALSE, file = NULL)
  ascending <- validation_cps(
    validation, print_table = FALSE, file = NULL,
    sort_by = "Bias", decreasing = FALSE
  )
  original <- validation_cps(
    validation, print_table = FALSE, file = NULL, sort_by = NULL
  )
  expect_identical(default$Statement, c("A", "B"))
  expect_identical(ascending$Statement, c("A", "B"))
  expect_identical(original$Statement, c("B", "A"))
  expect_error(
    validation_cps(
      validation, print_table = FALSE, file = NULL, sort_by = "missing"
    ),
    "Available columns"
  )
})

test_that("validation_means identifies every input-mean tie", {
  comparison <- data.frame(
    Statement = c("A", "B", "C"),
    cps = c(0.8, 0.7, 0.4),
    `Normalized input mean` = c(0.7, 0.7, 0.5),
    `cp-scores rank` = c(1, 2, 3),
    `Input-mean rank` = c(1.5, 1.5, 3),
    `Rank change (input means minus cp-scores)` = c(0.5, -0.5, 0),
    check.names = FALSE
  )
  validation <- list(
    `input mean sensitivity` = list(
      `statement comparison table` = comparison,
      `Spearman rank correlation` = 1,
      `Spearman rank-correlation diagnostic` = NULL,
      `top-rank overlap` = NULL,
      interpretation = "Example."
    )
  )

  table <- validation_means(
    validation, print_table = FALSE, sort_by = NULL, file = NULL
  )
  expect_identical(
    table$`Priority comparison`,
    c("Tied under input means", "Tied under input means", "Same rank")
  )
})

test_that("bootstrap diagnostics describe captured messages and warnings", {
  diagnostics <- list(
    known_conditions = data.frame(
      Type = "message",
      Classification = "informational message",
      Condition = "Example diagnostic",
      check.names = FALSE
    ),
    unclassified_warnings = character()
  )

  output <- capture.output(
    qapproach:::.signal_qmboots_diagnostics(diagnostics),
    type = "message"
  )
  expect_match(paste(output, collapse = "\n"), "recognized diagnostic conditions")
  expect_match(
    paste(output, collapse = "\n"),
    "See results\\$diagnostics for details, including alignment fallbacks"
  )
})

test_that("consensus_across_levels reports cumulative transition agreement", {
  result <- function(flagged, loadings, perspective_names, dataset_name) {
    colnames(flagged) <- colnames(loadings) <- perspective_names
    list(
      `dataset name` = dataset_name,
      `Q method results` = list(flagged = flagged, loa = loadings),
      perspectives = structure(
        matrix(0, nrow = length(perspective_names), ncol = 1L),
        dimnames = list(perspective_names, "S1")
      )
    )
  }
  level_one <- function(prefix, counts, undecided = 0L) {
    total <- sum(counts) + undecided
    flagged <- matrix(
      FALSE, total, length(counts),
      dimnames = list(
        paste0(prefix, "_r", seq_len(total)), paste0(prefix, seq_along(counts))
      )
    )
    start <- 1L
    for (j in seq_along(counts)) {
      rows <- seq.int(start, length.out = counts[j])
      flagged[rows, j] <- TRUE
      start <- start + counts[j]
    }
    result(flagged, flagged * 0.5, colnames(flagged), prefix)
  }
  a <- level_one("A", c(5L, 4L, 4L, 4L), 3L)
  b <- level_one("B", c(5L, 3L, 3L, 2L), 7L)

  synthesis_flagged <- matrix(
    FALSE, 8L, 3L,
    dimnames = list(c(paste0("A", 1:4), paste0("B", 1:4)), paste0("s", 1:3))
  )
  synthesis_flagged[cbind(1:7, c(1, 1, 2, 3, 1, 2, 3))] <- TRUE
  synthesis_loadings <- synthesis_flagged * 0.5
  synthesis_flagged[8L, 1L] <- TRUE
  synthesis_loadings[8L, 1L] <- -0.5
  synthesis <- result(
    synthesis_flagged, synthesis_loadings, paste0("s", 1:3), "Synthesis"
  )

  third_flagged <- matrix(
    c(TRUE, TRUE, FALSE), ncol = 1L,
    dimnames = list(paste0("s", 1:3), "t1")
  )
  third <- result(third_flagged, third_flagged * 0.5, "t1", "Third")
  output <- consensus_across_levels(
    list(
      `Level 1` = list(A = a, B = b),
      `Level 2` = synthesis,
      `Level 3` = third
    ),
    print_table = FALSE
  )

  expect_equal(output$raw_input_rankings, 40)
  expect_identical(
    names(output$transitions),
    c(
      "from", "to", "input_rankings", "resulting_perspectives",
      "agreeing", "opposing", "undecided", "effective_agreement",
      "underlying_pool_agreement", "underlying_individual_agreement",
      "underlying_pool_counts", "underlying_individual_by_perspective"
    )
  )
  expect_equal(output$transitions$underlying_pool_agreement, c(0.75, 0.75))
  expect_equal(output$transitions$underlying_individual_agreement, c(0.70, 0.525))
  expect_equal(
    output$transitions$underlying_pool_counts[[1L]]$rankings_agreeing,
    c(17, 13)
  )
  expect_equal(
    output$transitions$underlying_individual_by_perspective[[1L]],
    c(s1 = 14, s2 = 7, s3 = 7)
  )
  expect_equal(output$underlying_individual_by_perspective, c(t1 = 21))
})
