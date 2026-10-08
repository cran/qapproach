test_that("validation table helpers use a descriptive input name", {
  expect_identical(names(formals(validation_cps))[1L], "validation")
  expect_identical(names(formals(validation_perspectives))[1L], "validation")
  expect_identical(names(formals(validation_means))[1L], "validation")
  expect_null(formals(validation_cps)$file)
  expect_null(formals(validation_perspectives)$file)
  expect_null(formals(validation_means)$file)
  expect_identical(formals(validation_cps)$sort_by, "cp-scores")
  expect_identical(formals(validation_means)$sort_by, "cp-scores")
  expect_false("include_bottom" %in% names(formals(validate)))
  expect_false(formals(validation_cps)$include_bottom)
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
  validation_cps(
    validation, print_table = FALSE, include_bottom = TRUE, file = path
  )
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

test_that("bottom probabilities affect assessments independently of display", {
  scores <- stats::pnorm(seq(1.5, -1.5, length.out = 6L))
  bootstrap <- list(
    `cp-scores` = stats::setNames(scores, paste0("S", seq_along(scores))),
    `weighted z-scores` = stats::qnorm(scores),
    `bootstrap cp-scores` = matrix(scores, nrow = 6L, ncol = 20L)
  )

  hidden <- qapproach:::.consensus_priority_validation_table(
    bootstrap, include_bottom = FALSE
  )
  shown <- qapproach:::.consensus_priority_validation_table(
    bootstrap, include_bottom = TRUE
  )

  expect_identical(
    hidden$`Validation assessment`, shown$`Validation assessment`
  )
  expect_identical(
    hidden$`Validation assessment`[6L], "Robust last priority"
  )
  expect_false(any(grepl("^P\\(bottom ", names(hidden))))
  expect_true(all(
    paste0("P(bottom ", c(1L, 3L, 5L), ")") %in% names(shown)
  ))

  validation <- list(
    `consensus priority score stability` = list(`validation table` = shown)
  )
  wrapper_hidden <- validation_cps(
    validation, print_table = FALSE, sort_by = NULL
  )
  wrapper_shown <- validation_cps(
    validation, print_table = FALSE, sort_by = NULL, include_bottom = TRUE
  )
  expect_false(any(grepl("^P\\(bottom ", names(wrapper_hidden))))
  expect_true(all(
    paste0("P(bottom ", c(1L, 3L, 5L), ")") %in% names(wrapper_shown)
  ))
  expect_identical(
    wrapper_hidden$`Validation assessment`,
    wrapper_shown$`Validation assessment`
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

  printed <- capture.output(validation_means(
    validation, print_table = TRUE, sort_by = NULL, file = NULL
  ))
  expect_false(any(grepl("rank-change direction", printed, fixed = TRUE)))
  expect_false(any(grepl("sensitivity analysis", printed, fixed = TRUE)))
})

test_that("recognized bootstrap diagnostics stay silent unless fallback is used", {
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
  expect_length(output, 0L)

  diagnostics$known_conditions <- data.frame(
    Type = "message",
    Classification = "qindtest alignment fallback",
    Condition = paste(
      "qindtest alignment failed; the batch was repeated with orthogonal",
      "Procrustes alignment"
    ),
    check.names = FALSE
  )
  fallback_output <- capture.output(
    qapproach:::.signal_qmboots_diagnostics(diagnostics),
    type = "message"
  )
  expect_match(paste(fallback_output, collapse = "\n"), "See \\?validate")
})

test_that("unclassified bootstrap warnings include the bug-report URL", {
  diagnostics <- list(
    known_conditions = data.frame(),
    unclassified_warnings = "Unexpected bootstrap condition"
  )
  expect_warning(
    qapproach:::.signal_qmboots_diagnostics(diagnostics),
    "https://github.com/JonasGeschke/qapproach/issues"
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

test_that("agreement_across_levels traces positive paths and re-added rankings", {
  result <- function(dataset_name, input_ids, perspective_ids,
                     positive = list(), negative = list()) {
    flagged <- matrix(
      FALSE, nrow = length(input_ids), ncol = length(perspective_ids),
      dimnames = list(input_ids, perspective_ids)
    )
    loadings <- matrix(0, nrow(flagged), ncol(flagged), dimnames = dimnames(flagged))
    for (item in positive) {
      flagged[item[1L], item[2L]] <- TRUE
      loadings[item[1L], item[2L]] <- 0.7
    }
    for (item in negative) {
      flagged[item[1L], item[2L]] <- TRUE
      loadings[item[1L], item[2L]] <- -0.7
    }
    list(
      `dataset name` = dataset_name,
      `Q method results` = list(
        flagged = flagged,
        loa = loadings,
        dataset = matrix(
          0, nrow = 3L, ncol = length(input_ids),
          dimnames = list(paste0("s", 1:3), input_ids)
        )
      ),
      perspectives = matrix(
        0, nrow = length(perspective_ids), ncol = 3L,
        dimnames = list(perspective_ids, paste0("s", 1:3))
      )
    )
  }

  a <- result(
    "A", c("r1", "r2", "r3"), c("A_f1", "A_f2"),
    positive = list(c("r1", "A_f1")),
    negative = list(c("r2", "A_f1"))
  )
  b <- result(
    "B", "r4", "B_f1", positive = list(c("r4", "B_f1"))
  )
  synthesis <- result(
    "S", c("A_f1", "B_f1", "r2", "r3", "r5"), c("s_f1", "s_f2"),
    positive = list(c("A_f1", "s_f1"), c("r2", "s_f2"), c("r5", "s_f2")),
    negative = list(c("B_f1", "s_f1"))
  )
  global <- result(
    "G", c("s_f1", "s_f2"), "g_f1",
    positive = list(c("s_f1", "g_f1"), c("s_f2", "g_f1"))
  )

  traced <- agreement_across_levels(
    list(
      level1 = list(A = a, B = b),
      level2 = list(S = synthesis),
      level3 = list(G = global)
    ),
    print_table = FALSE
  )

  expect_type(traced, "list")
  expect_identical(
    names(traced),
    c(
      "paths", "underlying_agreement_counts", "terminal_path_summary",
      "rankings_not_agreeing"
    )
  )
  paths <- traced$paths
  expect_s3_class(paths, "data.frame")
  expect_identical(paths$`Input ranking`, c("r1", "r2", "r3", "r4", "r5"))
  expect_identical(
    paths$`Agreement path`,
    c(
      "A_f1 > s_f1 > g_f1",
      "s_f2 > g_f1",
      NA_character_,
      "B_f1",
      "s_f2 > g_f1"
    )
  )
  expect_identical(
    paths$`level2 status`,
    c("agreeing", "agreeing", "undecided", "opposing", "agreeing")
  )
  expect_identical(paths$`Source analysis`, c("A", "A", "A", "B", "S"))
  expect_true(is.na(paths$`level1 status`[paths$`Input ranking` == "r5"]))
  expect_true(all(c(
    "level1 analysis", "level1 status", "level1 perspective",
    "level2 analysis", "level2 status", "level2 perspective",
    "level3 analysis", "level3 status", "level3 perspective",
    "Agreement path"
  ) %in% names(paths)))
  expect_identical(
    traced$underlying_agreement_counts$`Underlying agreeing rankings`,
    c(1L, 0L, 1L, 1L, 2L, 3L)
  )
  expect_equal(sum(traced$terminal_path_summary$`Input rankings`), 5)
  expect_identical(
    traced$terminal_path_summary$`Terminal perspective`,
    c("g_f1", "No agreement", "B_f1")
  )
  expect_identical(traced$rankings_not_agreeing$count, 1L)
  expect_identical(traced$rankings_not_agreeing$rankings, "r3")
  expect_s3_class(traced$rankings_not_agreeing$ranking_values, "data.frame")
  expect_identical(rownames(traced$rankings_not_agreeing$ranking_values), "r3")
  expect_identical(
    names(traced$rankings_not_agreeing$ranking_values), paste0("s", 1:3)
  )
})
