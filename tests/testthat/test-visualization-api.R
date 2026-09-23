test_that("figure collection accepts analysis and validation separately", {
  expect_true("validation" %in% names(formals(write_figure_collection)))
  expect_true("file" %in% names(formals(write_figure_collection)))
  expect_null(formals(plot_network_two_layered)$file)
  expect_error(
    write_figure_collection(
      tempfile(fileext = ".pdf"),
      result = list(),
      validation = list()
    ),
    "validation must be an object returned by validate"
  )
})

test_that("plot files use exactly the path supplied", {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)

  expect_identical(
    .resolve_plot_file("collection.pdf"),
    "collection.pdf"
  )
  expect_identical(
    .resolve_plot_file(file.path("custom", "collection.pdf")),
    file.path("custom", "collection.pdf")
  )
  expect_identical(
    .resolve_plot_file("custom\\collection.pdf"),
    "custom\\collection.pdf"
  )
})

test_that("individual plot functions expose optional file and dimensions", {
  plotters <- list(
    plot_barplot, plot_heatmap, plot_jitterplot, plot_network, plot_spiderweb
  )
  expect_true(all(vapply(plotters, function(fun) {
    all(c("file", "width", "height") %in% names(formals(fun))) &&
      is.null(formals(fun)$file)
  }, logical(1))))
})

test_that("deterministic palettes preserve the caller's random-number state", {
  skip_if_not_installed("hues")
  set.seed(8675309L)
  seed_before <- .Random.seed
  first <- qapproach:::.general_statement_colors(8L)
  seed_after <- .Random.seed
  second <- qapproach:::.general_statement_colors(8L)

  expect_identical(seed_after, seed_before)
  expect_identical(.Random.seed, seed_before)
  expect_identical(second, first)
})

test_that("network layout seeds are optional and configurable", {
  expect_null(formals(plot_network)$layout_seed)
  expect_null(formals(plot_network_two_layered)$layout_seed)
  expect_null(formals(write_figure_collection)$layout_seed)
})

test_that("plot functions preserve graphics parameters and working directory", {
  result <- list(
    "cp-scores" = c(S1 = 0.25, S2 = 0.75),
    "weighted z-scores" = c(S1 = -0.5, S2 = 0.5)
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit(grDevices::dev.off(), add = TRUE)
  working_directory <- getwd()
  graphics_parameters <- graphics::par(c("mar", "mfrow", "xpd"))

  plot_barplot(
    result,
    labels = c("S1", "S2"),
    statement_colors = c("#336699", "#CC6633")
  )

  expect_identical(getwd(), working_directory)
  expect_equal(graphics::par(c("mar", "mfrow", "xpd")), graphics_parameters)
})

test_that("network description mentions isolated dots only when undecided", {
  result <- function(flagged, loading) {
    list(`Q method results` = list(flagged = flagged, loa = loading))
  }
  decided <- result(matrix(TRUE, 1L, 1L), matrix(0.5, 1L, 1L))
  undecided <- result(matrix(FALSE, 1L, 1L), matrix(0, 1L, 1L))
  description <- function(x) {
    definitions <- .qapproach_figure_definitions(result = x)
    definitions[[which(vapply(
      definitions, function(item) identical(item$id, "network"), logical(1)
    ))]]$description
  }

  expect_false(grepl("Isolated black dots", description(decided), fixed = TRUE))
  expect_true(grepl("Isolated black dots", description(undecided), fixed = TRUE))
})

test_that("jitterplot accepts the object returned by validate", {
  validation <- list(
    "consensus priority score stability" = list(
      "bootstrap cp-scores" = matrix(
        c(0.2, 0.3, 0.4, 0.7, 0.8, 0.9),
        nrow = 2L,
        dimnames = list(c("S1", "S2"), NULL)
      ),
      "cp-scores" = c(S1 = 0.3, S2 = 0.8)
    )
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot_jitterplot(
    validation,
    labels = c("S1", "S2"),
    statement_colors = c("#336699", "#CC6633")
  ))
})

test_that("barplot optionally overlays normalized weighted z-scores", {
  result <- list(
    "cp-scores" = c(S1 = 0.2, S2 = 0.5, S3 = 0.8),
    "weighted z-scores" = c(S1 = -0.8, S2 = 0, S3 = 1.1)
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot_barplot(
    result,
    labels = c("S1", "S2", "S3"),
    statement_colors = c("#336699", "#669933", "#CC6633"),
    show_normalized_weighted_z = TRUE,
    normalized_line_color = "black",
    normalized_line_width = 1.5
  ))
  expect_error(
    plot_barplot(
      result["cp-scores"],
      labels = c("S1", "S2", "S3"),
      statement_colors = c("#336699", "#669933", "#CC6633"),
      show_normalized_weighted_z = TRUE
    ),
    "weighted z-score"
  )
})
