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
    plot_barplot, plot_heatmap, plot_jitterplot, plot_network, plot_spiderweb,
    plot_sdg_cps, plot_hierarchical_levels
  )
  expect_true(all(vapply(plotters, function(fun) {
    all(c("file", "width", "height") %in% names(formals(fun))) &&
      is.null(formals(fun)$file)
  }, logical(1))))
})

test_that("plot_hierarchical_levels draws successive analytical levels", {
  skip_if_not_installed("hues")
  make_result <- function(inputs, perspective, flags, loadings, dataset) {
    flagged <- matrix(flags, ncol = 1L,
                      dimnames = list(inputs, perspective))
    loa <- matrix(loadings, ncol = 1L,
                  dimnames = list(inputs, perspective))
    list(
      `dataset name` = dataset,
      `Q method results` = list(
        flagged = flagged, loa = loa,
        dataset = matrix(0, nrow = 1L, ncol = length(inputs),
                         dimnames = list("Statement", inputs))
      ),
      perspectives = matrix(0, nrow = 1L,
                            dimnames = list(perspective, "Statement"))
    )
  }
  first <- make_result(
    c("ranking_1", "ranking_2"), "first_f1",
    c(TRUE, TRUE), c(0.7, -0.6), "first"
  )
  second <- make_result(
    c("first_f1", "ranking_2"), "second_f1",
    c(TRUE, FALSE), c(0.8, 0), "second"
  )
  levels <- list(level1 = list(first = first),
                 level2 = list(second = second))

  output <- tempfile(fileext = ".pdf")
  written <- plot_hierarchical_levels(
    levels, file = output, agreement = TRUE,
    level_node_sizes = c(12, 16), level_gaps = c(level1 = 0.5),
    analysis_labelled = FALSE
  )
  expect_true(file.exists(written))
  expect_gt(file.info(written)$size, 0)
  expect_error(plot_hierarchical_levels(list(first)), "at least two")
  expect_error(
    plot_hierarchical_levels(levels, level_colors = "red"),
    "one valid color per level"
  )
  network <- plot_hierarchical_levels(levels, network_object = TRUE)
  expect_s3_class(network, "igraph")
  expect_setequal(
    unique(igraph::vertex_attr(network, "type")),
    c("ranking", "analysis")
  )
  expect_true(all(c("input_type", "status") %in%
                    igraph::edge_attr_names(network)))
  expect_true(all(c("first", "second") %in%
                    igraph::vertex_attr(network, "name")))
  expect_true(all(c("ranking_1", "ranking_2", "first", "second") %in%
                    igraph::vertex_attr(network, "label")))
  expect_identical(
    igraph::graph_attr(network, "level_names"),
    c("level1", "level2")
  )
  expect_error(
    plot_hierarchical_levels(levels, network_object = NA),
    "network_object must be TRUE or FALSE"
  )
  expect_error(
    plot_hierarchical_levels(levels, level_gaps = c(level2 = 0.5)),
    "Available names"
  )
  expect_false("layout_seed" %in% names(formals(plot_hierarchical_levels)))

  graph <- igraph::graph_from_data_frame(
    data.frame(from = c("r1", "r2", "a1"),
               to = c("a1", "a1", "a2")),
    directed = TRUE,
    vertices = data.frame(
      name = c("r1", "r2", "a1", "a2"),
      type = c("ranking", "ranking", "analysis", "analysis"),
      level = c(0L, 0L, 1L, 2L)
    )
  )
  hierarchical <- .hierarchical_layout(
    graph,
    igraph::vertex_attr(graph, "level"),
    igraph::vertex_attr(graph, "type")
  )
  expect_equal(hierarchical[1L, 2L], hierarchical[2L, 2L])
  expect_equal(sort(hierarchical[1:2, 1L]), c(-1, 1))
  expect_gt(hierarchical[4L, 2L], hierarchical[3L, 2L])

  hierarchical_with_gap <- .hierarchical_layout(
    graph,
    igraph::vertex_attr(graph, "level"),
    igraph::vertex_attr(graph, "type"),
    level_gaps = c(level1 = 1),
    level_names = c("level1", "level2")
  )
  expect_gt(
    hierarchical_with_gap[4L, 2L] - hierarchical_with_gap[3L, 2L],
    hierarchical[4L, 2L] - hierarchical[3L, 2L]
  )
})

test_that("plot_sdg_cps validates, draws, and writes its SDG comparison", {
  skip_if_not_installed("magick")
  scores <- matrix(
    seq(0.1, 0.9, length.out = 34L), nrow = 2L,
    dimnames = list(c("Analysis A", "Analysis B"), paste0("sdg", 1:17))
  )
  current_device <- grDevices::dev.cur()
  graphics_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(graphics_file)
  expect_silent(plot_sdg_cps(scores))
  grDevices::dev.off()
  expect_identical(grDevices::dev.cur(), current_device)

  output <- tempfile(fileext = ".pdf")
  written <- plot_sdg_cps(
    scores, analysis_labels = c("First", "Second"),
    analysis_lines = TRUE, file = output
  )
  expect_true(file.exists(written))
  expect_gt(file.info(written)$size, 0)
  expect_error(plot_sdg_cps(scores[, -1L]), "exactly 17")
  expect_error(
    plot_sdg_cps(scores, file = tempfile(fileext = ".png")), "pdf"
  )
  side_by_side <- tempfile(fileext = ".pdf")
  expect_silent(plot_sdg_cps(
    scores, style = "side-by-side", file = side_by_side
  ))
  expect_true(file.exists(side_by_side))
  expect_error(plot_sdg_cps(scores, style = "unknown"), "arg")
  expect_error(plot_sdg_cps(scores, analysis_lines = NA), "analysis_lines")
  expect_true(formals(plot_sdg_cps)$analysis_lines)
})

test_that("plot_sdg_diamonds writes one TIFF per perspective", {
  skip_if_not_installed("magick")
  rankings <- rep(-3:3, c(1, 2, 3, 5, 3, 2, 1))
  result <- list(
    perspectives = matrix(
      rankings, nrow = 1L,
      dimnames = list("africa_f1", paste0("sdg", seq_len(17L)))
    )
  )
  output_directory <- tempfile("sdg-diamonds-")
  written <- plot_sdg_diamonds(
    result, directory = output_directory,
    width = 6, height = 4.2, resolution = 72
  )
  expect_length(written, 1L)
  expect_true(file.exists(written))
  expect_gt(file.info(written)$size, 0)
  expect_identical(basename(written), "sdgdiamond_africa_f1.tiff")

  malformed <- result
  malformed$perspectives[1L, 1L] <- 0
  expect_error(
    plot_sdg_diamonds(malformed, directory = tempfile()),
    "rank counts"
  )
  custom_directory <- tempfile("sdg-diamonds-custom-")
  custom <- plot_sdg_diamonds(
    result, directory = custom_directory, filename = "custom.tiff",
    width = 6, height = 4.2, resolution = 72
  )
  expect_identical(basename(custom), "custom_group1.tiff")
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
