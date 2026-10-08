############################################################################
### Visualization functions for the Q approach to consensus building     ###
############################################################################

# Statement-set presets ----------------------------------------------------
# Add future standardized sets here. Plotting code remains independent of
# the preset and receives only the prepared labels and colors.

.qapproach_visualization_presets <- function() {
  list(
    sdg = list(
      id = "sdg",
      name = "Sustainable Development Goals",
      statement_count = 17L,
      labels = paste("SDG", seq_len(17L)),
      colors = c(
        "#E5243B", "#DDA63A", "#4C9F38", "#C5192D", "#FF3A21",
        "#26BDE2", "#FCC30B", "#A21942", "#FD6925", "#DD1367",
        "#FD9D24", "#BF8B2E", "#3F7E44", "#0A97D9", "#56C02B",
        "#00689D", "#19486A"
      )
    ),
    `tca-actions` = list(
      id = "tca-actions",
      name = "IPBES Transformative Change Assessment actions",
      statement_count = 22L,
      labels = c(
        "1.1", "1.2", "1.3", "1.4", "1.5",
        "2.1", "2.2", "2.3", "2.4",
        "3.1", "3.2", "3.3", "3.4",
        "4.1", "4.2", "4.3", "4.4",
        "5.1", "5.2", "5.3", "5.4", "5.5"
      ),
      colors = c(
        rep("#b9cf84", 5L),
        rep("#ffb073", 4L),
        rep("#007058", 4L),
        rep("#915400", 4L),
        rep("#aabeff", 5L)
      )
    ),
    `tca-strategies` = list(
      id = "tca-strategies",
      name = "IPBES Transformative Change Assessment strategies",
      statement_count = 5L,
      labels = c(
        "1. Conserve and",
        "2. Drive systemic",
        "3. Transform economic",
        "4. Transform governance",
        "5. Shift societal"
      ),
      colors = c(
        "#b9cf84","#ffb073","#007058","#915400","#aabeff"
      )
    )
  )
}

.qapproach_visualization_preset <- function(name) {
  if (is.null(name) || !length(name) || !nzchar(trimws(name))) return(NULL)
  name <- tolower(trimws(name))
  presets <- .qapproach_visualization_presets()
  if (!name %in% names(presets)) {
    stop(
      "Unknown visualization preset '", name, "'. Available presets: ",
      paste(names(presets), collapse = ", "), "."
    )
  }
  presets[[name]]
}

.prepare_visualization_options <- function(
    statement_count,
    preset = NULL,
    default_labels = paste("Statement", seq_len(statement_count)),
    statement_labels = NULL,
    statement_colors = NULL) {
  if (length(statement_count) != 1L || is.na(statement_count) ||
      statement_count < 1L || statement_count != as.integer(statement_count)) {
    stop("'statement_count' must be a positive whole number.")
  }
  statement_count <- as.integer(statement_count)
  preset_definition <- .qapproach_visualization_preset(preset)

  if (!is.null(preset_definition) &&
      preset_definition$statement_count != statement_count) {
    stop(
      "The ", preset_definition$name, " preset requires ",
      preset_definition$statement_count, " statements, but the CSV contains ",
      statement_count, "."
    )
  }
  if (length(default_labels) != statement_count) {
    stop("'default_labels' must contain one label per statement.")
  }
  if (!is.null(statement_labels) &&
      length(statement_labels) != statement_count) {
    stop("'statement_labels' must contain one label per statement.")
  }
  if (!is.null(statement_colors) &&
      length(statement_colors) != statement_count) {
    stop("'statement_colors' must contain one color per statement.")
  }

  labels <- if (!is.null(statement_labels)) {
    statement_labels
  } else if (!is.null(preset_definition)) {
    preset_definition$labels
  } else {
    default_labels
  }
  colors <- if (!is.null(statement_colors)) {
    statement_colors
  } else if (!is.null(preset_definition)) {
    preset_definition$colors
  } else {
    NULL
  }

  list(
    preset = if (is.null(preset_definition)) NULL else preset_definition$id,
    statement_labels = labels,
    statement_colors = colors
  )
}

.resolve_visualization_arguments <- function(
    result, labels = NULL, statement_colors = NULL, preset = NULL) {
  statement_count <- length(result$`cp-scores`)
  if (!statement_count) {
    stop("result must contain consensus priority scores.")
  }
  default_labels <- names(result$`cp-scores`)
  if (is.null(default_labels) || length(default_labels) != statement_count ||
      any(!nzchar(default_labels))) {
    default_labels <- paste("Statement", seq_len(statement_count))
  }
  options <- .prepare_visualization_options(
    statement_count = statement_count,
    preset = preset,
    default_labels = default_labels,
    statement_labels = labels,
    statement_colors = statement_colors
  )
  list(
    labels = options$statement_labels,
    statement_colors = options$statement_colors,
    preset = options$preset
  )
}

# Shared formatting and palette helpers -----------------------------------

.general_statement_colors <- function(n) {
  if (!requireNamespace("hues", quietly = TRUE)) {
    stop("The R package 'hues' is required for the general statement palette.")
  }
  if (length(n) != 1L || is.na(n) || n < 1L || n != as.integer(n)) {
    stop("'n' must be a positive whole number.")
  }

  withr::local_seed(42L)
  hues::iwanthue(as.integer(n))
}

.resolve_statement_colors <- function(statement_count, statement_colors = NULL) {
  if (is.null(statement_colors)) {
    return(.general_statement_colors(statement_count))
  }
  if (length(statement_colors) != statement_count) {
    stop("Provide exactly one statement color per statement.")
  }
  statement_colors
}

.format_barplot_scores <- function(scores, max_digits = 15L) {
  scores <- as.numeric(scores)
  tolerance <- sqrt(.Machine$double.eps)
  endpoints <- abs(scores) < tolerance | abs(scores - 1) < tolerance
  digits <- rep(2L, length(scores))

  repeat {
    labels <- sprintf(paste0("%.", digits, "f"), scores)
    labels[abs(scores) < tolerance] <- "0"
    labels[abs(scores - 1) < tolerance] <- "1"
    collision_groups <- split(which(!endpoints), labels[!endpoints])
    collisions <- unlist(
      collision_groups[
        vapply(
          collision_groups,
          function(indices) length(unique(scores[indices])) > 1L,
          logical(1)
        )
      ],
      use.names = FALSE
    )
    if (!length(collisions) || all(digits[collisions] >= max_digits)) {
      return(labels)
    }
    digits[collisions] <- pmin(digits[collisions] + 1L, max_digits)
  }
}

.heatmap_ranking_colors <- function(values) {
  values <- sort(unique(as.numeric(values)))
  if (length(values) == 1L) return("#5B9BD5")
  c(
    grDevices::colorRampPalette(c("#FFFFFF", "#70AD47"))(
      length(values) - 1L
    ),
    "#5B9BD5"
  )
}

# Four reusable app/manual figures ----------------------------------------

.resolve_plot_file <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !nzchar(trimws(file))) {
    stop("file must be one non-empty path.")
  }
  path <- file
  parent <- dirname(path)
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE)
  path
}

.write_plot_file <- function(file, width, height, plot_function) {
  path <- .resolve_plot_file(file)
  extension <- tolower(tools::file_ext(path))
  if (extension == "pdf") {
    grDevices::pdf(path, width = width, height = height, bg = "white")
  } else if (extension == "svg") {
    grDevices::svg(path, width = width, height = height, bg = "white")
  } else if (extension == "png") {
    grDevices::png(path, width = width, height = height, units = "in",
                   res = 300, bg = "white")
  } else if (extension %in% c("tif", "tiff")) {
    grDevices::tiff(path, width = width, height = height, units = "in",
                    res = 300, compression = "lzw", bg = "white")
  } else {
    stop("file must have a .pdf, .svg, .png, .tif, or .tiff extension.")
  }
  device_open <- TRUE
  on.exit(if (device_open) grDevices::dev.off(), add = TRUE)
  plot_function()
  grDevices::dev.off()
  device_open <- FALSE
  invisible(normalizePath(path, mustWork = TRUE))
}

plot_barplot <- function(result, labels = NULL,
                         statement_colors = NULL,
                         network_labelled = FALSE,
                         preset = NULL,
                         show_normalized_weighted_z = FALSE,
                         normalized_line_color = "black",
                         normalized_line_width = 1.5,
                         file = NULL, width = 11.2, height = 6.4) {
  if (!is.null(file)) {
    return(.write_plot_file(file, width, height, function() {
      plot_barplot(
        result, labels, statement_colors, network_labelled, preset,
        show_normalized_weighted_z, normalized_line_color,
        normalized_line_width, file = NULL
      )
    }))
  }
  scores <- result$`cp-scores`
  nstat <- length(scores)
  if (!is.logical(show_normalized_weighted_z) ||
      length(show_normalized_weighted_z) != 1L ||
      is.na(show_normalized_weighted_z)) {
    stop("show_normalized_weighted_z must be TRUE or FALSE.")
  }
  if (isTRUE(show_normalized_weighted_z)) {
    weighted_z <- result$`weighted z-scores`
    if (is.null(weighted_z) || length(weighted_z) != nstat ||
        any(!is.finite(weighted_z))) {
      stop(
        "The result must contain one finite weighted z-score per cp-score to ",
        "show the normalized weighted-z-score overlay."
      )
    }
    weighted_range <- range(weighted_z)
    if (diff(weighted_range) <= 0) {
      stop("The weighted z-scores must vary to show the normalized overlay.")
    }
    if (!is.character(normalized_line_color) ||
        length(normalized_line_color) != 1L ||
        is.na(normalized_line_color)) {
      stop("normalized_line_color must be one color value.")
    }
    tryCatch(
      grDevices::col2rgb(normalized_line_color),
      error = function(error) stop("normalized_line_color must be a valid color.")
    )
    if (!is.numeric(normalized_line_width) ||
        length(normalized_line_width) != 1L ||
        !is.finite(normalized_line_width) || normalized_line_width <= 0) {
      stop("normalized_line_width must be one positive finite number.")
    }
  }
  options <- .resolve_visualization_arguments(
    result, labels, statement_colors, preset
  )
  labels <- options$labels
  statement_colors <- options$statement_colors
  statement_colors <- .resolve_statement_colors(nstat, statement_colors)
  order_index <- order(scores)
  scores <- scores[order_index]
  labels <- labels[order_index]
  colors <- statement_colors[order_index]
  if (isTRUE(show_normalized_weighted_z)) {
    weighted_z <- weighted_z[order_index]
    score_range <- range(scores)
    normalized_weighted_z <- score_range[1L] +
      (weighted_z - min(weighted_z)) /
      (max(weighted_z) - min(weighted_z)) * diff(score_range)
  }
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(
    mar = c(4, max(7, min(18, max(nchar(labels)) * 0.55)), 3, 2),
    xpd = if (isTRUE(show_normalized_weighted_z)) NA else FALSE
  )
  bars <- graphics::barplot(
    scores, horiz = TRUE, las = 1, names.arg = labels, col = colors,
    border = NA, xlim = c(0, 1.08), xlab = "",
    main = "", xaxt = "n"
  )
  graphics::axis(
    1, at = c(0, 0.5, 1),
    labels = c("Lower priority", "0.5", "Higher priority")
  )
  if (isTRUE(show_normalized_weighted_z)) {
    bar_half_height <- if (length(bars) > 1L) {
      min(diff(bars)) / 2.4
    } else {
      0.5
    }
    stair_x <- normalized_weighted_z[1L]
    stair_y <- bars[1L] - bar_half_height
    if (length(bars) > 1L) {
      for (i in seq_len(length(bars) - 1L)) {
        midpoint <- mean(bars[c(i, i + 1L)])
        stair_x <- c(
          stair_x, normalized_weighted_z[i], normalized_weighted_z[i + 1L]
        )
        stair_y <- c(stair_y, midpoint, midpoint)
      }
    }
    stair_x <- c(stair_x, normalized_weighted_z[length(normalized_weighted_z)])
    stair_y <- c(stair_y, bars[length(bars)] + bar_half_height)
    graphics::lines(
      stair_x, stair_y, col = "white",
      lwd = normalized_line_width + 2.5,
      lend = "butt", ljoin = "mitre"
    )
    graphics::lines(
      stair_x, stair_y, col = normalized_line_color,
      lwd = normalized_line_width,
      lend = "butt", ljoin = "mitre"
    )
    graphics::legend(
      x = 1, y = bars[1L], xjust = 1, yjust = 0.5, xpd = NA,
      legend = "Weighted z-scores rescaled to the observed cp-score range",
      col = normalized_line_color, lwd = normalized_line_width,
      bty = "n", horiz = TRUE, cex = 0.8
    )
  }
  graphics::text(
    pmin(scores + 0.02, 1.03), bars,
    labels = .format_barplot_scores(scores),
    adj = 0, cex = 0.8
  )
}

plot_spiderweb <- function(result, labels = NULL,
                           statement_colors = NULL,
                           network_labelled = FALSE,
                           preset = NULL, file = NULL,
                           width = 8.3, height = 8.3) {
  if (!is.null(file)) {
    return(.write_plot_file(file, width, height, function() {
      plot_spiderweb(
        result, labels, statement_colors, network_labelled, preset,
        file = NULL
      )
    }))
  }
  zscores <- as.matrix(t(result$`Q method results`$zsc))
  nstat <- ncol(zscores)
  nfactors <- nrow(zscores)
  options <- .resolve_visualization_arguments(
    result, labels, statement_colors, preset
  )
  labels <- options$labels
  radar_limit <- max(3, ceiling(max(abs(zscores), na.rm = TRUE)))
  radar_order <- if (nstat == 1L) 1L else c(1L, rev(seq.int(2L, nstat)))
  spider <- rbind(rep(radar_limit, nstat), rep(-radar_limit, nstat), zscores)
  colnames(spider) <- labels
  colors <- grDevices::hcl.colors(nfactors, "Set 2")
  eigenvalues <- result$`Q method results`$f_char$characteristics$eigenvals
  line_widths <- pmax(0.8, 3 * eigenvalues / max(eigenvalues))
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(0.5, 0.5, 0.5, 0.5), xpd = NA)
  fmsb::radarchart(
    as.data.frame(spider[, radar_order, drop = FALSE]),
    axistype = 1, seg = 6, vlcex = max(0.7, min(1.45, 20 / nstat)),
    title = "", pcol = colors, plwd = line_widths,
    cglcol = "grey80", cglty = 1, axislabcol = "grey45",
    caxislabels = seq(-radar_limit, radar_limit, length.out = 7),
    cglwd = 0.8
  )
  graphics::legend(
    x = 1.18, y = 1.15, xjust = 0, yjust = 1,
    legend = paste("Perspective", seq_len(nfactors)),
    bty = "n", pch = 20, col = colors, cex = 0.9, xpd = NA
  )
}

#' Plot cp-scores across analyses using SDG icons
#'
#' Draws one horizontal row per analysis and positions the 17 official United
#' Nations Sustainable Development Goal icons at their respective cp-scores.
#' Icon size increases linearly with the cp-score. The first matrix or data-frame
#' row is displayed at the top. When `file` is `NULL`, the figure is drawn on the
#' current graphics device; otherwise, a PDF is written to the exact supplied
#' path.
#'
#' @param x A numeric matrix or data frame with analyses in rows and SDGs 1--17
#'   in columns, in numerical SDG order. Values must be finite cp-scores between
#'   zero and one.
#' @param analysis_labels Optional character vector containing one displayed
#'   label per analysis. By default, row names are used; if these are absent,
#'   sequential analysis labels are generated.
#' @param icon_size_range Two positive numbers giving the minimum and maximum
#'   icon diameter in analysis-row units. Sizes are interpolated linearly from
#'   cp-scores zero to one.
#' @param style Either `"cpscores"`, which positions icons at their cp-scores,
#'   or `"side-by-side"`, which orders them from lower to higher priority on an
#'   evenly spaced grid centered around 0.5. Icon size represents the cp-score
#'   in both styles.
#' @param analysis_lines Logical; whether to draw horizontal row guides
#'   restricted to the priority axis from zero to one. Defaults to `TRUE`.
#' @param file `NULL` to draw on the current graphics device, or a path ending
#'   in `.pdf` to write the figure.
#' @param width,height PDF dimensions in inches when `file` is supplied.
#' @return Invisibly returns the normalized PDF path when `file` is supplied;
#'   otherwise invisibly returns `NULL` after drawing the figure.
#' @export
plot_sdg_cps <- function(x, analysis_labels = NULL,
                         icon_size_range = c(0.18, 0.62),
                         style = c("cpscores", "side-by-side"),
                         analysis_lines = TRUE,
                         file = NULL, width = 11.2, height = 8.3) {
  style <- match.arg(style)
  if (is.data.frame(x)) {
    numeric_columns <- vapply(x, is.numeric, logical(1))
    if (!all(numeric_columns)) {
      stop("x must contain only numeric cp-score columns.")
    }
  }
  scores <- as.matrix(x)
  if (!is.numeric(scores) || length(dim(scores)) != 2L) {
    stop("x must be a numeric matrix or data frame.")
  }
  if (nrow(scores) < 1L) {
    stop("x must contain at least one analysis row.")
  }
  if (ncol(scores) != 17L) {
    stop("x must contain exactly 17 SDG columns in numerical SDG order.")
  }
  if (any(!is.finite(scores)) || any(scores < 0 | scores > 1)) {
    stop("All cp-scores in x must be finite values between zero and one.")
  }
  if (!is.numeric(icon_size_range) || length(icon_size_range) != 2L ||
      any(!is.finite(icon_size_range)) || any(icon_size_range <= 0) ||
      icon_size_range[1L] > icon_size_range[2L]) {
    stop("icon_size_range must contain two ascending positive finite numbers.")
  }
  if (!is.logical(analysis_lines) || length(analysis_lines) != 1L ||
      is.na(analysis_lines)) {
    stop("analysis_lines must be TRUE or FALSE.")
  }
  default_labels <- rownames(scores)
  if (is.null(default_labels) || length(default_labels) != nrow(scores) ||
      any(!nzchar(default_labels))) {
    default_labels <- paste("Analysis", seq_len(nrow(scores)))
  }
  if (is.null(analysis_labels)) {
    analysis_labels <- default_labels
  } else if (!is.character(analysis_labels) ||
             length(analysis_labels) != nrow(scores) ||
             any(is.na(analysis_labels)) || any(!nzchar(analysis_labels))) {
    stop("analysis_labels must contain one non-empty label per analysis row.")
  }
  if (!is.null(file)) {
    if (tolower(tools::file_ext(file)) != "pdf") {
      stop("file must have a .pdf extension.")
    }
    return(.write_plot_file(file, width, height, function() {
      plot_sdg_cps(
        scores, analysis_labels = analysis_labels,
        icon_size_range = icon_size_range,
        style = style, analysis_lines = analysis_lines,
        file = NULL, width = width, height = height
      )
    }))
  }
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The R package 'magick' is required to draw the SDG icons.")
  }
  icon_directory <- system.file(
    "extdata", "sdg-icons", package = "qapproach"
  )
  icon_paths <- file.path(
    icon_directory, sprintf("E-WEB-Goal-%02d.png", seq_len(17L))
  )
  if (!nzchar(icon_directory) || any(!file.exists(icon_paths))) {
    stop("The bundled SDG icon files could not be found.")
  }
  icons <- lapply(icon_paths, function(path) {
    grDevices::as.raster(magick::image_resize(
      magick::image_read(path), "200x200"
    ))
  })

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  left_margin <- max(7, min(22, max(nchar(analysis_labels)) * 0.58))
  graphics::par(mar = c(4.5, left_margin, 1.2, 4.5), xpd = NA)
  analysis_count <- nrow(scores)
  y_positions <- rev(seq_len(analysis_count))
  graphics::plot(
    NA_real_, NA_real_,
    xlim = c(0, 1), ylim = c(0.5, analysis_count + 0.5),
    xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
    xlab = "cp-scores", ylab = "", bty = "n"
  )
  if (isTRUE(analysis_lines)) {
    graphics::segments(
      x0 = 0, y0 = y_positions, x1 = 1, y1 = y_positions,
      col = "#E6E6E6", lwd = 0.8, xpd = FALSE
    )
  }
  if (identical(style, "side-by-side")) {
    graphics::axis(
      1L, at = c(0, 1), labels = c("Lower priority", "Higher priority")
    )
  } else {
    graphics::axis(
      1L, at = c(0, 0.5, 1),
      labels = c("Lower priority", "0.5", "Higher priority")
    )
  }
  graphics::axis(
    2L, at = y_positions, labels = analysis_labels,
    las = 1L, tick = FALSE
  )

  user_coordinates <- graphics::par("usr")
  plot_inches <- graphics::par("pin")
  x_per_y <-
    (plot_inches[2L] / diff(user_coordinates[3:4])) /
    (plot_inches[1L] / diff(user_coordinates[1:2]))
  side_by_side_scale <- 1
  if (identical(style, "side-by-side")) {
    grid_spacing <- 0.9 / 16
    largest_requested_y <- icon_size_range[1L] + max(scores) *
      diff(icon_size_range)
    largest_requested_x <- largest_requested_y * x_per_y
    side_by_side_scale <- min(
      1, grid_spacing * 0.88 / largest_requested_x
    )
  }
  for (analysis_index in seq_len(analysis_count)) {
    row_scores <- scores[analysis_index, ]
    draw_order <- order(row_scores)
    x_positions <- if (identical(style, "side-by-side")) {
      positions <- seq(0.05, 0.95, length.out = 17L)
      output <- numeric(17L)
      output[draw_order] <- positions
      output
    } else {
      row_scores
    }
    for (sdg_index in draw_order) {
      diameter_y <- icon_size_range[1L] + row_scores[sdg_index] *
        diff(icon_size_range)
      diameter_x <- diameter_y * x_per_y
      if (identical(style, "side-by-side")) {
        diameter_x <- diameter_x * side_by_side_scale
        diameter_y <- diameter_y * side_by_side_scale
      }
      graphics::rasterImage(
        icons[[sdg_index]],
        x_positions[sdg_index] - diameter_x / 2,
        y_positions[analysis_index] - diameter_y / 2,
        x_positions[sdg_index] + diameter_x / 2,
        y_positions[analysis_index] + diameter_y / 2,
        interpolate = TRUE
      )
    }
  }
  invisible(NULL)
}

#' Export SDG-priority diamonds for all group perspectives
#'
#' Creates one TIFF figure per group perspective in a Q approach result. Each
#' figure arranges the 17 Sustainable Development Goal icons in the perspective's
#' required Q-sort distribution, from lower to higher priority. Tied statements
#' retain their order in the perspective table.
#'
#' @param results An object returned by [qapproach()] containing a numeric
#'   `perspectives` table with 17 SDG columns and the standard ranking
#'   distribution from -3 to 3.
#' @param directory Output directory. The directory is created when necessary.
#'   This argument is required because the function writes one TIFF per
#'   perspective.
#' @param filename Optional shared basename for the generated files. `NULL`
#'   creates one filename directly from each group-perspective name, prefixed
#'   with `sdgdiamond_`. A `.tif` or `.tiff` extension supplied with a custom
#'   basename is removed before the perspective suffix and final `.tiff`
#'   extension are added.
#' @param width,height TIFF dimensions in centimetres.
#' @param resolution TIFF resolution in dots per inch.
#' @return Invisibly returns the normalized paths of the written TIFF files.
#' @export
plot_sdg_diamonds <- function(results, directory, filename = NULL,
                              width = 30, height = 21, resolution = 300) {
  if (!is.list(results) || is.null(results$perspectives)) {
    stop(
      "results must be a Q approach result containing perspectives; file ",
      "paths are not accepted."
    )
  }
  if (!is.character(directory) || length(directory) != 1L ||
      is.na(directory) || !nzchar(trimws(directory))) {
    stop("directory must be one non-empty output-directory path.")
  }
  if (!is.null(filename)) {
    if (!is.character(filename) || length(filename) != 1L ||
        is.na(filename) || !nzchar(trimws(filename))) {
      stop("filename must be NULL or one non-empty basename.")
    }
    if (!identical(basename(filename), filename)) {
      stop("filename must be a basename without a directory path.")
    }
  }
  for (argument in c("width", "height", "resolution")) {
    value <- get(argument, inherits = FALSE)
    if (!is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value <= 0) {
      stop(argument, " must be one positive finite number.")
    }
  }

  perspectives <- as.matrix(results$perspectives)
  expected_ranks <- rep(-3:3, c(1, 2, 3, 5, 3, 2, 1))
  if (!is.numeric(perspectives) || nrow(perspectives) < 1L ||
      ncol(perspectives) != 17L || any(!is.finite(perspectives))) {
    stop(
      "The perspectives table must be numeric, contain 17 columns and at ",
      "least one row, and contain only finite values."
    )
  }
  valid_distributions <- apply(perspectives, 1L, function(values) {
    identical(sort(as.numeric(values)), as.numeric(expected_ranks))
  })
  if (!all(valid_distributions)) {
    stop(
      "Each perspective must contain rank counts 1, 2, 3, 5, 3, 2, 1 ",
      "for ranks -3 through 3."
    )
  }
  statement_ids <- colnames(perspectives)
  valid_id <- "^(sdg|stat)([1-9]|1[0-7])$"
  if (is.null(statement_ids) || any(!grepl(valid_id, statement_ids))) {
    stop(
      "The perspective columns must be named sdg1 through sdg17 or stat1 ",
      "through stat17."
    )
  }
  sdg_numbers <- as.integer(sub("^(sdg|stat)", "", statement_ids))
  if (anyDuplicated(sdg_numbers) ||
      !setequal(sdg_numbers, seq_len(17L))) {
    stop("Each SDG number from 1 through 17 must occur exactly once.")
  }
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The R package 'magick' is required to draw the SDG icons.")
  }

  icon_directory <- system.file(
    "extdata", "sdg-icons", package = "qapproach"
  )
  icon_paths <- file.path(
    icon_directory,
    sprintf("E-WEB-Goal-%02d.png", sdg_numbers)
  )
  if (!nzchar(icon_directory)) {
    stop("The bundled SDG icon directory could not be found.")
  }
  if (any(!file.exists(icon_paths))) {
    stop(
      "The following SDG icons are missing: ",
      paste(icon_paths[!file.exists(icon_paths)], collapse = ", "), "."
    )
  }
  icons <- lapply(icon_paths, function(path) {
    image <- magick::image_read(path)
    dimensions <- magick::image_info(image)
    if (dimensions$width[1L] != dimensions$height[1L]) {
      stop("Every source SDG icon must be square: ", path, ".")
    }
    grDevices::as.raster(image)
  })

  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
    stop("The output directory could not be created: ", directory, ".")
  }
  if (is.null(filename)) {
    perspective_names <- rownames(perspectives)
    if (is.null(perspective_names) ||
        length(perspective_names) != nrow(perspectives)) {
      perspective_names <- paste0("perspective_", seq_len(nrow(perspectives)))
    }
    missing_names <- is.na(perspective_names) | !nzchar(perspective_names)
    perspective_names[missing_names] <- paste0(
      "perspective_", which(missing_names)
    )
    safe_names <- gsub("[^[:alnum:]_.-]", "_", perspective_names)
    safe_names <- make.unique(safe_names, sep = "_")
    paths <- file.path(directory, paste0("sdgdiamond_", safe_names, ".tiff"))
  } else {
    output_name <- sub(
      "\\.(tif|tiff)$", "", filename, ignore.case = TRUE
    )
    safe_name <- gsub("[^[:alnum:]_.-]", "_", output_name)
    paths <- file.path(
      directory,
      paste0(safe_name, "_group", seq_len(nrow(perspectives)), ".tiff")
    )
  }
  diamond_x <- rep(3:9, c(1, 2, 3, 5, 3, 2, 1))
  diamond_y <- c(3, 3.5, 2.5, 4, 3, 2, 5, 4, 3, 2, 1,
                 4, 3, 2, 3.5, 2.5, 3)

  draw_perspective <- function(index) {
    device_arguments <- list(
      filename = paths[index], width = width, height = height, units = "cm",
      res = resolution, bg = "white"
    )
    previous_device <- grDevices::dev.cur()
    device_opened <- FALSE
    if (capabilities("cairo")) {
      cairo_arguments <- device_arguments
      cairo_arguments$type <- "cairo"
      cairo_arguments$compression <- "lzw"
      suppressWarnings(try(
        do.call(grDevices::tiff, cairo_arguments), silent = TRUE
      ))
      device_opened <- grDevices::dev.cur() != previous_device
    }
    if (!device_opened) {
      do.call(grDevices::tiff, device_arguments)
      device_opened <- grDevices::dev.cur() != previous_device
    }
    if (!device_opened) {
      stop("The TIFF graphics device could not be opened.")
    }
    device <- grDevices::dev.cur()
    on.exit({
      open_devices <- grDevices::dev.list()
      if (!is.null(open_devices) && device %in% open_devices) {
        grDevices::dev.off(device)
      }
    }, add = TRUE)
    graphics::par(mar = c(0, 2, 0, 2))
    graphics::plot(
      NA_real_, NA_real_, xlim = c(0, 13), ylim = c(0, 7), asp = 1,
      type = "n", axes = FALSE, xlab = "", ylab = ""
    )
    inches_per_unit <- graphics::par("pin") /
      c(diff(graphics::par("usr")[1:2]), diff(graphics::par("usr")[3:4]))
    if (abs(inches_per_unit[1L] / inches_per_unit[2L] - 1) > 1e-7) {
      stop("The plot aspect ratio does not produce square SDG icons.")
    }
    graphics::text(1.75, 3.5, "Lower priority", cex = 1.5)
    graphics::text(11.25, 3.5, "Higher priority", cex = 1.5)
    display_order <- order(
      perspectives[index, ], seq_len(ncol(perspectives))
    )
    for (position in seq_len(17L)) {
      graphics::rasterImage(
        icons[[display_order[position]]],
        diamond_x[position], diamond_y[position],
        diamond_x[position] + 1, diamond_y[position] + 1,
        interpolate = TRUE
      )
    }
    invisible(NULL)
  }
  for (index in seq_len(nrow(perspectives))) {
    draw_perspective(index)
  }
  invisible(normalizePath(paths, mustWork = TRUE))
}

plot_heatmap <- function(result, labels = NULL,
                         statement_colors = NULL,
                         network_labelled = FALSE,
                         preset = NULL, file = NULL,
                         width = 11.2, height = 6.4) {
  if (!is.null(file)) {
    return(.write_plot_file(file, width, height, function() {
      plot_heatmap(
        result, labels, statement_colors, network_labelled, preset,
        file = NULL
      )
    }))
  }
  perspectives <- as.matrix(result$perspectives)
  nfactors <- nrow(perspectives)
  nstat <- ncol(perspectives)
  options <- .resolve_visualization_arguments(
    result, labels, statement_colors, preset
  )
  labels <- options$labels
  display_order <- rev(seq_len(nfactors))
  perspectives <- perspectives[display_order, , drop = FALSE]
  values <- sort(unique(as.numeric(perspectives)))
  breaks <- seq(
    min(values) - 0.5, max(values) + 0.5,
    length.out = length(values) + 1L
  )
  colors <- .heatmap_ranking_colors(values)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  device_lines <- graphics::par("din") / graphics::par("csi")
  bottom_margin <- min(
    max(9, min(18, max(nchar(labels)) * 0.7)),
    max(1, device_lines[2L] * 0.42)
  )
  top_margin <- min(1, max(0.4, device_lines[2L] * 0.08))
  perspective_labels <- paste("Perspective", display_order)
  left_margin <- min(
    max(8, max(nchar(perspective_labels)) * 0.7),
    max(0.8, device_lines[1L] * 0.25)
  )
  right_margin <- min(12, max(0.8, device_lines[1L] * 0.3))
  graphics::par(
    mar = c(bottom_margin, left_margin, top_margin, right_margin)
  )
  graphics::image(
    seq_len(nstat), seq_len(nfactors), t(perspectives),
    col = colors, breaks = breaks, xaxt = "n", yaxt = "n",
    xlab = "", ylab = "", main = ""
  )
  graphics::axis(
    1, seq_len(nstat), labels, las = 2,
    cex.axis = max(0.75, min(1.3, 20 / nstat))
  )
  graphics::axis(2, seq_len(nfactors), perspective_labels, las = 1)
  graphics::box()
  plot_bounds <- graphics::par("usr")
  graphics::legend(
    x = plot_bounds[2L] + 0.35,
    y = mean(plot_bounds[3L:4L]),
    xjust = 0, yjust = 0.5,
    legend = rev(values),
    pch = 22, pt.bg = rev(colors), col = "#D7E0E3",
    pt.cex = 3, y.intersp = 2,
    title = "Ranking", bty = "n", xpd = NA, cex = 0.85
  )
}

plot_jitterplot <- function(result, labels = NULL,
                            statement_colors = NULL,
                            network_labelled = FALSE,
                            preset = NULL, file = NULL,
                            width = 11.2, height = 6.4) {
  if (!is.null(file)) {
    return(.write_plot_file(file, width, height, function() {
      plot_jitterplot(
        result, labels, statement_colors, network_labelled, preset,
        file = NULL
      )
    }))
  }
  bootstrap <- result$`consensus priority score stability`
  if (is.null(bootstrap)) {
    bootstrap <- result$`bootstrap consensus priority scores`
  }
  if (is.null(bootstrap)) {
    stop(
      "A validation result or bootstrap consensus priority scores are ",
      "required for the jitterplot."
    )
  }
  bootstrap_scores <- as.matrix(bootstrap$`bootstrap cp-scores`)
  original_scores <- bootstrap$`cp-scores`
  if (is.null(original_scores)) original_scores <- bootstrap$`cp-scores`
  if (is.null(original_scores)) original_scores <- bootstrap$`original cp-scores`
  nstat <- nrow(bootstrap_scores)
  options <- .resolve_visualization_arguments(
    list("cp-scores" = original_scores),
    labels, statement_colors, preset
  )
  labels <- options$labels
  statement_colors <- options$statement_colors
  statement_colors <- .resolve_statement_colors(nstat, statement_colors)
  score_limits <- c(-0.03, 1.03)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(
    mar = c(max(7, min(16, max(nchar(labels)) * 0.55)), 10, 3, 2)
  )
  graphics::boxplot(
    t(bootstrap_scores), outline = FALSE, las = 2,
    col = statement_colors, names = labels,
    ylim = score_limits, yaxs = "i", yaxt = "n",
    ylab = "cp-scores", main = ""
  )
  graphics::axis(
    2, at = c(0, 0.5, 1),
    labels = c("Lower priority", "0.5", "Higher priority"), las = 1
  )
  for (statement in seq_len(nstat)) {
    jittered_positions <- jitter(
      rep(statement, ncol(bootstrap_scores)), amount = 0.12
    )
    graphics::points(
      jittered_positions, bootstrap_scores[statement, ],
      pch = 20, cex = 0.4,
      col = grDevices::adjustcolor("black", alpha.f = 0.3)
    )
    graphics::points(
      statement, original_scores[statement],
      pch = 23, bg = "white",
      col = statement_colors[statement], cex = 1.25
    )
  }
}

.resolve_network_layout_collisions <- function(
    layout, graph, vertex_sizes, iterations = 500L) {
  edge_ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  node_clearance <- 0.035 + 0.006 * outer(
    vertex_sizes, vertex_sizes, "+"
  )
  edge_clearance <- 0.025 + 0.006 * vertex_sizes
  vertex_count <- nrow(layout)

  for (iteration in seq_len(iterations)) {
    collision_found <- FALSE

    # Keep the plotted node shapes apart.
    if (vertex_count > 1L) {
      for (first in seq_len(vertex_count - 1L)) {
        for (second in seq.int(first + 1L, vertex_count)) {
          difference <- layout[first, ] - layout[second, ]
          distance <- sqrt(sum(difference^2))
          required_distance <- node_clearance[first, second]
          if (distance >= required_distance) next
          collision_found <- TRUE
          if (distance < sqrt(.Machine$double.eps)) {
            angle <- 2 * pi * (first + second) / vertex_count
            direction <- c(cos(angle), sin(angle))
            distance <- 0
          } else {
            direction <- difference / distance
          }
          displacement <- direction * (required_distance - distance) * 0.52
          layout[first, ] <- layout[first, ] + displacement
          layout[second, ] <- layout[second, ] - displacement
        }
      }
    }

    # Keep every non-incident node away from the visible line and arrow.
    if (nrow(edge_ends)) {
      for (edge_index in seq_len(nrow(edge_ends))) {
        from <- edge_ends[edge_index, 1L]
        to <- edge_ends[edge_index, 2L]
        segment <- layout[to, ] - layout[from, ]
        segment_length_squared <- sum(segment^2)
        if (segment_length_squared < .Machine$double.eps) next

        for (node in setdiff(seq_len(vertex_count), c(from, to))) {
          projection <- sum(
            (layout[node, ] - layout[from, ]) * segment
          ) / segment_length_squared
          if (projection <= 0.03 || projection >= 0.97) next
          closest_point <- layout[from, ] + projection * segment
          difference <- layout[node, ] - closest_point
          distance <- sqrt(sum(difference^2))
          required_distance <- edge_clearance[node]
          if (distance >= required_distance) next
          collision_found <- TRUE

          if (distance < sqrt(.Machine$double.eps)) {
            direction <- c(-segment[2L], segment[1L])
            direction <- direction / sqrt(sum(direction^2))
            if ((node + edge_index) %% 2L) direction <- -direction
            distance <- 0
          } else {
            direction <- difference / distance
          }
          displacement <- direction * (required_distance - distance) * 0.62
          layout[node, ] <- layout[node, ] + displacement
          layout[from, ] <- layout[from, ] - displacement * (1 - projection) * 0.18
          layout[to, ] <- layout[to, ] - displacement * projection * 0.18
        }
      }
    }

    if (!collision_found) break
  }
  layout
}

.score_network_layout <- function(layout, graph) {
  edge_ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  if (nrow(edge_ends) < 2L) return(0)

  orientation <- function(a, b, c) {
    (b[1L] - a[1L]) * (c[2L] - a[2L]) -
      (b[2L] - a[2L]) * (c[1L] - a[1L])
  }
  crossing_count <- 0L
  near_overlap <- 0
  for (first in seq_len(nrow(edge_ends) - 1L)) {
    first_nodes <- edge_ends[first, ]
    first_start <- layout[first_nodes[1L], ]
    first_end <- layout[first_nodes[2L], ]
    first_vector <- first_end - first_start
    first_length <- sqrt(sum(first_vector^2))
    if (first_length < sqrt(.Machine$double.eps)) next

    for (second in seq.int(first + 1L, nrow(edge_ends))) {
      second_nodes <- edge_ends[second, ]
      second_start <- layout[second_nodes[1L], ]
      second_end <- layout[second_nodes[2L], ]
      second_vector <- second_end - second_start
      second_length <- sqrt(sum(second_vector^2))
      if (second_length < sqrt(.Machine$double.eps)) next

      if (!length(intersect(first_nodes, second_nodes))) {
        o1 <- orientation(first_start, first_end, second_start)
        o2 <- orientation(first_start, first_end, second_end)
        o3 <- orientation(second_start, second_end, first_start)
        o4 <- orientation(second_start, second_end, first_end)
        if (o1 * o2 < 0 && o3 * o4 < 0) crossing_count <- crossing_count + 1L
      }

      direction_similarity <- abs(sum(first_vector * second_vector) /
        (first_length * second_length))
      midpoint_distance <- sqrt(sum(
        ((first_start + first_end) / 2 -
           (second_start + second_end) / 2)^2
      ))
      if (direction_similarity > 0.985 && midpoint_distance < 0.08) {
        near_overlap <- near_overlap + (0.08 - midpoint_distance) / 0.08
      }
    }
  }
  100 * crossing_count + 20 * near_overlap
}

plot_network <- function(result, labels = NULL,
                         statement_colors = NULL,
                         network_labelled = FALSE,
                         network_perspective_label_cex = 2,
                         network_ranking_label_cex = 1.1,
                         network_dataset_colors = NULL,
                         network_synthesis_color = NULL,
                         network_cps_color = NULL,
                         input_network = FALSE,
                         negative_loadings_marked = TRUE,
                         network_arrow_size = 0.56,
                         layout_seed = NULL,
                         .mixed_synthesis_inputs = FALSE,
                         preset = NULL, file = NULL,
                         width = 11.2, height = 6.4) {
  if (!is.null(file)) {
    return(.write_plot_file(file, width, height, function() {
      plot_network(
        result, labels, statement_colors, network_labelled,
        network_perspective_label_cex, network_ranking_label_cex,
        network_dataset_colors, network_synthesis_color, network_cps_color,
        input_network, negative_loadings_marked, network_arrow_size,
        layout_seed,
        .mixed_synthesis_inputs, preset, file = NULL
      )
    }))
  }
  .resolve_visualization_arguments(
    result, labels, statement_colors, preset
  )
  if (isTRUE(input_network)) negative_loadings_marked <- FALSE
  consensus_node <- "consensus_priority_scores"
  multi_network <- result$`multiple dataset network`
  if (is.list(multi_network) && length(multi_network$dataset_results)) {
    dataset_results <- multi_network$dataset_results
    dataset_names <- multi_network$dataset_names
    if (is.null(dataset_names)) dataset_names <- names(dataset_results)
    dataset_letters <- LETTERS[seq_len(10L)]
    if (length(dataset_results) > length(dataset_letters)) {
      stop("The synthesis network supports up to 10 datasets.")
    }
    network_palette <- c(
      "#E78AC3", "#66A61E", "#E6AB02", "#D95F02", "#7570B3",
      "#A6761D", "#E7298A", "#1B9E77", "#B3B3B3", "#8C564B",
      "#BCBD22"
    )
    analysis_colors <- network_palette[
      seq_len(length(dataset_results) + 1L)
    ]
    if (!is.null(network_dataset_colors)) analysis_colors[seq_along(dataset_results)] <- rep(network_dataset_colors, length.out = length(dataset_results))
    consensus_color <- if (is.null(network_cps_color)) "#5B9BD5" else network_cps_color
    if (!is.null(network_synthesis_color)) analysis_colors[length(analysis_colors)] <- network_synthesis_color
    vertex_parts <- list()
    edge_parts <- list()
    dataset_perspective_nodes <- character()
    dataset_ranking_nodes <- character()
    dataset_ranking_ids <- character()
    dataset_ranking_values <- list()
    for (dataset_index in seq_along(dataset_results)) {
      dataset_result <- dataset_results[[dataset_index]]
      flagged <- as.matrix(dataset_result$`Q method results`$flagged)
      flagged[is.na(flagged)] <- FALSE
      ranking_ids <- rownames(flagged)
      if (is.null(ranking_ids) || any(!nzchar(ranking_ids))) {
        ranking_ids <- colnames(dataset_result$`Q method results`$dataset)
      }
      if (is.null(ranking_ids) || length(ranking_ids) != nrow(flagged)) {
        ranking_ids <- paste("Ranking", seq_len(nrow(flagged)))
      }
      ranking_nodes <- paste0(
        "dataset_", dataset_index, "_ranking_", seq_len(nrow(flagged))
      )
      perspective_nodes <- paste0(
        "dataset_", dataset_index, "_perspective_", seq_len(ncol(flagged))
      )
      dataset_perspective_nodes <- c(
        dataset_perspective_nodes, perspective_nodes
      )
      dataset_ranking_nodes <- c(dataset_ranking_nodes, ranking_nodes)
      dataset_ranking_ids <- c(dataset_ranking_ids, ranking_ids)
      dataset_ranking_values[[dataset_index]] <- as.matrix(
        dataset_result$`Q method results`$dataset
      )
      flag_indices <- which(flagged, arr.ind = TRUE)
      loadings <- as.matrix(dataset_result$`Q method results`$loa)
      negative_flagged <- flagged & is.finite(loadings) & loadings < 0
      negative_perspectives <- if (ncol(negative_flagged)) {
        colSums(negative_flagged) > 0
      } else logical()
      if (nrow(flag_indices)) {
        edge_parts[[length(edge_parts) + 1L]] <- data.frame(
          from = ranking_nodes[flag_indices[, "row"]],
          to = perspective_nodes[flag_indices[, "col"]],
          lty = if (isTRUE(negative_loadings_marked)) {
            ifelse(negative_flagged[flag_indices], 2, 1)
          } else 1,
          stringsAsFactors = FALSE
        )
      }
      vertex_parts[[length(vertex_parts) + 1L]] <- data.frame(
        name = c(ranking_nodes, perspective_nodes),
        type = c(rep("ranking", length(ranking_nodes)),
                 rep("perspective", length(perspective_nodes))),
        display_label = c(
          if (isTRUE(network_labelled)) {
            ranking_ids
          } else rep("", length(ranking_nodes)),
          paste0(
            dataset_letters[dataset_index],
            seq_along(perspective_nodes)
          )
        ),
        node_color = c(
          rep("#222222", length(ranking_nodes)),
          rep(analysis_colors[dataset_index], length(perspective_nodes))
        ),
        stringsAsFactors = FALSE
      )
    }
    synthesis_flagged <- as.matrix(result$`Q method results`$flagged)
    synthesis_flagged[is.na(synthesis_flagged)] <- FALSE
    perspective_input_count <- length(dataset_perspective_nodes)
    if (!isTRUE(.mixed_synthesis_inputs) &&
        nrow(synthesis_flagged) != perspective_input_count) {
      stop("The synthesis flagging assignments do not match the dataset perspectives.")
    }
    if (isTRUE(.mixed_synthesis_inputs) &&
        nrow(synthesis_flagged) < perspective_input_count) {
      stop("The synthesis has fewer inputs than the supplied dataset perspectives.")
    }
    synthesis_input_nodes <- dataset_perspective_nodes
    if (isTRUE(.mixed_synthesis_inputs) &&
        nrow(synthesis_flagged) > perspective_input_count) {
      synthesis_dataset <- as.matrix(result$`Q method results`$dataset)
      all_ranking_values <- do.call(cbind, dataset_ranking_values)
      extra_rows <- seq.int(
        perspective_input_count + 1L, nrow(synthesis_flagged)
      )
      extra_nodes <- character(length(extra_rows))
      used_matches <- logical(ncol(all_ranking_values))
      for (extra_index in seq_along(extra_rows)) {
        synthesis_row <- extra_rows[extra_index]
        target <- synthesis_dataset[, synthesis_row]
        matches <- which(vapply(
          seq_len(ncol(all_ranking_values)),
          function(candidate) isTRUE(all.equal(
            as.numeric(all_ranking_values[, candidate]),
            as.numeric(target), check.attributes = FALSE
          )),
          logical(1)
        ))
        synthesis_id <- rownames(synthesis_flagged)[synthesis_row]
        if (length(matches) && !is.null(synthesis_id) && nzchar(synthesis_id)) {
          id_matches <- matches[dataset_ranking_ids[matches] == synthesis_id]
          if (length(id_matches)) matches <- id_matches
        }
        unused_matches <- matches[!used_matches[matches]]
        if (length(unused_matches)) matches <- unused_matches
        if (!length(matches)) {
          stop(
            "An additional synthesis input could not be matched to an ",
            "individual ranking in the supplied datasets."
          )
        }
        selected_match <- matches[1L]
        used_matches[selected_match] <- TRUE
        extra_nodes[extra_index] <- dataset_ranking_nodes[selected_match]
      }
      synthesis_input_nodes <- c(synthesis_input_nodes, extra_nodes)
    }
    synthesis_nodes <- paste0(
      "synthesis_perspective_", seq_len(ncol(synthesis_flagged))
    )
    synthesis_indices <- which(synthesis_flagged, arr.ind = TRUE)
    if (nrow(synthesis_indices)) {
      synthesis_loadings <- as.matrix(result$`Q method results`$loa)
      edge_parts[[length(edge_parts) + 1L]] <- data.frame(
        from = synthesis_input_nodes[synthesis_indices[, "row"]],
        to = synthesis_nodes[synthesis_indices[, "col"]],
        lty = if (isTRUE(negative_loadings_marked)) {
          ifelse(synthesis_loadings[synthesis_indices] < 0, 2, 1)
        } else 1,
        stringsAsFactors = FALSE
      )
    }
    edge_parts[[length(edge_parts) + 1L]] <- data.frame(
      from = synthesis_nodes,
      to = rep(consensus_node, length(synthesis_nodes)),
      lty = 1,
      stringsAsFactors = FALSE
    )
    vertex_parts[[length(vertex_parts) + 1L]] <- data.frame(
      name = c(synthesis_nodes, consensus_node),
      type = c(rep("perspective", length(synthesis_nodes)), "consensus"),
      display_label = c(
        paste0("s", seq_along(synthesis_nodes)), "cp-scores"
      ),
      node_color = c(
        rep(analysis_colors[length(analysis_colors)], length(synthesis_nodes)),
        consensus_color
      ),
      stringsAsFactors = FALSE
    )
    edges <- do.call(rbind, edge_parts)
    vertices <- do.call(rbind, vertex_parts)
    if (isTRUE(input_network)) {
      direct_synthesis_ranking_edges <- grepl(
        "_ranking_", edges$from, fixed = TRUE
      ) & grepl("synthesis_perspective_", edges$to, fixed = TRUE)
      edges <- edges[
        !grepl("_ranking_", edges$from, fixed = TRUE) |
          direct_synthesis_ranking_edges,
        , drop = FALSE
      ]
      dataset_nodes <- paste0("dataset_", seq_along(dataset_results))
      pmap <- stats::setNames(rep(dataset_nodes, vapply(dataset_results, function(x) ncol(as.matrix(x$`Q method results`$flagged)), integer(1))), dataset_perspective_nodes)
      smap <- stats::setNames(rep("synthesis", length(synthesis_nodes)), synthesis_nodes)
      edges$from <- ifelse(edges$from %in% names(pmap), pmap[edges$from], edges$from)
      edges$to <- ifelse(edges$to %in% names(pmap), pmap[edges$to], edges$to)
      edges$from <- ifelse(edges$from %in% names(smap), "synthesis", edges$from)
      edges$to <- ifelse(edges$to %in% names(smap), "synthesis", edges$to)
      vertices <- rbind(vertices[vertices$type == "ranking", , drop = FALSE],
        data.frame(name = dataset_nodes, type = "perspective", display_label = dataset_letters[seq_along(dataset_nodes)], node_color = analysis_colors[seq_along(dataset_nodes)]),
        data.frame(name = "synthesis", type = "perspective", display_label = "s", node_color = analysis_colors[length(analysis_colors)]),
        vertices[vertices$name == consensus_node, , drop = FALSE])
      ranking_nodes_all <- vertices$name[vertices$type == "ranking"]
      edges <- rbind(edges, data.frame(
        from = ranking_nodes_all,
        to = paste0("dataset_", sub("dataset_([0-9]+)_.*", "\\1", ranking_nodes_all)),
        lty = 1,
        stringsAsFactors = FALSE
      ))
    }
  } else {
    qmethod_result <- result$`Q method results`
    flagged <- as.matrix(qmethod_result$flagged)
    if (!length(flagged) || is.null(dim(flagged))) {
      stop("Q method flagging assignments are required for the network figure.")
    }
    flagged[is.na(flagged)] <- FALSE
    ranking_ids <- rownames(flagged)
    if (is.null(ranking_ids) || any(!nzchar(ranking_ids))) {
      ranking_ids <- colnames(qmethod_result$dataset)
    }
    if (is.null(ranking_ids) || length(ranking_ids) != nrow(flagged)) {
      ranking_ids <- paste("Ranking", seq_len(nrow(flagged)))
    }
    perspective_count <- ncol(flagged)
    ranking_nodes <- paste0("ranking_", seq_len(nrow(flagged)))
    perspective_nodes <- paste0("perspective_", seq_len(perspective_count))
    flag_indices <- which(flagged, arr.ind = TRUE)
    loadings <- as.matrix(qmethod_result$loa)
    negative_flagged <- flagged & is.finite(loadings) & loadings < 0
    ranking_edges <- if (nrow(flag_indices)) {
      data.frame(
        from = ranking_nodes[flag_indices[, "row"]],
        to = perspective_nodes[flag_indices[, "col"]],
        lty = if (isTRUE(negative_loadings_marked)) {
          ifelse(negative_flagged[flag_indices], 2, 1)
        } else 1,
        stringsAsFactors = FALSE
      )
    } else data.frame(from = character(), to = character())
    edges <- rbind(ranking_edges, data.frame(
      from = perspective_nodes,
      to = rep(consensus_node, perspective_count),
      lty = 1,
      stringsAsFactors = FALSE
    ))
    perspective_colors <- grDevices::hcl.colors(perspective_count, "Set 2")
    vertices <- data.frame(
      name = c(ranking_nodes, perspective_nodes, consensus_node),
      type = c(rep("ranking", length(ranking_nodes)),
               rep("perspective", perspective_count), "consensus"),
      display_label = c(
        if (isTRUE(network_labelled)) ranking_ids else rep("", length(ranking_ids)),
        paste0("P", seq_len(perspective_count)), "cp-scores"
      ),
      node_color = c(
        rep("#222222", length(ranking_nodes)),
        perspective_colors, "#70AD47"
      ),
      stringsAsFactors = FALSE
    )
  }
  graph <- igraph::graph_from_data_frame(
    edges, directed = TRUE, vertices = vertices
  )

  if (!is.null(layout_seed)) {
    if (length(layout_seed) != 1L || is.na(layout_seed) ||
        !is.finite(layout_seed) || layout_seed != as.integer(layout_seed)) {
      stop("layout_seed must be NULL or one whole number.")
    }
    withr::local_seed(as.integer(layout_seed))
  }
  optimize_straight_layout <- !isTRUE(input_network) &&
    is.list(multi_network) && length(multi_network$dataset_results)
  layout_candidates <- if (optimize_straight_layout) 12L else 1L
  candidate_layouts <- lapply(seq_len(layout_candidates), function(candidate_index) {
    candidate <- igraph::layout_with_fr(graph, niter = 1000L)
    igraph::norm_coords(
      candidate, xmin = -1, xmax = 1, ymin = -1, ymax = 1
    )
  })
  layout <- candidate_layouts[[1L]]

  vertex_type <- igraph::vertex_attr(graph, "type")
  vertex_labels <- igraph::vertex_attr(graph, "display_label")
  vertex_degree <- igraph::degree(graph, mode = "all")
  maximum_degree <- max(vertex_degree)
  vertex_sizes <- if (maximum_degree == 0) {
    rep(2.5, length(vertex_degree))
  } else {
    2.5 + 25.5 * vertex_degree / maximum_degree
  }
  vertex_sizes[vertex_type == "ranking"] <- 2.5
  vertex_sizes[vertex_type == "perspective"] <-
    vertex_sizes[vertex_type == "perspective"] * 0.75
  vertex_betweenness <- igraph::betweenness(
    graph, directed = FALSE, normalized = TRUE
  )
  maximum_betweenness <- max(vertex_betweenness)
  consensus_index <- which(vertex_type == "consensus")
  vertex_sizes[consensus_index] <- if (maximum_betweenness == 0) {
    2.5
  } else {
    2.5 + 25.5 *
      vertex_betweenness[consensus_index] / maximum_betweenness
  }
  if (isTRUE(input_network)) {
    labels_for_size <- igraph::vertex_attr(graph, "display_label")
    synthesis_index <- which(labels_for_size == "s")[1L]
    synthesis_size <- vertex_sizes[synthesis_index]
    vertex_sizes[consensus_index] <- synthesis_size
  }
  if (length(candidate_layouts) > 1L) {
    candidate_layouts <- lapply(
      candidate_layouts,
      .resolve_network_layout_collisions,
      graph = graph,
      vertex_sizes = vertex_sizes
    )
    layout_scores <- vapply(
      candidate_layouts, .score_network_layout, numeric(1), graph = graph
    )
    layout <- candidate_layouts[[which.min(layout_scores)]]
  } else {
    layout <- .resolve_network_layout_collisions(
      layout, graph, vertex_sizes
    )
  }
  if (isTRUE(input_network) &&
      is.list(multi_network) && length(multi_network$dataset_results)) {
    vertex_names <- igraph::vertex_attr(graph, "name")
    synthesis_layout_index <- match("synthesis", vertex_names)
    consensus_layout_index <- match(consensus_node, vertex_names)
    if (!is.na(synthesis_layout_index) && !is.na(consensus_layout_index)) {
      synthesis_to_consensus <-
        layout[consensus_layout_index, ] - layout[synthesis_layout_index, ]
      if (sqrt(sum(synthesis_to_consensus^2)) > 0) {
        synthesis_consensus_midpoint <-
          (layout[synthesis_layout_index, ] +
             layout[consensus_layout_index, ]) / 2
        half_separation <- 1.15 * synthesis_to_consensus
        layout[synthesis_layout_index, ] <-
          synthesis_consensus_midpoint - half_separation
        layout[consensus_layout_index, ] <-
          synthesis_consensus_midpoint + half_separation
      }
    }
  }
  vertex_colors <- igraph::vertex_attr(graph, "node_color")
  vertex_border_colors <- c(
    ranking = "#222222", perspective = "#333333", consensus = "#333333"
  )[vertex_type]
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
  plot_padding <- 0.05 + 0.006 * max(vertex_sizes)
  x_limits <- range(layout[, 1L]) + c(-plot_padding, plot_padding)
  y_limits <- range(layout[, 2L]) + c(-plot_padding, plot_padding)
  network_font <- if ("Calibri" %in% names(grDevices::pdfFonts())) "Calibri" else "sans"
  plot(
    graph,
    layout = layout,
    vertex.shape = ifelse(vertex_type == "consensus", "square", "circle"),
    vertex.size = vertex_sizes,
    vertex.color = vertex_colors,
    vertex.frame.color = vertex_border_colors,
    vertex.label = vertex_labels,
    vertex.label.color = "#1F1F1F",
    vertex.label.family = network_font,
    vertex.label.cex = c(
      ranking = if (isTRUE(network_labelled)) network_ranking_label_cex else 0,
      perspective = network_perspective_label_cex,
      consensus = 1.4
    )[vertex_type],
    vertex.label.dist = c(ranking = 0.9, perspective = 0, consensus = 0)[
      vertex_type
    ],
    edge.color = "#A0A0A0",
    edge.width = 1.6,
    edge.arrow.mode = 2,
    edge.arrow.size = network_arrow_size,
    edge.arrow.width = 1.4,
    edge.lty = igraph::edge_attr(graph, "lty"),
    edge.curved = igraph::curve_multiple(graph),
    asp = 1,
    rescale = FALSE,
    xlim = x_limits,
    ylim = y_limits
  )
}

#' Plot and save a two-layered multi-dataset synthesis network
#'
#' @param file Optional output graphics path. When `NULL`, the plot is drawn on
#'   the active graphics device without writing a file.
#' @param datasets Named list of qapproach() result objects, one per dataset.
#' @param synthesis qapproach() result for the combined synthesis dataset. The
#'   synthesis input may contain dataset perspectives and individual rankings
#'   returned by `not_agreeing()`.
#' @param labels Optional statement labels.
#' @param statement_colors Optional statement colors.
#' @param network_labelled Whether to label individual ranking nodes.
#' @param perspective_label_cex,ranking_label_cex Label-size multipliers for
#'   perspective and ranking nodes.
#' @param dataset_colors Optional colors for the dataset-perspective nodes.
#' @param synthesis_color Optional color for synthesis-perspective nodes.
#' @param cps_color Optional color for the cp-score node.
#' @param preset Optional visualization preset.
#' @param width,height Figure dimensions in inches.
#' @param input_network Whether to show the flow of analytical inputs instead
#'   of agreement and opposition results.
#' @param arrow_size Arrow-size multiplier.
#' @param negative_loadings_marked Whether negative flagged loadings are marked.
#' @param layout_seed Integer seed for reproducible network-node placement, or
#'   `NULL` to use the current random-number state.
#' @return Invisibly returns the normalized output path.
#' @export
plot_network_two_layered <- function(
    file = NULL,
    datasets, synthesis, labels = NULL, statement_colors = NULL,
    network_labelled = FALSE, perspective_label_cex = 2,
    ranking_label_cex = 1.1, dataset_colors = NULL,
    synthesis_color = NULL, cps_color = NULL, preset = NULL,
    width = 11.2, height = 6.4, input_network = FALSE,
    arrow_size = 0.56, negative_loadings_marked = TRUE,
    layout_seed = NULL) {
  if (!is.list(datasets) || !length(datasets)) {
    stop("datasets must be a non-empty list of qapproach() results.")
  }
  if (is.null(synthesis) || !is.list(synthesis)) {
    stop("synthesis must be a qapproach() result object.")
  }
  if (is.null(names(datasets))) names(datasets) <- paste0("Dataset", seq_along(datasets))
  synthesis_copy <- synthesis
  synthesis_copy$`multiple dataset network` <- list(
    dataset_results = datasets,
    dataset_names = names(datasets)
  )
  draw_network <- function() plot_network(synthesis_copy, labels, statement_colors,
               network_labelled = network_labelled,
               network_perspective_label_cex = perspective_label_cex,
               network_ranking_label_cex = ranking_label_cex,
               network_dataset_colors = dataset_colors,
               network_synthesis_color = synthesis_color,
               network_cps_color = cps_color,
               input_network = input_network,
               negative_loadings_marked = negative_loadings_marked,
               network_arrow_size = arrow_size,
               layout_seed = layout_seed,
    .mixed_synthesis_inputs = TRUE,
               preset = preset)
  if (is.null(file)) {
    draw_network()
    return(invisible(NULL))
  }
  .write_plot_file(file, width, height, draw_network)
}

.hierarchical_permutations <- function(values) {
  if (length(values) <= 1L) return(list(values))
  unlist(lapply(seq_along(values), function(index) {
    lapply(.hierarchical_permutations(values[-index]), function(remainder) {
      c(values[index], remainder)
    })
  }), recursive = FALSE)
}

.hierarchical_layout <- function(
    graph, vertex_levels, vertex_types,
    level_gaps = NULL, level_names = NULL) {
  vertex_levels <- as.integer(vertex_levels)
  if (length(vertex_levels) != igraph::vcount(graph) ||
      length(vertex_types) != igraph::vcount(graph)) {
    stop("The graph, vertex levels, and vertex types are incompatible.")
  }
  maximum_level <- max(vertex_levels)
  # Mirror the legacy layer specification: rankings occupy the highest
  # Sugiyama layer number, analytical levels descend toward zero, and layer 1
  # is intentionally left empty to create additional space before the top.
  sugiyama_layers <- ifelse(
    vertex_levels == 0L,
    maximum_level + 1L,
    ifelse(vertex_levels == maximum_level, 0L,
           maximum_level - vertex_levels + 1L)
  )

  # Insert requested gaps into the Sugiyama layer specification itself. This
  # lets Sugiyama account for the empty layers while arranging vertices and
  # routing edges, rather than stretching an already completed layout.
  if (!is.null(level_gaps)) {
    if (is.null(level_names) || length(level_names) != maximum_level) {
      stop(
        "level_names must provide one name per analytical level when ",
        "level_gaps is used."
      )
    }
    # Sugiyama requires integer layer identifiers. Find the smallest modest
    # scale that represents commonly useful fractional gaps (for example,
    # 0.5 or 0.25) exactly while avoiding unnecessary dummy layers.
    possible_scales <- seq_len(20L)
    compatible_scale <- vapply(possible_scales, function(scale) {
      all(abs(level_gaps * scale - round(level_gaps * scale)) < 1e-8)
    }, logical(1))
    if (!any(compatible_scale)) {
      stop(
        "level_gaps values must be expressible as fractions with a ",
        "denominator of 20 or less."
      )
    }
    layer_scale <- possible_scales[which(compatible_scale)[1L]]
    sugiyama_layers <- as.integer(sugiyama_layers * layer_scale)
    for (gap_name in names(level_gaps)) {
      boundary_level <- if (identical(gap_name, "rankings")) {
        0L
      } else {
        match(gap_name, level_names)
      }
      lower_levels <- vertex_levels <= boundary_level
      sugiyama_layers[lower_levels] <-
        sugiyama_layers[lower_levels] +
        as.integer(round(level_gaps[[gap_name]] * layer_scale))
    }
  }
  layout <- igraph::layout_with_sugiyama(
    graph, layers = sugiyama_layers, hgap = 1, vgap = 1,
    maxiter = 8000L
  )$layout

  # Sugiyama may leave gaps for dummy routing vertices. Preserve its ordering
  # but place the actual rankings directly beside one another.
  ranking_indices <- which(vertex_types == "ranking")
  ranking_order <- ranking_indices[order(layout[ranking_indices, 1L])]
  layout[ranking_order, 1L] <- if (length(ranking_order) == 1L) {
    mean(range(layout[, 1L]))
  } else {
    seq(min(layout[, 1L]), max(layout[, 1L]),
        length.out = length(ranking_order))
  }

  rescale_coordinate <- function(values) {
    value_range <- range(values)
    if (diff(value_range) == 0) return(rep(0, length(values)))
    2 * (values - value_range[1L]) / diff(value_range) - 1
  }
  layout[, 1L] <- rescale_coordinate(layout[, 1L])
  layout[, 2L] <- rescale_coordinate(layout[, 2L])
  if (mean(layout[vertex_levels == 0L, 2L]) >
      mean(layout[vertex_levels == maximum_level, 2L])) {
    layout[, 2L] <- -layout[, 2L]
  }

  # Optimize the initial Sugiyama layout while retaining equal horizontal gaps
  # within every analytical level.
  maximum_level <- max(vertex_levels)
  layer_indices <- lapply(seq_len(maximum_level), function(level_index) {
    which(vertex_levels == level_index & vertex_types == "analysis")
  })
  layer_permutations <- lapply(layer_indices, .hierarchical_permutations)
  combination_count <- prod(lengths(layer_permutations))
  graph_ends <- igraph::ends(graph, es = igraph::E(graph), names = FALSE)
  graph_ends <- matrix(as.integer(graph_ends), ncol = 2L)
  input_type <- igraph::edge_attr(graph, "input_type")
  perspective_edges <- which(input_type == "perspective")

  layer_slots <- lapply(seq_along(layer_indices), function(level_index) {
    count <- length(layer_indices[[level_index]])
    if (count <= 1L) return(0)
    # Use equal gaps between adjacent nodes and the two horizontal sides.
    seq(-1, 1, length.out = count + 2L)[-c(1L, count + 2L)]
  })

  crossing_count <- function(candidate) {
    if (length(perspective_edges) < 2L) return(0)
    count <- 0
    for (first_position in seq_len(length(perspective_edges) - 1L)) {
      first_edge <- graph_ends[perspective_edges[first_position], ]
      for (second_position in seq.int(
          first_position + 1L, length(perspective_edges))) {
        second_edge <- graph_ends[perspective_edges[second_position], ]
        if (length(intersect(first_edge, second_edge))) next
        if (vertex_levels[first_edge[1L]] != vertex_levels[second_edge[1L]] ||
            vertex_levels[first_edge[2L]] != vertex_levels[second_edge[2L]]) {
          next
        }
        source_difference <-
          candidate[first_edge[1L], 1L] - candidate[second_edge[1L], 1L]
        target_difference <-
          candidate[first_edge[2L], 1L] - candidate[second_edge[2L], 1L]
        if (source_difference * target_difference < 0) count <- count + 1
      }
    }
    count
  }

  score_layout <- function(candidate) {
    horizontal_lengths <- abs(
      candidate[graph_ends[, 1L], 1L] - candidate[graph_ends[, 2L], 1L]
    )
    edge_weights <- ifelse(input_type == "perspective", 2.5, 0.035)
    length_score <- sum(edge_weights * horizontal_lengths)
    crossing_score <- 18 * crossing_count(candidate)
    balance_score <- 0.5 * sum(vapply(layer_indices, function(indices) {
      if (length(indices)) mean(candidate[indices, 1L])^2 else 0
    }, numeric(1)))
    crossing_score + length_score + balance_score
  }

  place_order <- function(candidate, level_index, ordering) {
    candidate[ordering, 1L] <- layer_slots[[level_index]]
    candidate
  }
  best_layout <- layout
  best_score <- Inf

  if (combination_count <= 500L) {
    search_levels <- function(level_index, candidate) {
      if (level_index > length(layer_indices)) {
        candidate_score <- score_layout(candidate)
        if (candidate_score < best_score) {
          best_score <<- candidate_score
          best_layout <<- candidate
        }
        return(invisible(NULL))
      }
      for (ordering in layer_permutations[[level_index]]) {
        search_levels(
          level_index + 1L,
          place_order(candidate, level_index, ordering)
        )
      }
      invisible(NULL)
    }
    search_levels(1L, layout)
  } else {
    # For unusually large hierarchies, improve one layer at a time until the
    # ordering stabilizes rather than evaluating a factorial search space.
    best_layout <- layout
    for (iteration in seq_len(12L)) {
      changed <- FALSE
      for (level_index in seq_along(layer_indices)) {
        candidate_scores <- vapply(
          layer_permutations[[level_index]], function(ordering) {
            score_layout(place_order(best_layout, level_index, ordering))
          }, numeric(1)
        )
        selected <- which.min(candidate_scores)
        candidate <- place_order(
          best_layout, level_index,
          layer_permutations[[level_index]][[selected]]
        )
        if (!identical(candidate[, 1L], best_layout[, 1L])) changed <- TRUE
        best_layout <- candidate
      }
      if (!changed) break
    }
  }

  ranking_indices <- which(vertex_types == "ranking")
  if (length(ranking_indices) > 1L) {
    outgoing_targets <- split(graph_ends[, 2L], graph_ends[, 1L])
    vertex_names <- igraph::vertex_attr(graph, "name")
    earliest_target <- vapply(ranking_indices, function(index) {
      targets <- outgoing_targets[[as.character(index)]]
      if (is.null(targets) || !length(targets)) return(NA_integer_)
      target_levels <- vertex_levels[targets]
      earliest_targets <- targets[target_levels == min(target_levels)]
      earliest_targets[
        order(
          best_layout[earliest_targets, 1L],
          vertex_names[earliest_targets]
        )[1L]
      ]
    }, integer(1))

    # Keep all rankings feeding into the same earliest analysis together.
    # Target groups follow their horizontal positions. If different levels
    # share a position, direct inputs to the higher-level analysis come first.
    target_groups <- unique(earliest_target[!is.na(earliest_target)])
    target_groups <- target_groups[
      order(
        best_layout[target_groups, 1L],
        -vertex_levels[target_groups],
        vertex_names[target_groups]
      )
    ]
    foundation_order <- unlist(lapply(target_groups, function(target) {
      members <- ranking_indices[earliest_target == target]
      members[
        order(layout[members, 1L], vertex_names[members])
      ]
    }), use.names = FALSE)
    isolated <- ranking_indices[is.na(earliest_target)]
    if (length(isolated)) {
      isolated <- isolated[order(layout[isolated, 1L], vertex_names[isolated])]
      foundation_order <- c(foundation_order, isolated)
    }
    best_layout[foundation_order, 1L] <- seq(
      -1, 1, length.out = length(foundation_order)
    )
  }
  best_layout
}

#' Plot a hierarchical network across Q approach levels
#'
#' Draws a bottom-up network in which the original individual rankings feed
#' into their analyses and the resulting group perspectives feed into analyses
#' at subsequent levels. Each group perspective is retained as a separate
#' edge, including when several perspectives connect the same two analyses.
#' Individual rankings feeding into the same earliest target analysis are
#' positioned next to one another; their order within each target group retains
#' the crossing-minimizing Sugiyama order.
#'
#' @param levels A named list of levels. Each element is a qapproach() result
#'   or a list of qapproach() results representing the analyses at that level.
#' @param file Optional output graphics path. When `NULL`, the plot is drawn on
#'   the active graphics device without writing a file.
#' @param level_colors Optional vector containing one node color per level.
#'   The default uses distinguishable colors generated by `hues::iwanthue()`.
#' @param individual_arrow_color Color of arrows from individual rankings when
#'   `agreement = FALSE`.
#' @param perspective_arrow_color Color of arrows representing group
#'   perspectives when `agreement = FALSE`.
#' @param level_node_sizes Optional sizes for the analysis nodes. `NULL` uses
#'   0.3 times the number of underlying individual rankings agreeing through
#'   the analysis's group perspectives. A single value is used for all
#'   analysis nodes; a vector may provide one value per level or per analysis.
#'   A named vector may use the displayed analysis labels.
#' @param analysis_labels Optional displayed labels for the analysis nodes. A
#'   named vector may be matched to the internal analysis names; otherwise one
#'   label must be supplied per analysis in level order.
#' @param ranking_labelled Logical; show labels for the individual-ranking
#'   nodes. The default is `FALSE` and does not affect analysis-node labels.
#' @param analysis_labelled Logical; show labels for the analysis nodes. The
#'   default is `TRUE`. Set both `ranking_labelled = FALSE` and
#'   `analysis_labelled = FALSE` to draw the network without node labels.
#' @param level_gaps Optional named numeric vector adding vertical space after
#'   selected levels. Names must be `"rankings"` or one of the supplied level
#'   names except the final level. Values are multiples of the standard
#'   distance between adjacent levels. For example,
#'   `c(rankings = 0.5, level2 = 1)` adds half a standard distance between the
#'   ranking foundation and level 1, and one standard distance between level 2
#'   and level 3. Values must be finite and non-negative. `NULL` preserves the
#'   standard vertical spacing.
#' @param ranking_node_size Size of individual-ranking nodes.
#' @param arrow_size Arrow-size multiplier.
#' @param width,height Figure dimensions in inches when `file` is supplied.
#' @param agreement Logical; distinguish agreement, opposition, and undecided
#'   inputs through edge formatting when plotting. This argument affects only
#'   the drawn styling.
#' @param network_object Logical; return the prepared `igraph` network object
#'   without plotting or writing a file. The returned object contains only the
#'   network metadata prepared before plotting and does not contain plot
#'   styling selected through `agreement` or any other graphical settings. It
#'   retains the vertex attributes `name`, `type`, `level`, `label`, and
#'   `color`; the edge attributes `input_id`, `input_type`, and `status`; and
#'   the graph attribute `level_names`. The `color` vertex attribute is the
#'   unmodified level color before transparency is applied. The `input_type`
#'   and `status` edge attributes allow the agreement styling to be
#'   reconstructed. For analysis vertices, `name` contains the technical
#'   analysis name supplied through `levels` (for example, `alps` or
#'   `nandes`), whereas `label` contains the displayed analysis label.
#' @details When `network_object = TRUE`, the returned `igraph` object contains
#'   analytical metadata, but not the graphical settings calculated during
#'   plotting.
#'
#'   \strong{Attributes included in the returned network}
#'
#'   \itemize{
#'     \item The standard vertex attribute `name`, plus vertex attributes
#'       `type`, `level`, `label`, and `color`. For analysis vertices, `name`
#'       contains the technical analysis name supplied through `levels`; the
#'       `color` attribute contains the unmodified level color, before plotting
#'       transparency is applied;
#'     \item Edge attributes `input_id`, `input_type`, and `status`;
#'     \item The graph attribute `level_names`.
#'   }
#'
#'   \strong{Graphical settings not stored in the returned network}
#'
#'   The object does not contain the generated layout, vertex sizes, vertex
#'   frame color, label size or other label formatting, edge colors, edge line
#'   types or widths, arrow settings, edge curvature, plot limits, or other
#'   graphical parameters. These are calculated only when
#'   `network_object = FALSE`.
#'
#'   In the standard plot, individual-ranking nodes use `ranking_node_size`
#'   (default 1). Unless `level_node_sizes` is supplied, each analysis node is
#'   sized as 0.3 times the sum of the underlying individual rankings agreeing
#'   through its group perspectives. Analysis-node colors use the stored
#'   `color` attribute with 80 percent opacity; ranking nodes remain opaque.
#'   Frames are omitted. Ranking and analysis label sizes are 0.5 and 0.95,
#'   respectively; analysis labels are bold, and labels use color `#1F1F1F`
#'   and the sans-serif font family.
#'
#'   With `agreement = FALSE`, individual-ranking edges use
#'   `individual_arrow_color` and group-perspective edges use
#'   `perspective_arrow_color`; these defaults are light grey and black,
#'   respectively. These are edge colors, not line types, and both use solid
#'   lines. With `agreement = TRUE`, the input-type colors are replaced:
#'   agreeing edges are grey, opposing edges are firebrick, and undecided
#'   edges are dodger blue (`dodgerblue3`). All three statuses use solid lines.
#'   Individual-ranking and group-perspective edges both use width 0.5.
#'   Arrows use `arrow_size`, and parallel edges are curved with
#'   `igraph::curve_multiple()`.
#'
#'   The `agreement` argument changes only the drawn styling. The returned
#'   network object is the same in either mode: its `input_type` edge attribute
#'   supports the individual-versus-perspective mapping, and its `status` edge
#'   attribute supports the agreeing-versus-opposing-versus-undecided mapping.
#'
#'   Vertical gaps are applied after the deterministic Sugiyama ordering and
#'   equal-gap horizontal placement have been calculated. Each value in
#'   `level_gaps` shifts every subsequent analytical level upward without
#'   changing node order or horizontal position. Thus, `level2 = 0.5` adds
#'   half a standard level distance only between levels 2 and 3. The special
#'   name `rankings` controls the gap between the individual-ranking foundation
#'   and the first analytical level. Multiple named gaps are cumulative.
#'
#'   The retained attributes allow specialists to reproduce these mappings or
#'   construct entirely custom layouts and plots directly with `igraph`.
#' @return When `network_object = TRUE`, returns the prepared `igraph` object.
#'   Otherwise, invisibly returns the normalized output path when a file is
#'   written and invisibly returns `NULL` when drawing on the active device.
#' @export
plot_hierarchical_levels <- function(
    levels, file = NULL, level_colors = NULL,
    individual_arrow_color = "grey",
    perspective_arrow_color = "black", level_node_sizes = NULL,
    analysis_labels = NULL, ranking_labelled = FALSE,
    analysis_labelled = TRUE, level_gaps = NULL,
    ranking_node_size = 1, arrow_size = 0.25,
    width = 12, height = 10,
    agreement = FALSE, network_object = FALSE) {
  if (!is.list(levels) || length(levels) < 2L) {
    stop("levels must be a list containing at least two analytical levels.")
  }
  if (is.null(names(levels)) || any(!nzchar(names(levels)))) {
    names(levels) <- paste0("Level ", seq_along(levels))
  }
  names(levels) <- make.unique(names(levels))
  for (argument in c(
      "agreement", "ranking_labelled", "analysis_labelled", "network_object"
  )) {
    value <- get(argument, inherits = FALSE)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(argument, " must be TRUE or FALSE.")
    }
  }
  if (!is.null(level_gaps)) {
    valid_gap_names <- c("rankings", names(levels)[-length(levels)])
    if (!is.numeric(level_gaps) || !length(level_gaps) ||
        is.null(names(level_gaps)) || any(!nzchar(names(level_gaps))) ||
        anyDuplicated(names(level_gaps)) || anyNA(level_gaps) ||
        any(!is.finite(level_gaps)) || any(level_gaps < 0)) {
      stop(
        "level_gaps must be NULL or a uniquely named numeric vector of ",
        "finite, non-negative values."
      )
    }
    invalid_gap_names <- setdiff(names(level_gaps), valid_gap_names)
    if (length(invalid_gap_names)) {
      stop(
        "level_gaps names must be 'rankings' or a supplied level name ",
        "other than the final level. Available names: ",
        paste(valid_gap_names, collapse = ", "), "."
      )
    }
  }

  as_results <- function(level) {
    if (is.list(level) && !is.null(level$`Q method results`)) {
      return(list(level))
    }
    if (is.list(level) && length(level) &&
        all(vapply(level, function(x) {
          is.list(x) && !is.null(x$`Q method results`)
        }, logical(1)))) {
      return(level)
    }
    stop("Each level must contain qapproach() result object(s).")
  }
  levels <- lapply(levels, as_results)

  analysis_names <- lapply(seq_along(levels), function(level_index) {
    results <- levels[[level_index]]
    supplied <- names(results)
    if (!is.null(supplied) && all(nzchar(supplied))) {
      return(make.unique(as.character(supplied)))
    }
    make.unique(vapply(seq_along(results), function(index) {
      candidate <- results[[index]]$`dataset name`
      if (is.null(candidate) || !length(candidate) ||
          is.na(candidate[1L]) || !nzchar(as.character(candidate[1L]))) {
        paste0(names(levels)[level_index], " analysis ", index)
      } else {
        as.character(candidate[1L])
      }
    }, character(1)))
  })
  internal_names <- unlist(lapply(seq_along(analysis_names), function(i) {
    paste0("analysis::", i, "::", seq_along(analysis_names[[i]]))
  }), use.names = FALSE)
  natural_labels <- unlist(analysis_names, use.names = FALSE)
  if (anyDuplicated(natural_labels)) {
    natural_labels <- make.unique(natural_labels)
  }
  displayed_labels <- natural_labels
  if (!is.null(analysis_labels)) {
    if (!is.character(analysis_labels) || anyNA(analysis_labels) ||
        any(!nzchar(analysis_labels))) {
      stop("analysis_labels must contain non-empty character labels.")
    }
    if (!is.null(names(analysis_labels)) &&
        all(natural_labels %in% names(analysis_labels))) {
      displayed_labels <- unname(analysis_labels[natural_labels])
    } else if (length(analysis_labels) == length(natural_labels)) {
      displayed_labels <- unname(analysis_labels)
    } else {
      stop("analysis_labels must provide one label per analysis.")
    }
  }

  if (is.null(level_colors)) {
    withr::local_seed(42L)
    level_colors <- hues::iwanthue(length(levels))
  }
  if (length(level_colors) != length(levels) ||
      anyNA(level_colors) || any(!nzchar(level_colors))) {
    stop("level_colors must provide one valid color per level.")
  }

  perspective_owner <- character()
  analysis_counter <- 0L
  for (level_index in seq_along(levels)) {
    for (result_index in seq_along(levels[[level_index]])) {
      analysis_counter <- analysis_counter + 1L
      result <- levels[[level_index]][[result_index]]
      q <- result$`Q method results`
      perspective_ids <- rownames(result$perspectives)
      if (is.null(perspective_ids) ||
          length(perspective_ids) != ncol(as.matrix(q$flagged)) ||
          any(!nzchar(perspective_ids))) {
        perspective_ids <- colnames(as.matrix(q$flagged))
      }
      if (is.null(perspective_ids) ||
          length(perspective_ids) != ncol(as.matrix(q$flagged)) ||
          any(!nzchar(perspective_ids))) {
        perspective_ids <- paste0(
          natural_labels[analysis_counter], "_f",
          seq_len(ncol(as.matrix(q$flagged)))
        )
      }
      duplicate_ids <- intersect(names(perspective_owner), perspective_ids)
      if (length(duplicate_ids)) {
        stop("Group perspective identifiers must be unique across analyses.")
      }
      perspective_owner[perspective_ids] <- internal_names[analysis_counter]
    }
  }

  edge_rows <- list()
  individual_ids <- character()
  analysis_counter <- 0L
  edge_counter <- 0L
  for (level_index in seq_along(levels)) {
    for (result_index in seq_along(levels[[level_index]])) {
      analysis_counter <- analysis_counter + 1L
      result <- levels[[level_index]][[result_index]]
      q <- result$`Q method results`
      flagged <- as.matrix(q$flagged)
      loadings <- as.matrix(q$loa)
      if (!identical(dim(flagged), dim(loadings))) {
        stop("Flagging and loading matrices have incompatible dimensions.")
      }
      input_ids <- rownames(flagged)
      if (is.null(input_ids) || length(input_ids) != nrow(flagged) ||
          any(!nzchar(input_ids))) input_ids <- rownames(loadings)
      if (is.null(input_ids) || length(input_ids) != nrow(flagged) ||
          any(!nzchar(input_ids))) input_ids <- colnames(q$dataset)
      if (is.null(input_ids) || length(input_ids) != nrow(flagged) ||
          any(!nzchar(input_ids))) {
        stop("Every analysis must retain identifiers for its input rankings.")
      }
      positive <- flagged & is.finite(loadings) & loadings > 0
      negative <- flagged & is.finite(loadings) & loadings < 0
      positive[is.na(positive)] <- FALSE
      negative[is.na(negative)] <- FALSE
      status <- ifelse(
        rowSums(positive) > 0L, "agreeing",
        ifelse(rowSums(negative) > 0L, "opposing", "undecided")
      )
      is_perspective <- input_ids %in% names(perspective_owner)
      individual_ids <- union(individual_ids, input_ids[!is_perspective])
      sources <- ifelse(
        is_perspective,
        unname(perspective_owner[input_ids]),
        paste0("ranking::", input_ids)
      )
      for (input_index in seq_along(input_ids)) {
        edge_counter <- edge_counter + 1L
        edge_rows[[edge_counter]] <- data.frame(
          from = sources[input_index],
          to = internal_names[analysis_counter],
          input_id = input_ids[input_index],
          input_type = if (is_perspective[input_index]) {
            "perspective"
          } else "individual",
          status = status[input_index],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  edges <- do.call(rbind, edge_rows)
  # Draw the many individual-ranking links first so that the fewer and more
  # informative group-perspective links remain visible above them.
  edges <- edges[order(edges$input_type == "perspective"), , drop = FALSE]
  ranking_names <- paste0("ranking::", individual_ids)
  vertices <- data.frame(
    name = c(ranking_names, internal_names),
    label = c(individual_ids, displayed_labels),
    type = c(rep("ranking", length(individual_ids)),
             rep("analysis", length(internal_names))),
    level = c(rep(0L, length(individual_ids)),
              rep(seq_along(levels), lengths(analysis_names))),
    color = c(rep("black", length(individual_ids)),
              rep(level_colors, lengths(analysis_names))),
    stringsAsFactors = FALSE
  )
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE,
                                         vertices = vertices)
  graph_names <- igraph::vertex_attr(graph, "name")
  graph_types <- igraph::vertex_attr(graph, "type")
  graph_levels <- as.integer(igraph::vertex_attr(graph, "level"))
  graph_labels <- igraph::vertex_attr(graph, "label")
  graph_colors <- igraph::vertex_attr(graph, "color")
  graph <- igraph::set_graph_attr(graph, "level_names", names(levels))

  if (isTRUE(network_object)) {
    returned_names <- graph_names
    returned_names[match(internal_names, graph_names)] <- natural_labels
    graph <- igraph::set_vertex_attr(
      graph, "name", value = returned_names
    )
    return(graph)
  }

  layout <- .hierarchical_layout(
    graph, graph_levels, graph_types,
    level_gaps = level_gaps, level_names = names(levels)
  )

  if (is.null(level_node_sizes)) {
    agreement_counts <- agreement_across_levels(
      levels, print_table = FALSE
    )$underlying_agreement_counts
    agreement_counts$Analysis <- factor(
      agreement_counts$Analysis,
      levels = unique(agreement_counts$Analysis)
    )
    underlying_by_analysis <- stats::aggregate(
      `Underlying agreeing rankings` ~ Analysis,
      data = agreement_counts,
      FUN = sum
    )
    count_lookup <- stats::setNames(
      underlying_by_analysis$`Underlying agreeing rankings`,
      as.character(underlying_by_analysis$Analysis)
    )
    analysis_sizes <- unname(count_lookup[natural_labels]) * 0.3
    if (anyNA(analysis_sizes)) {
      stop(
        "Underlying agreement counts could not be matched to every analysis."
      )
    }
  } else if (!is.numeric(level_node_sizes) || anyNA(level_node_sizes) ||
             any(!is.finite(level_node_sizes)) || any(level_node_sizes <= 0)) {
    stop("level_node_sizes must contain positive finite numbers.")
  } else if (!is.null(names(level_node_sizes)) &&
             all(displayed_labels %in% names(level_node_sizes))) {
    analysis_sizes <- unname(level_node_sizes[displayed_labels])
  } else if (length(level_node_sizes) == 1L) {
    analysis_sizes <- rep(level_node_sizes, length(internal_names))
  } else if (length(level_node_sizes) == length(levels)) {
    analysis_sizes <- rep(level_node_sizes, lengths(analysis_names))
  } else if (length(level_node_sizes) == length(internal_names)) {
    analysis_sizes <- level_node_sizes
  } else {
    stop("level_node_sizes must have length 1, the number of levels, or the number of analyses.")
  }
  names(analysis_sizes) <- internal_names
  vertex_sizes <- rep(ranking_node_size, length(graph_names))
  analysis_indices <- match(internal_names, graph_names)
  vertex_sizes[analysis_indices] <- as.numeric(analysis_sizes[internal_names])
  edge_input_type <- igraph::edge_attr(graph, "input_type")
  edge_status <- igraph::edge_attr(graph, "status")
  edge_colors <- if (!isTRUE(agreement)) {
    ifelse(edge_input_type == "individual", individual_arrow_color,
           perspective_arrow_color)
  } else {
    c(agreeing = "grey", opposing = "firebrick",
      undecided = "dodgerblue3")[edge_status]
  }
  edge_lty <- rep(1L, nrow(edges))

  draw_plot <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mar = c(0.5, 0.5, 0.5, 0.5), xpd = NA)
    vertex_colors <- graph_colors
    analysis_vertices <- graph_types == "analysis"
    vertex_colors[analysis_vertices] <- grDevices::adjustcolor(
      graph_colors[analysis_vertices], alpha.f = 0.8
    )
    edge_widths <- rep(0.5, length(edge_input_type))
    plot_labels <- graph_labels
    if (!isTRUE(ranking_labelled)) {
      plot_labels[graph_types == "ranking"] <- ""
    }
    if (!isTRUE(analysis_labelled)) {
      plot_labels[graph_types == "analysis"] <- ""
    }
    y_limits <- c(min(layout[, 2L]) - 0.08, max(layout[, 2L]) + 0.22)
    plot(
      graph, layout = layout,
      vertex.size = vertex_sizes,
      vertex.color = vertex_colors,
      vertex.frame.color = NA,
      vertex.label = plot_labels,
      vertex.label.cex = ifelse(graph_types == "ranking", 0.5, 0.95),
      vertex.label.font = ifelse(graph_types == "ranking", 1, 2),
      vertex.label.color = "#1F1F1F",
      vertex.label.family = "sans",
      edge.color = edge_colors,
      edge.lty = edge_lty,
      edge.width = edge_widths,
      edge.arrow.mode = 2,
      edge.arrow.size = arrow_size,
      edge.curved = igraph::curve_multiple(graph, start = 0.2),
      asp = NA,
      rescale = FALSE,
      xlim = c(-1.35, 1.35),
      ylim = y_limits
    )
  }
  if (is.null(file)) {
    draw_plot()
    return(invisible(NULL))
  }
  .write_plot_file(file, width, height, draw_plot)
}


# Shared app/PDF figure pipeline ------------------------------------------

.has_undecided_rankings <- function(result) {
  has_undecided <- function(x) {
    q <- x$`Q method results`
    if (is.null(q$flagged) || is.null(q$loa)) return(FALSE)
    flagged <- as.matrix(q$flagged)
    loadings <- as.matrix(q$loa)
    if (!identical(dim(flagged), dim(loadings))) return(FALSE)
    decided <- (flagged & is.finite(loadings) & loadings > 0) |
      (flagged & is.finite(loadings) & loadings < 0)
    any(rowSums(decided, na.rm = TRUE) == 0L)
  }
  if (is.null(result) || !is.list(result)) return(FALSE)
  candidates <- list(result)
  multi_network <- result$`multiple dataset network`
  if (is.list(multi_network) && length(multi_network$dataset_results)) {
    candidates <- c(candidates, multi_network$dataset_results)
  }
  any(vapply(candidates, has_undecided, logical(1)))
}

.qapproach_figure_definitions <- function(include_validation = TRUE,
                                          result = NULL) {
  network_description <- paste(
    'A visualization of how the individual rankings feed into the group perspectives ("P" circles, their size reflects the number of incoming rankings), and how those are consolidated to the consensual priorities ("cp-scores", its size reflects its so-called betweenness centrality).',
    "Black dots represent the underlying individual rankings.",
    "Solid lines represent statistical agreement, dashed lines represent statistical opposition and further need for dialogue."
  )
  if (.has_undecided_rankings(result)) {
    network_description <- paste(
      network_description,
      "Isolated black dots represent undecided rankings."
    )
  }
  definitions <- list(
    list(
      id = "heatmap",
      title = "Statement rankings by group perspective",
      description = "Heatmap of the group perspectives that have a consensus on their ranking. Perspective 1 is the strongest, etc. The lowest priority is marked white, followed by a green color gradient to the second highest priority. The highest priority of each perspective is marked blue.",
      plot_function = plot_heatmap
    ),
    list(
      id = "barplot",
      title = "Consensus priority scores",
      description = "Barplot of the consensual priorities across the group perspectives, indicated by the consensus priority score per statement. A value of 0.5 represents neutral prioritization; lower and higher values indicate relatively lower and higher priority. Note that the difference between two bars does not represent an absolute prioritization gap.",
      plot_function = plot_barplot
    ),
    list(
      id = "network",
      title = "Consensus network",
      description = network_description,
      plot_function = plot_network
    ),
    list(
      id = "spiderweb",
      title = "Group perspective z-scores",
      description = "A visualization of the standard Q method results that are used to calculate the consensusal priorities among all group perspectives, i.e. the z-scores of the statements (a standardized metric of how strongly a perspective agrees or disagrees with a statement, given by the point locations in the radar chart) and the eigenvalues of the perspectives (their strength, given by the line width between points).",
      plot_function = plot_spiderweb
    ),
    list(
      id = "jitterplot",
      title = "Distributions of the bootstrapped consensus priority scores",
      description = "Boxplots of the consensus priority scores' validation, based on their bootstrapping. The diamonds represent the standard consensus priority scores, and their position relative to the boxplots indicates how robust they are and where further discussions on their prioritization may be needed.",
      plot_function = plot_jitterplot
    )
  )
  if (!isTRUE(include_validation)) {
    definitions <- Filter(
      function(definition) !identical(definition$id, "jitterplot"),
      definitions
    )
  }
  definitions
}

.create_figure_assets <- function(result, labels, output_dir,
                                 validation = NULL,
                                 statement_colors = NULL,
                                 network_labelled = FALSE,
                                 layout_seed = NULL,
                                 width = 11.2, height = 6.4,
                                 fallback_dpi = 150) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  include_validation <- !is.null(validation)
  if (include_validation &&
      (!is.list(validation) ||
       is.null(validation$`consensus priority score stability`))) {
    stop(
      "validation must be an object returned by validate() when supplied."
    )
  }
  definitions <- .qapproach_figure_definitions(include_validation, result)

  create_asset <- function(definition) {
    plot_input <- if (identical(definition$id, "jitterplot")) {
      validation
    } else {
      result
    }
    svg_file <- file.path(output_dir, paste0(definition$id, ".svg"))
    svg_created <- tryCatch({
      grDevices::svg(
        svg_file, width = width, height = height,
        bg = "white", onefile = TRUE
      )
      plot_arguments <- list(
        plot_input, labels, statement_colors,
        network_labelled = network_labelled
      )
      if (identical(definition$id, "network")) {
        plot_arguments$layout_seed <- layout_seed
      }
      do.call(definition$plot_function, plot_arguments)
      grDevices::dev.off()
      file.exists(svg_file) && file.info(svg_file)$size > 0
    }, error = function(error) {
      if (grDevices::dev.cur() > 1L) grDevices::dev.off()
      FALSE
    })

    if (isTRUE(svg_created)) {
      return(list(
        source_path = normalizePath(svg_file),
        source_type = "svg",
        display_path = normalizePath(svg_file),
        content_type = "image/svg+xml"
      ))
    }

    pdf_file <- file.path(output_dir, paste0(definition$id, ".pdf"))
    grDevices::pdf(
      pdf_file, width = width, height = height,
      bg = "white", onefile = TRUE
    )
    tryCatch(
      definition$plot_function(
        plot_input, labels, statement_colors,
        network_labelled = network_labelled
      ),
      error = function(error) {
        grDevices::dev.off()
        stop(error)
      }
    )
    grDevices::dev.off()

    png_file <- file.path(output_dir, paste0(definition$id, "_display.png"))
    rendered <- pdftools::pdf_render_page(pdf_file, page = 1L, dpi = fallback_dpi)
    image <- magick::image_read(rendered)
    magick::image_write(image, path = png_file, format = "png")
    list(
      source_path = normalizePath(pdf_file),
      source_type = "pdf",
      display_path = normalizePath(png_file),
      content_type = "image/png"
    )
  }

  assets <- lapply(definitions, create_asset)
  stats::setNames(
    assets,
    vapply(definitions, `[[`, character(1), "id")
  )
}

.figure_asset_raster <- function(asset, dpi = 300, trim = FALSE) {
  if (identical(asset$source_type, "svg")) {
    image <- magick::image_read(
      asset$source_path,
      density = paste0(dpi, "x", dpi)
    )
  } else {
    image <- magick::image_read(
      pdftools::pdf_render_page(asset$source_path, page = 1L, dpi = dpi)
    )
  }
  if (isTRUE(trim)) {
    image <- magick::image_trim(image)
    image <- magick::image_border(image, "white", "24x24")
  }
  grDevices::as.raster(image)
}

write_figure_collection <- function(
                                    file, result, validation = NULL,
                                    labels = NULL,
                                    statement_colors = NULL,
                                    network_labelled = FALSE,
                                    layout_seed = NULL,
                                    figure_assets = NULL,
                                    preset = NULL,
                                    two_layered = FALSE) {
  file <- .resolve_plot_file(file)
  if (tolower(tools::file_ext(file)) != "pdf") {
    stop("file must have a .pdf extension.")
  }
  include_validation <- !is.null(validation)
  if (include_validation &&
      (!is.list(validation) ||
       is.null(validation$`consensus priority score stability`))) {
    stop(
      "validation must be an object returned by validate() when supplied."
    )
  }
  options <- .resolve_visualization_arguments(
    result, labels, statement_colors, preset
  )
  labels <- options$labels
  statement_colors <- options$statement_colors
  temporary_assets <- NULL
  if (is.null(figure_assets)) {
    temporary_assets <- tempfile("qapproach-figure-assets-")
    dir.create(temporary_assets)
    on.exit(unlink(temporary_assets, recursive = TRUE), add = TRUE)
    figure_assets <- .create_figure_assets(
      result, labels, temporary_assets,
      validation = validation,
      statement_colors = statement_colors,
      network_labelled = network_labelled,
      layout_seed = layout_seed
    )
  }

  figure_page <- function(definition, page_width, page_height,
                          trim_figure = FALSE) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mar = c(0, 0, 0, 0), xpd = FALSE)
    graphics::plot.new()
    raster <- .figure_asset_raster(
      figure_assets[[definition$id]], trim = trim_figure
    )
    raster_ratio <- ncol(raster) / nrow(raster)
    page_ratio <- page_width / page_height
    caption_cex <- 0.95
    caption_bottom <- 0.035
    caption_gap <- 0.015
    caption_line_height <- graphics::strheight(
      "Ag", units = "user", cex = caption_cex
    ) * 1.35

    figure_geometry <- function(region_bottom) {
      region <- c(
        left = 0.03, bottom = region_bottom, right = 0.97, top = 0.97
      )
      panel_ratio <- page_ratio *
        (region[["right"]] - region[["left"]]) /
        (region[["top"]] - region[["bottom"]])
      if (raster_ratio > panel_ratio) {
        raster_width <- region[["right"]] - region[["left"]]
        raster_height <- (region[["top"]] - region[["bottom"]]) *
          panel_ratio / raster_ratio
      } else {
        raster_width <- (region[["right"]] - region[["left"]]) *
          raster_ratio / panel_ratio
        raster_height <- region[["top"]] - region[["bottom"]]
      }
      list(
        width = raster_width,
        height = raster_height,
        x = mean(region[c("left", "right")]) - raster_width / 2,
        y = region[["bottom"]]
      )
    }

    wrap_description <- function(first_width, full_width) {
      words <- strsplit(trimws(definition$description), "[[:space:]]+")[[1L]]
      if (!length(words) || identical(words, "")) return("")
      lines <- character()
      current <- ""
      available_width <- max(0, first_width)
      for (word in words) {
        candidate <- if (nzchar(current)) paste(current, word) else word
        candidate_width <- graphics::strwidth(
          candidate, units = "user", cex = caption_cex
        )
        if (!nzchar(current) && candidate_width > available_width &&
            available_width < full_width) {
          lines <- c(lines, "")
          current <- word
          available_width <- full_width
        } else if (!nzchar(current) || candidate_width <= available_width) {
          current <- candidate
        } else {
          lines <- c(lines, current)
          current <- word
          available_width <- full_width
        }
      }
      c(lines, current)
    }

    region_bottom <- 0.11
    for (iteration in seq_len(4L)) {
      geometry <- figure_geometry(region_bottom)
      title_width <- graphics::strwidth(
        definition$title, units = "user", cex = caption_cex, font = 2
      )
      separator_width <- graphics::strwidth(
        ": ", units = "user", cex = caption_cex
      )
      caption_lines <- wrap_description(
        geometry$width - title_width - separator_width,
        geometry$width
      )
      caption_height <- length(caption_lines) * caption_line_height
      region_bottom <- caption_bottom + caption_height + caption_gap
    }
    geometry <- figure_geometry(region_bottom)
    graphics::rasterImage(
      raster,
      geometry$x, geometry$y,
      geometry$x + geometry$width, geometry$y + geometry$height,
      interpolate = TRUE
    )
    caption_top <- caption_bottom +
      length(caption_lines) * caption_line_height
    first_line_y <- caption_top - caption_line_height / 2
    graphics::text(
      geometry$x, first_line_y, definition$title,
      adj = c(0, 0.5), font = 2, cex = caption_cex, xpd = NA
    )
    graphics::text(
      geometry$x + title_width, first_line_y,
      paste0(": ", caption_lines[1L]),
      adj = c(0, 0.5), font = 1, cex = caption_cex, xpd = NA
    )
    if (length(caption_lines) > 1L) {
      for (line_index in seq.int(2L, length(caption_lines))) {
        graphics::text(
          geometry$x,
          first_line_y - (line_index - 1L) * caption_line_height,
          caption_lines[line_index],
          adj = c(0, 0.5), font = 1, cex = caption_cex, xpd = NA
        )
      }
    }
  }

  page_directory <- tempfile("qapproach-figure-pages-")
  dir.create(page_directory)
  on.exit(unlink(page_directory, recursive = TRUE), add = TRUE)
  definitions <- .qapproach_figure_definitions(include_validation, result)
  if (isTRUE(two_layered)) {
    network_index <- which(vapply(definitions, function(x)
      identical(x$id, "network"), logical(1)))
    if (length(network_index)) {
      definitions[[network_index]]$description <- paste(
        definitions[[network_index]]$description,
        "In a multiple-dataset analysis, dataset perspectives feed into the synthesis perspectives. When added to the analysis, individual rankings that do not agree with a dataset perspective may also feed into a synthesis perspective. The cp-scores are illustrated for the synthesis only, but in fact computed for every partial analysis."
      )
    }
  }
  portrait_ids <- character()
  page_files <- vapply(
    seq_along(definitions),
    function(index) {
      definition <- definitions[[index]]
      portrait <- definition$id %in% portrait_ids
      page_width <- if (portrait) 8.3 else 11.7
      page_height <- if (portrait) 11.7 else 8.3
      page_file <- file.path(
        page_directory, sprintf("page-%02d.pdf", index)
      )
      grDevices::pdf(
        page_file, width = page_width, height = page_height,
        onefile = TRUE
      )
      device_open <- TRUE
      tryCatch(
        figure_page(
          definition, page_width, page_height,
          trim_figure = FALSE
        ),
        finally = {
          if (device_open) grDevices::dev.off()
        }
      )
      page_file
    },
    character(1)
  )
  pdftools::pdf_combine(page_files, output = file)
  invisible(normalizePath(file))
}
