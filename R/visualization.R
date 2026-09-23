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
