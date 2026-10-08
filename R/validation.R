############################################################################
### Bootstrap validation of Q approach perspectives and                  ###
### consensus priority scores                                            ###
############################################################################
### Source qapproach_analysis.R before this file for manual use.         ###
############################################################################

.format_validation_number <- function(values, digits = 2L) {
  vapply(values, function(value) {
    if (is.na(value)) return(NA_character_)
    use_digits <- if (is.finite(value) && abs(value - round(value)) < 1e-8) {
      0L
    } else as.integer(digits)
    formatC(value, format = "f", digits = use_digits)
  }, character(1))
}
.sort_validation_table <- function(table, sort_by = NULL, decreasing = TRUE) {
  if (is.null(sort_by)) return(table)
  if (!is.character(sort_by) || length(sort_by) != 1L || is.na(sort_by) ||
      !nzchar(sort_by)) {
    stop("sort_by must be NULL or one column name.")
  }
  if (!sort_by %in% names(table)) {
    stop(
      "sort_by column not found: ", sort_by, ". Available columns are: ",
      paste(names(table), collapse = ", "), "."
    )
  }
  if (!is.logical(decreasing) || length(decreasing) != 1L ||
      is.na(decreasing)) {
    stop("decreasing must be TRUE or FALSE.")
  }
  values <- table[[sort_by]]
  ord <- if (is.numeric(values)) {
    order(values, decreasing = decreasing, na.last = TRUE)
  } else {
    order(as.character(values), decreasing = decreasing, na.last = TRUE)
  }
  table[ord, , drop = FALSE]
}

.write_validation_csv <- function(table, file) {
  if (is.null(file)) return(invisible(NULL))
  if (!is.character(file) || length(file) != 1L ||
      is.na(file) || !nzchar(trimws(file))) {
    stop("file must be NULL or one non-empty path.")
  }
  file <- if (grepl("[.]csv$", file, ignore.case = TRUE)) {
    file
  } else {
    paste0(file, ".csv")
  }
  path <- file
  parent <- dirname(path)
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE)
  utils::write.csv(table, path, row.names = FALSE, na = "")
  invisible(normalizePath(path, mustWork = TRUE))
}

#' Summarize consensus across successive Q approach levels
#'
#' **Experimental.** This function is provided for testing and further
#' refinement. Its interface and calculations may change, and it should not
#' yet be used for definitive analytical conclusions.
#'
#' @param levels A named list of levels. Each element is a Q approach result
#'   or a list of Q approach results representing the datasets at that level.
#' @param print_table Logical; print the transition table for convenience.
#' @return A list containing the number of raw input rankings, a transition
#'   table, and convenient final-transition values. Each transition reports
#'   direct and underlying agreement; dataset pool counts and propagated
#'   individual counts by resulting perspective are stored as list-columns.
#' @export
consensus_across_levels <- function(levels, print_table = TRUE) {
  if (!is.list(levels) || length(levels) < 2L) {
    stop("levels must be a list containing at least two analytical levels.")
  }
  if (is.null(names(levels)) || any(!nzchar(names(levels)))) {
    names(levels) <- paste0("Level ", seq_along(levels))
  }
  as_results <- function(level) {
    if (is.list(level) && !is.null(level$`Q method results`)) {
      return(list(level))
    }
    if (is.list(level) && length(level) &&
        all(vapply(level, function(x) is.list(x) &&
          !is.null(x$`Q method results`), logical(1)))) {
      return(level)
    }
    stop("Each level must contain qapproach() result object(s).")
  }
  levels <- lapply(levels, as_results)
  q_of <- function(x) x$`Q method results`
  agreement <- function(q) {
    flagged <- as.matrix(q$flagged)
    loading <- as.matrix(q$loa)
    if (!identical(dim(flagged), dim(loading))) {
      stop("Flagging and loading matrices have incompatible dimensions.")
    }
    positive <- flagged & is.finite(loading) & loading > 0
    negative <- flagged & is.finite(loading) & loading < 0
    list(
      input_units = nrow(flagged),
      agreeing = sum(positive, na.rm = TRUE),
      opposing = sum(negative, na.rm = TRUE),
      undecided = max(0, nrow(flagged) - sum(positive | negative, na.rm = TRUE)),
      positive = positive,
      negative = negative
    )
  }
  first_stats <- lapply(levels[[1L]], function(x) agreement(q_of(x)))
  pool_total <- sum(vapply(first_stats, `[[`, numeric(1), "input_units"))
  pool_agreeing <- sum(vapply(first_stats, `[[`, numeric(1), "agreeing"))

  first_names <- names(levels[[1L]])
  if (is.null(first_names) || any(!nzchar(first_names))) {
    first_names <- vapply(seq_along(levels[[1L]]), function(i) {
      name <- levels[[1L]][[i]]$`dataset name`
      if (is.null(name) || !length(name) || !nzchar(name[1L])) {
        paste0("Dataset ", i)
      } else as.character(name[1L])
    }, character(1))
  }
  underlying_pool_counts <- data.frame(
    dataset = first_names,
    rankings_agreeing = vapply(first_stats, `[[`, numeric(1), "agreeing"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  underlying_pool_agreement <- if (pool_total) {
    pool_agreeing / pool_total
  } else NA_real_

  raw_lineage <- do.call(c, lapply(levels[[1L]], function(x) {
    q <- q_of(x)
    ids <- rownames(q$flagged)
    if (is.null(ids) || any(!nzchar(ids))) ids <- colnames(q$dataset)
    if (is.null(ids) || length(ids) != nrow(q$flagged)) return(numeric())
    stats::setNames(rep(1, length(ids)), ids)
  }))
  perspective_lineage <- do.call(c, lapply(levels[[1L]], function(x) {
    q <- q_of(x)
    a <- agreement(q)
    counts <- as.numeric(colSums(a$positive, na.rm = TRUE))
    ids <- rownames(x$perspectives)
    if (is.null(ids) || length(ids) != length(counts) || any(!nzchar(ids))) {
      ids <- paste0("Perspective ", seq_along(counts))
    }
    stats::setNames(counts, ids)
  }))
  lineage_lookup <- c(raw_lineage, perspective_lineage)

  transition_rows <- vector("list", length(levels) - 1L)
  for (i in seq_len(length(levels) - 1L)) {
    target_results <- levels[[i + 1L]]
    target_stats <- lapply(target_results, function(x) agreement(q_of(x)))
    input_rankings <- sum(vapply(target_stats, `[[`, numeric(1), "input_units"))
    agreeing <- sum(vapply(target_stats, `[[`, numeric(1), "agreeing"))
    opposing <- sum(vapply(target_stats, `[[`, numeric(1), "opposing"))
    resulting_perspectives <- sum(vapply(
      target_stats, function(x) ncol(x$positive), numeric(1)
    ))

    next_lineage <- lapply(seq_along(target_results), function(j) {
      result <- target_results[[j]]
      q <- q_of(result)
      stats <- target_stats[[j]]
      input_ids <- rownames(q$flagged)
      weights <- NULL
      if (!is.null(input_ids) && length(input_ids) == nrow(stats$positive) &&
          all(input_ids %in% names(lineage_lookup))) {
        weights <- as.numeric(lineage_lookup[input_ids])
      } else if (length(target_results) == 1L &&
                 length(perspective_lineage) == nrow(stats$positive)) {
        # Preserve the documented concatenation-order fallback when identifiers
        # are unavailable but the level structures align exactly.
        weights <- as.numeric(perspective_lineage)
      }
      counts <- if (is.null(weights)) {
        rep(NA_real_, ncol(stats$positive))
      } else {
        as.numeric(crossprod(weights, stats$positive))
      }
      ids <- rownames(result$perspectives)
      if (is.null(ids) || length(ids) != length(counts) || any(!nzchar(ids))) {
        prefix <- if (length(target_results) > 1L) paste0("Dataset", j, "_") else ""
        ids <- paste0(prefix, "Perspective ", seq_along(counts))
      }
      stats::setNames(counts, ids)
    })
    underlying_by_perspective <- do.call(c, next_lineage)
    underlying_individual_agreement <- if (
      pool_total && length(underlying_by_perspective) &&
      all(is.finite(underlying_by_perspective))
    ) {
      sum(underlying_by_perspective) / pool_total
    } else NA_real_

    row <- data.frame(
      from = names(levels)[i], to = names(levels)[i + 1L],
      input_rankings = input_rankings,
      resulting_perspectives = resulting_perspectives,
      agreeing = agreeing, opposing = opposing,
      undecided = input_rankings - agreeing - opposing,
      effective_agreement = if (input_rankings) agreeing / input_rankings else NA_real_,
      underlying_pool_agreement = underlying_pool_agreement,
      underlying_individual_agreement = underlying_individual_agreement,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    row$underlying_pool_counts <- I(list(underlying_pool_counts))
    row$underlying_individual_by_perspective <- I(list(underlying_by_perspective))
    transition_rows[[i]] <- row

    perspective_lineage <- underlying_by_perspective
    lineage_lookup <- c(lineage_lookup, perspective_lineage)
  }
  transitions <- do.call(rbind, transition_rows)
  final_transition <- nrow(transitions)
  output <- list(
    raw_input_rankings = pool_total,
    transitions = transitions,
    underlying_pool_agreement = transitions$underlying_pool_agreement[final_transition],
    underlying_pool_counts = transitions$underlying_pool_counts[[final_transition]],
    underlying_individual_agreement =
      transitions$underlying_individual_agreement[final_transition],
    underlying_individual_by_perspective =
      transitions$underlying_individual_by_perspective[[final_transition]]
  )
  if (isTRUE(print_table)) {
    display <- transitions
    display$underlying_pool_counts <- vapply(
      transitions$underlying_pool_counts,
      function(x) paste0(x$dataset, ": ", x$rankings_agreeing,
                         collapse = "; "), character(1)
    )
    display$underlying_individual_by_perspective <- vapply(
      transitions$underlying_individual_by_perspective,
      function(x) paste0(names(x), ": ", x, collapse = "; "), character(1)
    )
    print(display, row.names = FALSE)
    cat("Underlying pool agreement:",
        paste0(round(100 * output$underlying_pool_agreement, 1), "%"), "\n")
    cat("Underlying individual agreement:",
        paste0(round(100 * output$underlying_individual_agreement, 1), "%"), "\n")
  }
  output
}

#' Trace ranking agreement across successive Q approach levels
#'
#' **Experimental.** This function traces each original input ranking through
#' an arbitrary number of analytical levels. Original rankings may enter at
#' any supplied level; inputs that match a generated group-perspective
#' identifier are excluded from the set of original rankings. Its interface
#' and interpretation may be refined as additional multi-level applications
#' become available.
#'
#' For every supplied level, the returned table reports the analysis in which
#' the current input was found, whether it agreed with, opposed, or was
#' undecided about a perspective, and the relevant perspective identifier.
#' `Agreement path` joins only positively agreeing perspectives with ` > `.
#' Opposition and undecided status do not propagate agreement through a group
#' perspective. If a raw ranking is explicitly re-added at a later level, it
#' can enter the agreement path there. Rankings that do not agree with a
#' perspective at any level receive `NA` as their agreement path.
#'
#' @param levels A named list of levels. Each element is a Q approach result
#'   or a list of Q approach results representing the analyses at that level.
#' @param print_table Logical; print the agreement-lineage table for
#'   convenience.
#' @return A list containing `paths`, with one row per original input ranking;
#'   `underlying_agreement_counts`, with the number of unique original rankings
#'   whose positive path reaches each perspective; and
#'   `terminal_path_summary`, with counts and shares by terminal perspective.
#'   `rankings_not_agreeing` reports the count, identifiers, and underlying
#'   statement-ranking values of original rankings that do not positively
#'   agree with any perspective at any level.
#'   An internal completeness check warns if the original rankings are not
#'   represented exactly once in `paths`.
#' @export
agreement_across_levels <- function(levels, print_table = TRUE) {
  if (!is.list(levels) || length(levels) < 2L) {
    stop("levels must be a list containing at least two analytical levels.")
  }
  if (!is.logical(print_table) || length(print_table) != 1L ||
      is.na(print_table)) {
    stop("print_table must be TRUE or FALSE.")
  }
  if (is.null(names(levels)) || any(!nzchar(names(levels)))) {
    names(levels) <- paste0("Level ", seq_along(levels))
  }
  names(levels) <- make.unique(names(levels))

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

  analysis_names <- function(results, level_name) {
    supplied <- names(results)
    if (!is.null(supplied) && all(nzchar(supplied))) {
      return(make.unique(supplied))
    }
    make.unique(vapply(seq_along(results), function(index) {
      candidate <- results[[index]]$`dataset name`
      if (is.null(candidate) || !length(candidate) ||
          is.na(candidate[1L]) || !nzchar(as.character(candidate[1L]))) {
        paste0(level_name, " analysis ", index)
      } else {
        as.character(candidate[1L])
      }
    }, character(1)))
  }

  result_map <- function(result, analysis_name) {
    q <- result$`Q method results`
    flagged <- as.matrix(q$flagged)
    loadings <- as.matrix(q$loa)
    if (!identical(dim(flagged), dim(loadings))) {
      stop("Flagging and loading matrices have incompatible dimensions.")
    }
    input_ids <- rownames(flagged)
    if (is.null(input_ids) || length(input_ids) != nrow(flagged) ||
        any(!nzchar(input_ids))) {
      input_ids <- rownames(loadings)
    }
    if (is.null(input_ids) || length(input_ids) != nrow(flagged) ||
        any(!nzchar(input_ids))) {
      input_ids <- colnames(q$dataset)
    }
    if (is.null(input_ids) || length(input_ids) != nrow(flagged) ||
        any(!nzchar(input_ids))) {
      stop("Every analysis must retain identifiers for its input rankings.")
    }
    perspective_ids <- rownames(result$perspectives)
    if (is.null(perspective_ids) ||
        length(perspective_ids) != ncol(flagged) ||
        any(!nzchar(perspective_ids))) {
      perspective_ids <- colnames(flagged)
    }
    if (is.null(perspective_ids) ||
        length(perspective_ids) != ncol(flagged) ||
        any(!nzchar(perspective_ids))) {
      perspective_ids <- paste0(analysis_name, "_f", seq_len(ncol(flagged)))
    }
    positive <- flagged & is.finite(loadings) & loadings > 0
    negative <- flagged & is.finite(loadings) & loadings < 0
    positive[is.na(positive)] <- FALSE
    negative[is.na(negative)] <- FALSE
    if (any(rowSums(positive) > 1L) || any(rowSums(negative) > 1L)) {
      stop(
        "An input ranking flags for more than one perspective within an analysis; ",
        "its cross-level path is ambiguous."
      )
    }
    positive_index <- max.col(positive, ties.method = "first")
    negative_index <- max.col(negative, ties.method = "first")
    has_positive <- rowSums(positive) == 1L
    has_negative <- rowSums(negative) == 1L
    data.frame(
      input_id = as.character(input_ids),
      analysis = analysis_name,
      status = ifelse(
        has_positive, "agreeing", ifelse(has_negative, "opposing", "undecided")
      ),
      perspective = ifelse(
        has_positive,
        perspective_ids[positive_index],
        ifelse(has_negative, perspective_ids[negative_index], NA_character_)
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  mapped_levels <- lapply(seq_along(levels), function(level_index) {
    names_at_level <- analysis_names(levels[[level_index]], names(levels)[level_index])
    maps <- Map(result_map, levels[[level_index]], names_at_level)
    combined <- do.call(rbind, maps)
    rownames(combined) <- NULL
    duplicated_inputs <- unique(combined$input_id[duplicated(combined$input_id)])
    if (length(duplicated_inputs)) {
      stop(
        "Input identifiers must be unique within each analytical level. ",
        "Duplicated identifiers: ", paste(duplicated_inputs, collapse = ", "), "."
      )
    }
    combined
  })

  perspective_catalog <- do.call(rbind, lapply(seq_along(levels), function(level_index) {
    names_at_level <- analysis_names(levels[[level_index]], names(levels)[level_index])
    do.call(rbind, Map(function(result, analysis_name) {
      ids <- rownames(result$perspectives)
      if (is.null(ids) || length(ids) != nrow(result$perspectives) ||
          any(!nzchar(ids))) {
        ids <- colnames(result$`Q method results`$flagged)
      }
      if (is.null(ids) || length(ids) != nrow(result$perspectives) ||
          any(!nzchar(ids))) {
        ids <- paste0(analysis_name, "_f", seq_len(nrow(result$perspectives)))
      }
      data.frame(
        Level = names(levels)[level_index],
        Analysis = analysis_name,
        Perspective = as.character(ids),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, levels[[level_index]], names_at_level))
  }))
  rownames(perspective_catalog) <- NULL
  perspective_ids <- unique(perspective_catalog$Perspective)
  all_inputs <- do.call(rbind, lapply(seq_along(mapped_levels), function(index) {
    map <- mapped_levels[[index]]
    map$level_index <- index
    map
  }))
  raw_inputs <- all_inputs[!all_inputs$input_id %in% perspective_ids, , drop = FALSE]
  raw_inputs <- raw_inputs[!duplicated(raw_inputs$input_id), , drop = FALSE]
  if (!nrow(raw_inputs)) {
    stop("No original input rankings could be identified across the supplied levels.")
  }
  output <- data.frame(
    `Input ranking` = raw_inputs$input_id,
    `Source analysis` = raw_inputs$analysis,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  current_input <- raw_inputs$input_id
  paths <- vector("list", nrow(raw_inputs))

  add_level_columns <- function(level_index, matches) {
    level_name <- names(levels)[level_index]
    output[[paste0(level_name, " analysis")]] <<- matches$analysis
    output[[paste0(level_name, " status")]] <<- matches$status
    output[[paste0(level_name, " perspective")]] <<- matches$perspective
  }

  for (level_index in seq_along(levels)) {
      mapping <- mapped_levels[[level_index]]
      matched_rows <- rep(NA_integer_, nrow(output))
      for (row in seq_len(nrow(output))) {
        candidates <- unique(c(current_input[row], output$`Input ranking`[row]))
        candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
        candidate_matches <- match(candidates, mapping$input_id, nomatch = 0L)
        candidate_matches <- candidate_matches[candidate_matches > 0L]
        if (length(candidate_matches)) matched_rows[row] <- candidate_matches[1L]
      }
      matches <- data.frame(
        analysis = rep(NA_character_, nrow(output)),
        status = rep(NA_character_, nrow(output)),
        perspective = rep(NA_character_, nrow(output)),
        stringsAsFactors = FALSE
      )
      found <- !is.na(matched_rows)
      if (any(found)) {
        matches[found, ] <- mapping[
          matched_rows[found], c("analysis", "status", "perspective"), drop = FALSE
        ]
      }
      add_level_columns(level_index, matches)
      agreeing <- found & matches$status == "agreeing"
      for (row in which(agreeing)) {
        paths[[row]] <- c(paths[[row]], matches$perspective[row])
        current_input[row] <- matches$perspective[row]
      }
      current_input[found & !agreeing] <- NA_character_
  }

  output$`Agreement path` <- vapply(paths, function(path) {
    if (!length(path)) NA_character_ else paste(path, collapse = " > ")
  }, character(1))

  expected_ids <- unique(raw_inputs$input_id)
  complete <- !anyDuplicated(output$`Input ranking`) &&
    length(output$`Input ranking`) == length(expected_ids) &&
    setequal(output$`Input ranking`, expected_ids)
  if (!complete) {
    warning(
      "The cross-level agreement output is incomplete: original input rankings ",
      "are missing or duplicated in the path table."
    )
  }

  underlying_counts <- perspective_catalog
  underlying_counts$`Underlying agreeing rankings` <- vapply(
    underlying_counts$Perspective,
    function(perspective) {
      sum(vapply(paths, function(path) perspective %in% path, logical(1)))
    },
    integer(1)
  )

  terminal_perspective <- vapply(paths, function(path) {
    if (!length(path)) NA_character_ else path[length(path)]
  }, character(1))
  terminal_values <- unique(terminal_perspective)
  terminal_summary <- do.call(rbind, lapply(terminal_values, function(perspective) {
    no_agreement <- is.na(perspective)
    count <- if (no_agreement) {
      sum(is.na(terminal_perspective))
    } else {
      sum(terminal_perspective == perspective, na.rm = TRUE)
    }
    catalog_row <- if (no_agreement) {
      NA_integer_
    } else {
      match(perspective, perspective_catalog$Perspective)
    }
    data.frame(
      `Terminal level` = if (is.na(catalog_row)) NA_character_ else
        perspective_catalog$Level[catalog_row],
      `Terminal analysis` = if (is.na(catalog_row)) NA_character_ else
        perspective_catalog$Analysis[catalog_row],
      `Terminal perspective` = if (no_agreement) "No agreement" else perspective,
      `Input rankings` = count,
      Share = count / nrow(output),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
  rownames(terminal_summary) <- NULL

  not_agreeing_ids <- output$`Input ranking`[is.na(output$`Agreement path`)]
  ranking_value_lookup <- list()
  for (level_index in seq_along(levels)) {
    for (analysis_index in seq_along(levels[[level_index]])) {
      result_object <- levels[[level_index]][[analysis_index]]
      q <- result_object$`Q method results`
      dataset <- as.matrix(q$dataset)
      input_ids <- rownames(q$flagged)
      if (is.null(input_ids) || length(input_ids) != ncol(dataset) ||
          any(!nzchar(input_ids))) {
        input_ids <- colnames(dataset)
      }
      if (is.null(input_ids) || length(input_ids) != ncol(dataset)) next
      statement_ids <- rownames(dataset)
      if (is.null(statement_ids) || length(statement_ids) != nrow(dataset) ||
          any(!nzchar(statement_ids))) {
        statement_ids <- paste0("Statement ", seq_len(nrow(dataset)))
      }
      available <- intersect(not_agreeing_ids, input_ids)
      for (ranking_id in available) {
        if (is.null(ranking_value_lookup[[ranking_id]])) {
          ranking_value_lookup[[ranking_id]] <- stats::setNames(
            as.numeric(dataset[, match(ranking_id, input_ids)]),
            statement_ids
          )
        }
      }
    }
  }
  statement_ids <- unique(unlist(lapply(ranking_value_lookup, names), use.names = FALSE))
  ranking_values <- matrix(
    NA_real_, nrow = length(not_agreeing_ids), ncol = length(statement_ids),
    dimnames = list(not_agreeing_ids, statement_ids)
  )
  for (ranking_id in intersect(not_agreeing_ids, names(ranking_value_lookup))) {
    values <- ranking_value_lookup[[ranking_id]]
    ranking_values[ranking_id, names(values)] <- values
  }
  ranking_values <- as.data.frame(
    ranking_values, stringsAsFactors = FALSE, check.names = FALSE
  )
  missing_values <- setdiff(not_agreeing_ids, names(ranking_value_lookup))
  if (length(missing_values)) {
    warning(
      "Statement-ranking values could not be recovered for: ",
      paste(missing_values, collapse = ", "), "."
    )
  }

  result <- list(
    paths = output,
    underlying_agreement_counts = underlying_counts,
    terminal_path_summary = terminal_summary,
    rankings_not_agreeing = list(
      count = length(not_agreeing_ids),
      rankings = not_agreeing_ids,
      ranking_values = ranking_values
    )
  )
  if (isTRUE(print_table)) {
    print(result$paths, row.names = FALSE)
    print(result$underlying_agreement_counts, row.names = FALSE)
    print(result$terminal_path_summary, row.names = FALSE)
    print(result$rankings_not_agreeing)
  }
  result
}

.extract_bootstrap_cps_batch <- function(results, bootstrap_results) {
  full_results <- bootstrap_results$full.bts.res
  factor_count <- length(full_results)
  statement_count <- length(results$`cp-scores`)
  requested_steps <- ncol(full_results[[1L]]$zsc)
  scores <- vector("list", requested_steps)

  for (step in seq_len(requested_steps)) {
    eigenvalues <- vapply(seq_len(factor_count), function(factor) {
      loadings <- full_results[[factor]]$loa[, step]
      if (all(is.na(loadings))) return(NA_real_)
      sum(loadings^2, na.rm = TRUE)
    }, numeric(1))
    zscores <- vapply(seq_len(factor_count), function(factor) {
      values <- full_results[[factor]]$zsc[, step]
      if (length(values) != statement_count) {
        rep(NA_real_, statement_count)
      } else {
        values
      }
    }, numeric(statement_count))
    if (any(!is.finite(eigenvalues)) || sum(eigenvalues) <= 0 ||
        any(!is.finite(zscores))) {
      next
    }
    scores[[step]] <- tryCatch(
      cpscores(zscores, eigenvalues),
      error = function(error) NULL
    )
  }

  list(
    scores = Filter(Negate(is.null), scores),
    requested_steps = requested_steps
  )
}

qaboots <- function(
    results, steps = 40, method = "multiplication", seed = NULL,
    max_batch_steps = 500L, max_attempt_multiplier = 10L,
    progress = interactive()) {
  method <- match.arg(method, c("multiplication", "manual"))
  integer_parameters <- list(
    steps = steps,
    max_batch_steps = max_batch_steps,
    max_attempt_multiplier = max_attempt_multiplier
  )
  valid_integer <- vapply(integer_parameters, function(value) {
    is.numeric(value) && length(value) == 1L && is.finite(value) &&
      value >= 1 && value == as.integer(value)
  }, logical(1))
  if (!all(valid_integer)) {
    stop(
      paste(names(integer_parameters)[!valid_integer], collapse = ", "),
      " must be positive whole numbers."
    )
  }
  steps <- as.integer(steps)
  max_batch_steps <- as.integer(max_batch_steps)
  max_attempt_multiplier <- as.integer(max_attempt_multiplier)
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("progress must be TRUE or FALSE.")
  }
  if (!is.null(seed)) withr::local_seed(as.integer(seed))

  if (method == "multiplication") {
    if (steps < 40L) steps <- 40L
    target_valid_steps <- max(
      1000L,
      ncol(results$`Q method results`$dataset) * steps
    )
  } else {
    target_valid_steps <- steps
  }
  max_requested_steps <- target_valid_steps * max_attempt_multiplier

  collected_scores <- list()
  bootstrap_batches <- list()
  batch_diagnostics <- list()
  batch_alignment_methods <- character()
  requested_steps <- 0L
  generated_valid_steps <- 0L
  batch_count <- 0L
  next_batch_steps <- min(max_batch_steps, target_valid_steps)
  progress_bar <- if (isTRUE(progress)) {
    utils::txtProgressBar(min = 0, max = target_valid_steps, style = 3)
  } else NULL
  if (!is.null(progress_bar)) on.exit(close(progress_bar), add = TRUE)
  progress_count <- 0L
  progress_callback <- if (!is.null(progress_bar)) function() {
    progress_count <<- min(progress_count + 1L, max(0L, target_valid_steps - 1L))
    utils::setTxtProgressBar(progress_bar, progress_count)
  } else NULL

  while (generated_valid_steps < target_valid_steps &&
         requested_steps < max_requested_steps) {
    batch_count <- batch_count + 1L
    remaining_attempts <- max_requested_steps - requested_steps
    batch_steps <- min(next_batch_steps, remaining_attempts)
    batch_seed <- if (is.null(seed)) NULL else as.integer(seed) + batch_count - 1L
    if (!is.null(batch_seed)) set.seed(batch_seed)

    bootstrap_run <- .run_aligned_bootstrap_with_diagnostics(
      results$`Q method results`$dataset,
      qmethod_result = results$`Q method results`,
      nsteps = batch_steps,
      progress_callback = progress_callback
    )
    batch_alignment_methods[batch_count] <- bootstrap_run$alignment_method
    batch <- bootstrap_run$result
    condition_types <- c(
      rep("warning", length(bootstrap_run$warnings)),
      rep("message", length(bootstrap_run$messages))
    )
    condition_text <- c(bootstrap_run$warnings, bootstrap_run$messages)
    batch_diagnostics[[batch_count]] <- data.frame(
      Batch = rep(batch_count, length(condition_text)),
      Type = condition_types,
      Condition = condition_text,
      check.names = FALSE,
      row.names = NULL
    )
    bootstrap_batches[[batch_count]] <- batch
    extracted <- .extract_bootstrap_cps_batch(results, batch)
    requested_steps <- requested_steps + extracted$requested_steps
    if (length(extracted$scores)) {
      collected_scores <- c(collected_scores, extracted$scores)
      generated_valid_steps <- length(collected_scores)
      if (!is.null(progress_bar)) {
        utils::setTxtProgressBar(progress_bar, min(generated_valid_steps, max(0L, target_valid_steps - 1L)))
      }
    }

    if (generated_valid_steps < target_valid_steps) {
      observed_rate <- generated_valid_steps / requested_steps
      remaining_valid <- target_valid_steps - generated_valid_steps
      estimated_attempts <- ceiling(
        remaining_valid / max(observed_rate, 0.05) * 1.05
      )
      next_batch_steps <- min(
        max_batch_steps, max(40L, estimated_attempts)
      )
    }
  }
  if (!is.null(progress_bar) && generated_valid_steps >= target_valid_steps) {
    utils::setTxtProgressBar(progress_bar, target_valid_steps)
  }

  if (generated_valid_steps < target_valid_steps) {
    stop(
      "Only ", generated_valid_steps, " valid bootstrap iterations were ",
      "produced after ", requested_steps, " attempts; the target was ",
      target_valid_steps, ". Try a different number of perspectives or ",
      "increase max_attempt_multiplier."
    )
  }

  score_matrix <- do.call(
    cbind, collected_scores[seq_len(target_valid_steps)]
  )
  rownames(score_matrix) <- names(results$`cp-scores`)
  colnames(score_matrix) <- paste0("valid_step", seq_len(target_valid_steps))
  discarded_steps <- requested_steps - generated_valid_steps
  bootstrap_diagnostics <- .combine_qmboots_diagnostics(batch_diagnostics)
  bootstrap_diagnostics$target_valid_steps <- target_valid_steps
  bootstrap_diagnostics$requested_steps <- requested_steps
  bootstrap_diagnostics$generated_valid_steps <- generated_valid_steps
  bootstrap_diagnostics$discarded_steps <- discarded_steps
  bootstrap_diagnostics$alignment_method <- if (any(grepl(
      "fallback", batch_alignment_methods, fixed = TRUE
  ))) {
    "qindtest with orthogonal Procrustes fallback"
  } else {
    unique(batch_alignment_methods)[1L]
  }
  bootstrap_diagnostics$alignment_methods_by_batch <- data.frame(
    Batch = seq_along(batch_alignment_methods),
    Method = unname(batch_alignment_methods),
    check.names = FALSE
  )
  bootstrap_diagnostics$factor_count <-
    results$`Q method results`$brief$nfactors
  .signal_qmboots_diagnostics(
    bootstrap_diagnostics,
    context = "The consensus priority scores' bootstrap",
    diagnostics_path = "validation$diagnostics$`consensus priority scores`$initial"
  )
  cps_result <- list(
    "bootstrap cp-scores" = score_matrix,
    "weighted z-scores" = results$`weighted z-scores`,
    "cp-scores" = results$`cp-scores`,
    "diagnostics" = results$diagnostics,
    "bootstrap diagnostics" = bootstrap_diagnostics,
    "alignment method" = bootstrap_run$alignment_method,
    "target valid steps" = target_valid_steps,
    "requested steps" = requested_steps,
    "valid steps" = target_valid_steps,
    "generated valid steps" = generated_valid_steps,
    "discarded steps" = discarded_steps,
    "unused valid steps" = generated_valid_steps - target_valid_steps,
    "batches" = batch_count
  )

  list(
    "dataset name" = results$`dataset name`,
    "Q approach results" = results,
    "diagnostics" = results$diagnostics,
    "bootstrap diagnostics" = bootstrap_diagnostics,
    "alignment method" = bootstrap_run$alignment_method,
    # Retained for compatibility with code written for the original qaboots().
    "bootstrap results" = bootstrap_batches[[1L]],
    "bootstrap batches" = bootstrap_batches,
    "bootstrap consensus priority scores" = cps_result
  )
}

bootstrap_consensus_priority_scores <- function(
    results, steps = NULL, seed = NULL, target_valid_steps = NULL,
    valid_steps_per_ranking = 40L, max_batch_steps = 500L,
    max_attempt_multiplier = 10L, progress = interactive()) {
  if (!is.null(steps) && !is.null(target_valid_steps)) {
    stop("Supply either steps or target_valid_steps, not both.")
  }
  manual_target <- if (!is.null(target_valid_steps)) {
    target_valid_steps
  } else {
    steps
  }
  if (is.null(manual_target)) {
    bootstrap <- qaboots(
      results,
      steps = valid_steps_per_ranking,
      method = "multiplication",
      seed = seed,
      max_batch_steps = max_batch_steps,
      max_attempt_multiplier = max_attempt_multiplier,
      progress = progress
    )
  } else {
    bootstrap <- qaboots(
      results,
      steps = manual_target,
      method = "manual",
      seed = seed,
      max_batch_steps = max_batch_steps,
      max_attempt_multiplier = max_attempt_multiplier,
      progress = progress
    )
  }
  bootstrap$`bootstrap consensus priority scores`
}


# Consensus-priority-score validation summaries

#' Create a bootstrap validation table for consensus priority scores
#'
#' Summarises the standard and bootstrapped consensus priority scores for all
#' statements. The validation interpretation is descriptive: it classifies
#' statements using their bootstrap probabilities of appearing in the top and
#' bottom ranks. Bottom-rank probabilities are always used for the descriptive
#' assessment, but their columns can optionally be included in the table.
#'
#' `x` may be the three-part result returned by `validate()`, a complete result
#' with a `"bootstrap consensus priority scores"` element, or the object
#' returned by `bootstrap_consensus_priority_scores()` itself. For a
#' `validate()` result, the function reads the second validation procedure.
#'
#' @param x A qapproach() result or bootstrap consensus-priority-score result.
#' @param statement_labels Optional labels in statement order. If omitted,
#'   names from the standard scores or bootstrap matrix row names are used.
#' @param confidence_level Confidence level for score and rank intervals.
#' @param rank_cutoffs Positive whole-number rank cutoffs for the stability
#'   probabilities. The default reports top 1, top 3, and top 5 probabilities.
#' @param include_bottom Logical; if `TRUE`, also report the corresponding
#'   bottom-rank probability columns. These probabilities are always calculated
#'   and used for the lower-priority assessments.
#' @return A data frame with one row per statement.
#' @noRd
.consensus_priority_validation_table <- function(
    result, statement_labels = NULL, confidence_level = 0.95,
    rank_cutoffs = c(1L, 3L, 5L), include_bottom = FALSE) {
  if (!is.list(result)) stop("x must be a Q approach or bootstrap result list.")

  bootstrap <- result$`consensus priority score stability`
  if (is.null(bootstrap)) {
    bootstrap <- result$`bootstrap consensus priority scores`
  }
  if (is.null(bootstrap)) bootstrap <- result

  bootstrap_scores <- bootstrap$`bootstrap cp-scores`
  original_scores <- bootstrap$`cp-scores`
  if (is.null(bootstrap_scores) || is.null(original_scores)) {
    stop(
      "The input result must contain bootstrap cp-scores and standard cp-scores, either ",
      "directly or in 'bootstrap consensus priority scores'."
    )
  }

  bootstrap_scores <- as.matrix(bootstrap_scores)
  storage.mode(bootstrap_scores) <- "double"
  original_scores <- as.numeric(original_scores)
  statement_count <- length(original_scores)

  if (nrow(bootstrap_scores) != statement_count &&
      ncol(bootstrap_scores) == statement_count) {
    bootstrap_scores <- t(bootstrap_scores)
  }
  if (nrow(bootstrap_scores) != statement_count) {
    stop("The bootstrap scores must have one row per standard score.")
  }
  if (!ncol(bootstrap_scores)) stop("No bootstrap iterations were supplied.")
  if (any(!is.finite(original_scores)) ||
      any(!is.finite(bootstrap_scores))) {
    stop("Standard and bootstrap scores must contain only finite values.")
  }
  if (!is.numeric(confidence_level) || length(confidence_level) != 1L ||
      !is.finite(confidence_level) || confidence_level <= 0 ||
      confidence_level >= 1) {
    stop("confidence_level must be one number between 0 and 1.")
  }
  if (!is.numeric(rank_cutoffs) || !length(rank_cutoffs) ||
      any(!is.finite(rank_cutoffs)) || any(rank_cutoffs < 1) ||
      any(rank_cutoffs != as.integer(rank_cutoffs))) {
    stop("rank_cutoffs must contain positive whole numbers.")
  }
  if (!is.logical(include_bottom) || length(include_bottom) != 1L ||
      is.na(include_bottom)) {
    stop("include_bottom must be TRUE or FALSE.")
  }
  rank_cutoffs <- sort(unique(as.integer(rank_cutoffs)))
  rank_cutoffs <- rank_cutoffs[rank_cutoffs <= statement_count]
  if (!length(rank_cutoffs)) {
    stop("At least one rank_cutoff must not exceed the number of statements.")
  }

  score_names <- names(bootstrap$`cp-scores`)
  if (is.null(statement_labels)) {
    statement_labels <- score_names
    if (is.null(statement_labels) || any(!nzchar(statement_labels))) {
      statement_labels <- rownames(bootstrap_scores)
    }
    if (is.null(statement_labels) || any(!nzchar(statement_labels))) {
      statement_labels <- paste0("stat", seq_len(statement_count))
    }
  }
  if (length(statement_labels) != statement_count ||
      anyNA(statement_labels) || any(!nzchar(as.character(statement_labels)))) {
    stop("statement_labels must contain one non-empty label per statement.")
  }

  alpha <- (1 - confidence_level) / 2
  interval_probabilities <- c(alpha, 1 - alpha)
  bootstrap_mean <- rowMeans(bootstrap_scores)
  bootstrap_se <- apply(bootstrap_scores, 1L, stats::sd)
  score_intervals <- t(apply(
    bootstrap_scores, 1L, stats::quantile,
    probs = interval_probabilities, names = FALSE, type = 7
  ))

  bootstrap_ranks <- apply(
    bootstrap_scores, 2L, function(scores) {
      rank(-scores, ties.method = "average")
    }
  )
  if (is.null(dim(bootstrap_ranks))) {
    bootstrap_ranks <- matrix(bootstrap_ranks, ncol = 1L)
  }
  rank_intervals <- t(apply(
    bootstrap_ranks, 1L, stats::quantile,
    probs = interval_probabilities, names = FALSE, type = 7
  ))
  median_rank <- apply(bootstrap_ranks, 1L, stats::median)
  probability_top <- vapply(rank_cutoffs, function(cutoff) {
    rowMeans(bootstrap_ranks <= cutoff)
  }, numeric(statement_count))
  if (is.null(dim(probability_top))) {
    probability_top <- matrix(probability_top, ncol = 1L)
  }
  colnames(probability_top) <- paste0("P(top ", rank_cutoffs, ")")

  probability_bottom <- vapply(rank_cutoffs, function(cutoff) {
    boundary <- statement_count - cutoff + 1L
    rowMeans(bootstrap_ranks >= boundary)
  }, numeric(statement_count))
  if (is.null(dim(probability_bottom))) {
    probability_bottom <- matrix(probability_bottom, ncol = 1L)
  }
  colnames(probability_bottom) <- paste0("P(bottom ", rank_cutoffs, ")")

  probability_at <- function(probabilities, cutoff) {
    column <- match(cutoff, rank_cutoffs)
    if (is.na(column)) return(rep(NA_real_, statement_count))
    probabilities[, column]
  }
  top_1 <- probability_at(probability_top, 1L)
  top_3 <- probability_at(probability_top, 3L)
  top_5 <- probability_at(probability_top, 5L)
  bottom_1 <- probability_at(probability_bottom, 1L)
  bottom_3 <- probability_at(probability_bottom, 3L)
  bottom_5 <- probability_at(probability_bottom, 5L)
  informative_cutoffs <- rank_cutoffs[rank_cutoffs < statement_count]
  broad_cutoff <- if (length(informative_cutoffs)) {
    max(informative_cutoffs)
  } else {
    NA_integer_
  }
  broad_top <- if (is.na(broad_cutoff)) {
    rep(NA_real_, statement_count)
  } else {
    probability_at(probability_top, broad_cutoff)
  }
  broad_bottom <- if (is.na(broad_cutoff)) {
    rep(NA_real_, statement_count)
  } else {
    probability_at(probability_bottom, broad_cutoff)
  }

  interpretation <- vapply(seq_len(statement_count), function(index) {
    score <- original_scores[index]

    if (!is.na(top_1[index]) && top_1[index] >= 0.90) {
      return("Robust first priority")
    }
    if (!is.na(top_3[index]) && top_3[index] >= 0.90) {
      return("Robust core priority")
    }
    if (!is.na(broad_top[index]) && broad_top[index] >= 0.90) {
      return("Robust upper priority")
    }
    if (!is.na(broad_top[index]) && broad_top[index] >= 0.75) {
      return("Likely upper priority")
    }
    if (!is.na(bottom_1[index]) && bottom_1[index] >= 0.90) {
      return("Robust last priority")
    }
    if (!is.na(bottom_3[index]) && bottom_3[index] >= 0.90) {
      return("Robust bottom-core priority")
    }
    if (!is.na(broad_bottom[index]) && broad_bottom[index] >= 0.90) {
      return("Robust lower priority")
    }
    if (!is.na(broad_bottom[index]) && broad_bottom[index] >= 0.75) {
      return("Likely lower priority")
    }
    if (score > 0.5) return("Tentative upper-half priority")
    if (score < 0.5) return("Tentative lower-half priority")
    "Indeterminate middle priority"
  }, character(1))

  interval_percentage <- formatC(
    confidence_level * 100, format = "fg", digits = 8
  )
  weighted_z_scores <- bootstrap$`weighted z-scores`
  if (is.null(weighted_z_scores)) {
    weighted_z_scores <- stats::qnorm(original_scores)
  }
  if (length(weighted_z_scores) != statement_count ||
      any(!is.finite(weighted_z_scores))) {
    stop("The weighted z-scores are incomplete or malformed.")
  }
  output <- data.frame(
    Statement = as.character(statement_labels),
    cps = original_scores,
    `Weighted z-score` = as.numeric(weighted_z_scores),
    `Bootstrap mean` = bootstrap_mean,
    Bias = bootstrap_mean - original_scores,
    `Bootstrap SE` = bootstrap_se,
    `Score CI lower` = score_intervals[, 1L],
    `Score CI upper` = score_intervals[, 2L],
    `Median rank` = median_rank,
    `Rank CI lower` = rank_intervals[, 1L],
    `Rank CI upper` = rank_intervals[, 2L],
    check.names = FALSE
  )
  for (column in seq_along(rank_cutoffs)) {
    output[[colnames(probability_top)[column]]] <- probability_top[, column]
  }
  if (include_bottom) {
    for (column in seq_along(rank_cutoffs)) {
      output[[colnames(probability_bottom)[column]]] <-
        probability_bottom[, column]
    }
  }
  output$`Validation assessment` <- interpretation

  probability_descriptions <- stats::setNames(
    paste0(
      "Bootstrap-estimated probability that the statement ",
      "appears among the ", rank_cutoffs, " highest-ranked statement",
      ifelse(rank_cutoffs == 1L, ".", "s.")
    ),
    colnames(probability_top)
  )
  if (include_bottom) {
    probability_descriptions <- c(
      probability_descriptions,
      stats::setNames(
        paste0(
          "Bootstrap-estimated probability that the statement ",
          "appears among the ", rank_cutoffs, " lowest-ranked statement",
          ifelse(rank_cutoffs == 1L, ".", "s.")
        ),
        colnames(probability_bottom)
      )
    )
  }
  attr(output, "probability_descriptions") <- probability_descriptions
  broad_top_name <- paste0("P(top ", broad_cutoff, ")")
  broad_bottom_name <- paste0("P(bottom ", broad_cutoff, ")")
  attr(output, "validation_assessment_descriptions") <- c(
    "Robust first priority" = "P(top 1) is at least 90%.",
    "Robust core priority" =
      "P(top 3) is at least 90%, while P(top 1) is below 90%.",
    "Robust upper priority" =
      paste0("P(top 3) or ", broad_top_name, " is at least 90%, after the stricter top-rank criteria."),
    "Likely upper priority" =
      paste0(broad_top_name, " is at least 75% but below 90%."),
    "Indeterminate middle priority" =
      "The cp-score equals 0.5, or no upper or lower criterion is met.",
    "Likely lower priority" =
      paste0(broad_bottom_name, " is at least 75% but below 90%."),
    "Robust lower priority" =
      paste0(
        broad_bottom_name, " is at least 90%, while ",
        if (3L %in% rank_cutoffs) "P(bottom 3)" else
          "the stricter included bottom-rank probability",
        " is below 90%."
      ),
    "Robust bottom-core priority" =
      "P(bottom 3) is at least 90%, while P(bottom 1) is below 90%.",
    "Robust last priority" = "P(bottom 1) is at least 90%."
  )
  attr(output, "broad_priority_cutoff") <- broad_cutoff
  attr(output, "confidence_level") <- confidence_level
  attr(output, "confidence_interval_label") <- paste0(
    interval_percentage, "% bootstrap interval"
  )
  attr(output, "valid_bootstrap_iterations") <- ncol(bootstrap_scores)
  output
}

#' Print the consensus-priority-score validation table
#'
#' Extracts the table already calculated by `validate()`, prints a readable
#' version in the console, and invisibly returns the displayed results for
#' assignment or export. Score and rank confidence intervals are each combined
#' in one column using `[lower, upper]` notation.
#'
#' `Weighted z-score` is the eigenvalue-weighted mean of the statement's
#' z-scores across all group perspectives. Positive values indicate relatively
#' higher prioritization, negative values indicate relatively lower
#' prioritization, and zero represents average prioritization across the group
#' perspectives. Differences between weighted z-scores provide a linear
#' measure of the prioritization gap between statements.
#'
#' @param validation An object returned by `validate()`.
#' @param digits Number of decimal places used for console display.
#' @param print_table Print the formatted heading, metadata, and table. Set to
#'   `FALSE` when only the returned data frame is needed.
#' @param sort_by Column used to sort the returned table. The default is
#'   `"cp-scores"`; use `NULL` to retain the original statement order. Any
#'   returned technical column name may be supplied.
#' @param decreasing Logical; sort in decreasing order.
#' @param include_bottom Include the `P bottom 1`, `P bottom 3`, and
#'   `P bottom 5` columns in the returned, printed, and exported table. The
#'   default `FALSE` hides these columns only. `validate()` always calculates
#'   and stores the bottom-rank probabilities, and the validation assessment
#'   always uses them.
#' @param file Optional CSV path. The default `NULL` writes no file. If the `.csv`
#'   extension is omitted, it is added
#'   automatically. The exported table retains numeric values for further
#'   analysis.
#' @return Invisibly, the validation data frame with merged interval columns.
#' @export
validation_cps <- function(
    validation, digits = 2L, print_table = TRUE, sort_by = "cp-scores",
    decreasing = TRUE, include_bottom = FALSE, file = NULL) {
  if (!is.list(validation) ||
      is.null(validation$`consensus priority score stability`)) {
    stop("validation must be an object returned by validate().")
  }
  raw_table <-
    validation$`consensus priority score stability`$`validation table`
  if (!is.data.frame(raw_table)) {
    stop(
      "validation does not contain a consensus priority score validation ",
      "table."
    )
  }
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) ||
      digits < 0L || digits != as.integer(digits)) {
    stop("digits must be one non-negative whole number.")
  }
  if (!is.logical(print_table) || length(print_table) != 1L ||
      is.na(print_table)) {
    stop("print_table must be TRUE or FALSE.")
  }
  if (!is.logical(include_bottom) || length(include_bottom) != 1L ||
      is.na(include_bottom)) {
    stop("include_bottom must be TRUE or FALSE.")
  }

  formatted_interval <- function(lower, upper) {
    paste0(
      "[",
      formatC(lower, format = "f", digits = as.integer(digits)),
      ", ",
      formatC(upper, format = "f", digits = as.integer(digits)),
      "]"
    )
  }
  table <- raw_table
  if ("cps" %in% names(table) && !"cp-scores" %in% names(table)) {
    table$`cp-scores` <- table$cps
    table$cps <- NULL
  }
  if (!"Weighted z-score" %in% names(table)) {
    table$`Weighted z-score` <- stats::qnorm(table$`cp-scores`)
  }
  table$`Score CI` <- formatted_interval(
    raw_table$`Score CI lower`, raw_table$`Score CI upper`
  )
  table$`Rank CI` <- paste0(
    "[", formatC(raw_table$`Rank CI lower`, format = "f", digits = 0L),
    ", ", formatC(raw_table$`Rank CI upper`, format = "f", digits = 0L), "]"
  )
  leading_columns <- c(
    "Statement", "cp-scores", "Weighted z-score", "Bootstrap mean", "Bias", "Bootstrap SE",
    "Score CI", "Median rank", "Rank CI"
  )
  removed_columns <- c(
    "Score CI lower", "Score CI upper", "Rank CI lower", "Rank CI upper"
  )
  remaining_columns <- setdiff(
    names(table), c(leading_columns, removed_columns)
  )
  table <- table[, c(leading_columns, remaining_columns), drop = FALSE]
  if (!include_bottom) {
    bottom_columns <- grep(
      "^P\\(bottom [0-9]+\\)$", names(table), value = TRUE
    )
    table[bottom_columns] <- NULL
  }
  for (attribute_name in c(
      "probability_descriptions", "validation_assessment_descriptions",
      "broad_priority_cutoff", "confidence_level",
      "confidence_interval_label", "valid_bootstrap_iterations")) {
    attr(table, attribute_name) <- attr(raw_table, attribute_name)
  }
  probability_descriptions <- attr(table, "probability_descriptions")
  if (!is.null(probability_descriptions)) {
    attr(table, "probability_descriptions") <- probability_descriptions[
      names(probability_descriptions) %in% names(table)
    ]
  }
  attr(table, "weighted_z_score_description") <- paste(
    "The eigenvalue-weighted mean of the statement's z-scores across all",
    "group perspectives. Positive values indicate relatively higher",
    "prioritization, negative values indicate relatively lower prioritization,",
    "and zero represents average prioritization across the group perspectives.",
    "Differences between weighted z-scores provide a linear measure of the",
    "prioritization gap between statements."
  )

  table <- .sort_validation_table(table, sort_by, decreasing)
  csv_table <- table
  csv_probability_columns <- grep(
    "^P\\((top|bottom) [0-9]+\\)$", names(csv_table), value = TRUE
  )
  if (length(csv_probability_columns)) {
    names(csv_table)[match(csv_probability_columns, names(csv_table))] <-
      sub(
        "^P\\((top|bottom) ([0-9]+)\\)$",
        "P \\1 \\2",
        csv_probability_columns
      )
  }
  .write_validation_csv(csv_table, file)
  display <- table
  numeric_columns <- vapply(display, is.numeric, logical(1))
  display[numeric_columns] <- lapply(
    display[numeric_columns],
    function(values) formatC(values, format = "f", digits = as.integer(digits))
  )
  display$`Median rank` <- formatC(
    table$`Median rank`, format = "f", digits = 0L
  )
  probability_columns <- grep("^P\\(top [135]\\)$", names(display), value = TRUE)
  if (length(probability_columns)) {
    display[probability_columns] <- lapply(
      display[probability_columns],
      function(values) paste0(formatC(100 * as.numeric(values),
        format = "f", digits = 0L), "%")
    )
    names(display)[match(probability_columns, names(display))] <-
      sub("^P\\(top ([135])\\)$", "P top \\1", probability_columns)
  }

  if (isTRUE(print_table)) {
    confidence_label <- attr(raw_table, "confidence_interval_label")
    valid_steps <- attr(raw_table, "valid_bootstrap_iterations")
    cat("Consensus priority score validation\n")
    if (!is.null(confidence_label) || !is.null(valid_steps)) {
      details <- c(
        if (!is.null(confidence_label)) confidence_label,
        if (!is.null(valid_steps)) {
          paste(valid_steps, "valid bootstrap iterations")
        }
      )
      cat(paste(details, collapse = " | "), "\n\n", sep = "")
    }
    print(display, row.names = FALSE, right = TRUE)
  }
  invisible(table)
}

#' Print the group-perspective stability validation table
#'
#' Creates a one-row-per-perspective overview from the first validation
#' procedure returned by `validate()`. The assessment is descriptive: it flags
#' rank-order agreement below `rank_correlation_threshold` and any statement
#' whose absolute bootstrap z-score bias reaches the threshold supplied to
#' `validate()`.
#'
#' @param validation An object returned by `validate()`.
#' @param digits Number of decimal places used for console display.
#' @param rank_correlation_threshold Minimum Spearman correlation treated as
#'   stable rank-order agreement.
#' @param print_table Print the formatted heading, metadata, table, and any
#'   alignment caution. Set to `FALSE` when only the returned data frame is
#'   needed.
#' @param sort_by Optional returned column name used to sort the table in
#'   decreasing order. Use `NULL` to retain perspective order.
#' @param file Optional CSV path. The default `NULL` writes no file. If the `.csv`
#'   extension is omitted, it is added
#'   automatically. The exported table retains numeric values for further
#'   analysis.
#' @return Invisibly, the detailed perspective-validation data frame.
#' @export
validation_perspectives <- function(
    validation, digits = 2L, rank_correlation_threshold = 0.90,
    print_table = TRUE, sort_by = NULL, file = NULL) {
  if (!is.list(validation) ||
      is.null(validation$`group perspective stability`)) {
    stop("validation must be an object returned by validate().")
  }
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) ||
      digits < 0L || digits != as.integer(digits)) {
    stop("digits must be one non-negative whole number.")
  }
  if (!is.numeric(rank_correlation_threshold) ||
      length(rank_correlation_threshold) != 1L ||
      !is.finite(rank_correlation_threshold) ||
      rank_correlation_threshold < -1 || rank_correlation_threshold > 1) {
    stop("rank_correlation_threshold must be one number from -1 to 1.")
  }
  if (!is.logical(print_table) || length(print_table) != 1L ||
      is.na(print_table)) {
    stop("print_table must be TRUE or FALSE.")
  }

  stability <- validation$`group perspective stability`
  factor_table <- stability$`factor stability table`
  position_changes <- as.matrix(stability$`perspective position changes`)
  zscore_bias <- as.matrix(stability$`absolute z-score bias`)
  zscore_unstable <- as.matrix(
    stability$`z-scores exceeding instability threshold`
  )
  if (!is.data.frame(factor_table) ||
      nrow(position_changes) != nrow(factor_table) ||
      nrow(zscore_bias) != nrow(factor_table) ||
      nrow(zscore_unstable) != nrow(factor_table)) {
    stop("The group-perspective stability result is incomplete or malformed.")
  }

  finite_mean <- function(values) {
    values <- values[is.finite(values)]
    if (length(values)) mean(values) else NA_real_
  }
  finite_max <- function(values) {
    values <- values[is.finite(values)]
    if (length(values)) max(values) else NA_real_
  }
  changed_positions <- rowSums(
    is.finite(position_changes) & position_changes != 0
  )
  mean_position_change <- apply(
    abs(position_changes), 1L, finite_mean
  )
  max_position_change <- apply(
    abs(position_changes), 1L, finite_max
  )
  mean_zscore_bias <- apply(zscore_bias, 1L, finite_mean)
  max_zscore_bias <- apply(zscore_bias, 1L, finite_max)
  unstable_zscores <- rowSums(zscore_unstable == TRUE, na.rm = TRUE)
  available_zscores <- rowSums(!is.na(zscore_unstable))
  unstable_zscore_share <- ifelse(
    available_zscores > 0L, unstable_zscores / available_zscores, NA_real_
  )

  correlation <- factor_table$`Perspective rank correlation`
  rank_warning <- !is.finite(correlation) |
    correlation < rank_correlation_threshold
  zscore_warning <- unstable_zscores > 0L
  assessment <- ifelse(
    rank_warning & zscore_warning,
    "Unstable",
    ifelse(
      rank_warning,
      "Stable z-scores",
      ifelse(
        zscore_warning,
        "Stable rank-order",
        "Stable"
      )
    )
  )

  formatted_number <- function(values) {
    .format_validation_number(values, digits)
  }
  formatted_interval <- function(lower, upper) {
    paste0("[", formatted_number(lower), ", ", formatted_number(upper), "]")
  }

  table <- data.frame(
    Perspective = paste("Perspective", seq_len(nrow(factor_table))),
    Eigenvalue = factor_table$Eigenvalue,
    `Bootstrap eigenvalue mean` =
      factor_table$`Bootstrap eigenvalue mean`,
    `Eigenvalue bias` = factor_table$`Bootstrap eigenvalue mean` -
      factor_table$Eigenvalue,
    `Bootstrap eigenvalue CI` = formatted_interval(
      factor_table$`Bootstrap eigenvalue CI lower`,
      factor_table$`Bootstrap eigenvalue CI upper`
    ),
    `Rankings agreeing` = factor_table$`Rankings agreeing`,
    `Bootstrap agreement mean` = factor_table$`Bootstrap agreeing mean`,
    `Bootstrap agreement CI` = formatted_interval(
      factor_table$`Bootstrap agreeing CI lower`,
      factor_table$`Bootstrap agreeing CI upper`
    ),
    `Rankings opposing` = factor_table$`Rankings opposing`,
    `Bootstrap opposition mean` = factor_table$`Bootstrap opposing mean`,
    `Bootstrap opposition CI` = formatted_interval(
      factor_table$`Bootstrap opposing CI lower`,
      factor_table$`Bootstrap opposing CI upper`
    ),
    `Perspective rank correlation` = correlation,
    `Statements changing position` = changed_positions,
    `Mean absolute position change` = mean_position_change,
    `Maximum absolute position change` = max_position_change,
    `Mean absolute z-score bias` = mean_zscore_bias,
    `Maximum absolute z-score bias` = max_zscore_bias,
    `Statements exceeding z-score threshold` = unstable_zscores,
    `Share exceeding z-score threshold` = unstable_zscore_share,
    `Stability assessment` = assessment,
    check.names = FALSE,
    row.names = NULL
  )
  perspective_column_order <- c(
    "Perspective", "Eigenvalue", "Rankings agreeing", "Rankings opposing",
    "Bootstrap eigenvalue mean", "Eigenvalue bias", "Bootstrap eigenvalue CI",
    "Bootstrap agreement mean", "Bootstrap agreement CI",
    "Bootstrap opposition mean", "Bootstrap opposition CI",
    "Perspective rank correlation", "Statements changing position",
    "Mean absolute position change", "Maximum absolute position change",
    "Mean absolute z-score bias", "Maximum absolute z-score bias",
    "Statements exceeding z-score threshold",
    "Share exceeding z-score threshold", "Stability assessment"
  )
  table <- table[, perspective_column_order, drop = FALSE]
  attr(table, "valid_aligned_iterations") <-
    stability$`valid aligned iterations`
  attr(table, "discarded_iterations") <- stability$`discarded iterations`
  attr(table, "alignment_method") <- stability$`alignment method`
  attr(table, "alignment_caution") <- stability$`alignment caution`
  attr(table, "confidence_level") <- stability$`confidence level`
  attr(table, "zscore_instability_threshold") <-
    stability$`z-score instability threshold`
  attr(table, "rank_correlation_diagnostics") <-
    stability$`perspective rank-correlation diagnostics`
  attr(table, "rank_correlation_threshold") <- rank_correlation_threshold
  attr(table, "stability_assessment_descriptions") <- c(
    "Stable" = paste(
      "The rank correlation meets the selected threshold and no statement",
      "exceeds the z-score-bias threshold."
    ),
    "Stable rank-order" = paste(
      "The rank correlation meets the selected threshold, but at least one",
      "statement exceeds the z-score-bias threshold. The statement order is",
      "stable, while the underlying z-scores require attention."
    ),
    "Stable z-scores" = paste(
      "No statement exceeds the z-score-bias threshold, but the rank",
      "correlation is below the selected threshold. The z-scores are stable,",
      "while the statement order requires attention."
    ),
    "Unstable" = paste(
      "The rank correlation is below the selected threshold and at least one",
      "statement exceeds the z-score-bias threshold. Neither diagnostic",
      "supports stability."
    )
  )

  table <- .sort_validation_table(table, sort_by)
  .write_validation_csv(table, file)
  unstable_summary <- paste0(
    unstable_zscores, "/", available_zscores, " (",
    formatted_number(100 * unstable_zscore_share), "%)"
  )
  display <- data.frame(
    Perspective = table$Perspective,
    `Eigenvalue` = formatted_number(table$Eigenvalue),
    `Bootstrap eigenvalue` =
      formatted_number(table$`Bootstrap eigenvalue mean`),
    `Bootstrap eigenvalue CI` = table$`Bootstrap eigenvalue CI`,
    `Rankings agreeing` = as.character(table$`Rankings agreeing`),
    `Bootstrap agreement` =
      formatted_number(table$`Bootstrap agreement mean`),
    `Bootstrap agreement CI` = table$`Bootstrap agreement CI`,
    `Rankings opposing` = as.character(table$`Rankings opposing`),
    `Bootstrap opposition` =
      formatted_number(table$`Bootstrap opposition mean`),
    `Bootstrap opposition CI` = table$`Bootstrap opposition CI`,
    `Rank correlation` =
      formatted_number(table$`Perspective rank correlation`),
    `Changed positions` = as.character(table$`Statements changing position`),
    `Mean position change` =
      formatted_number(table$`Mean absolute position change`),
    `Mean z-score bias` =
      formatted_number(table$`Mean absolute z-score bias`),
    `Z-scores above threshold` = unstable_summary,
    Assessment = table$`Stability assessment`,
    check.names = FALSE,
    row.names = NULL
  )
  zero_opposition <- is.finite(table$`Bootstrap opposition mean`) &
    table$`Bootstrap opposition mean` == 0
  display$`Bootstrap opposition`[zero_opposition] <- "0"
  zero_opposition_ci <- grepl("^\\[0[.]0+, 0[.]0+\\]$",
    display$`Bootstrap opposition CI`)
  display$`Bootstrap opposition CI`[zero_opposition_ci] <- "[0, 0]"

  if (isTRUE(print_table)) {
    cat("Group perspective stability validation\n")
    details <- c(
      if (!is.null(stability$`confidence level`)) {
        paste0(100 * stability$`confidence level`, "% bootstrap intervals")
      },
      paste(stability$`valid aligned iterations`, "valid aligned iterations"),
      paste(stability$`discarded iterations`, "discarded iterations")
    )
    cat(paste(details, collapse = " | "), "\n\n", sep = "")
    print(display, row.names = FALSE, right = TRUE)
    if (!is.null(stability$`alignment caution`) &&
        !is.na(stability$`alignment caution`)) {
      cat("\nCaution: ", stability$`alignment caution`, "\n", sep = "")
    }
  }
  invisible(table)
}

#' Print the sensitivity comparison with input-ranking means
#'
#' Creates a one-row-per-statement comparison of consensus priority scores
#' with input-ranking means transformed onto the same fixed standard-normal
#' cumulative-probability scale. This is a sensitivity analysis, not a test of
#' whether the consensus priority scores are valid: differences may reflect
#' the intended perspective-based weighting.
#'
#' @details
#' The direction of the rank-change measure intentionally differs from the
#' arithmetic order named in its column heading. Because lower rank numbers
#' indicate higher priority, a positive rank change means that the statement
#' moved upward under the cp-score ranking. Positive values in both the score-
#' difference and rank-change columns therefore indicate a higher cp-score
#' priority relative to the input means. When input means are tied, `Priority
#' comparison` reports `Tied under input means` instead of assigning a
#' directional interpretation.
#'
#' The comparison of the cp-scores with the input means is a sensitivity
#' analysis, not a test of whether the consensus priority scores are valid.
#' Differences can reflect the intended
#' perspective-based weighting: input means treat every ranking equally,
#' whereas cp-scores explicitly represent group perspectives. A small
#' difference may indicate that perspective-based weighting has little effect
#' in the dataset, which is itself informative.
#'
#' @param validation An object returned by `validate()`.
#' @param digits Number of decimal places used for console display.
#' @param print_table Print the formatted heading, metadata, and table. Set to
#'   `FALSE` when only the returned data frame is needed.
#' @param sort_by Column used to sort the returned table. The default is
#'   `"cp-scores"`; use `NULL` to retain the original statement order. Any
#'   returned technical column name may be supplied.
#' @param decreasing Logical; sort in decreasing order.
#' @param file Optional CSV path. The default `NULL` writes no file. If the `.csv`
#'   extension is omitted, it is added
#'   automatically. The exported table retains numeric values for further
#'   analysis.
#' @return Invisibly, the unformatted statement-comparison data frame.
#' @export
validation_means <- function(
    validation, digits = 2L, print_table = TRUE, sort_by = "cp-scores",
    decreasing = TRUE, file = NULL) {
  if (!is.list(validation) ||
      is.null(validation$`input mean sensitivity`)) {
    stop("validation must be an object returned by validate().")
  }
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) ||
      digits < 0L || digits != as.integer(digits)) {
    stop("digits must be one non-negative whole number.")
  }
  if (!is.logical(print_table) || length(print_table) != 1L ||
      is.na(print_table)) {
    stop("print_table must be TRUE or FALSE.")
  }

  sensitivity <- validation$`input mean sensitivity`
  comparison <- sensitivity$`statement comparison table`
  if (!is.data.frame(comparison) ||
      !all(c(
        "Statement", "cps", "Normalized input mean", "cp-scores rank",
        "Input-mean rank", "Rank change (input means minus cp-scores)"
      ) %in% names(comparison))) {
    stop("The input-mean sensitivity result is incomplete or malformed.")
  }

  statement_labels <- comparison$Statement
  score_difference <- comparison$cps - comparison$`Normalized input mean`
  rank_change <- comparison$`Rank change (input means minus cp-scores)`
  input_mean_tied <- duplicated(comparison$`Normalized input mean`) |
    duplicated(comparison$`Normalized input mean`, fromLast = TRUE)
  assessment <- ifelse(
    input_mean_tied,
    "Tied under input means",
    ifelse(
      rank_change > 0,
      "Higher priority under cp-scores",
      ifelse(
        rank_change < 0,
        "Higher priority under input means",
        "Same rank"
      )
    )
  )

  table <- data.frame(
    Statement = as.character(statement_labels),
    `cp-scores` = comparison$cps,
    `Normalized input mean` = comparison$`Normalized input mean`,
    `Score difference (cp-scores minus input means)` = score_difference,
    `Absolute score difference` = abs(score_difference),
    `cp-scores rank` = comparison$`cp-scores rank`,
    `Input-mean rank` = comparison$`Input-mean rank`,
    `Rank change (input means minus cp-scores)` = rank_change,
    `Absolute rank change` = abs(rank_change),
    `Priority comparison` = assessment,
    check.names = FALSE,
    row.names = NULL
  )
  attr(table, "spearman_rank_correlation") <-
    sensitivity$`Spearman rank correlation`
  attr(table, "spearman_rank_correlation_diagnostic") <-
    sensitivity$`Spearman rank-correlation diagnostic`
  attr(table, "top_rank_overlap") <- sensitivity$`top-rank overlap`
  attr(table, "interpretation") <- sensitivity$interpretation
  attr(table, "priority_comparison_descriptions") <- c(
    "Higher priority under cp-scores" = paste(
      "The cp-score ranking places the statement at a higher priority",
      "position than the input means."
    ),
    "Higher priority under input means" = paste(
      "The input-mean ranking places the statement at a higher priority",
      "position than the cp-scores."
    ),
    "Same rank" = paste(
      "The statement has the same numerical rank under both analytical",
      "approaches."
    ),
    "Tied under input means" = paste(
      "The statement shares its input-mean value and ranking position with",
      "one or more other statements. Its comparison with the cp-score rank",
      "may therefore require closer examination."
    )
  )

  table <- .sort_validation_table(table, sort_by, decreasing)
  .write_validation_csv(table, file)
  display <- table
  numeric_columns <- vapply(display, is.numeric, logical(1))
  display[numeric_columns] <- lapply(
    display[numeric_columns],
    function(values) formatC(values, format = "f", digits = as.integer(digits))
  )
  display$`cp-scores rank` <- formatC(
    table$`cp-scores rank`, format = "f", digits = 0L
  )

  correlation <- sensitivity$`Spearman rank correlation`
  overlap <- sensitivity$`top-rank overlap`
  details <- c(
    paste0(
      "Spearman rank correlation: ",
      formatC(correlation, format = "f", digits = as.integer(digits))
    ),
    if (length(overlap)) paste0(
      names(overlap), ": ",
      formatC(100 * overlap, format = "f", digits = as.integer(digits)),
      "%"
    )
  )
  if (isTRUE(print_table)) {
    cat("Consensus priority scores compared with input-ranking means\n")
    cat(paste(details, collapse = " | "), "\n\n", sep = "")
    print(display, row.names = FALSE, right = TRUE)
    correlation_diagnostic <-
      sensitivity$`Spearman rank-correlation diagnostic`
    if (is.data.frame(correlation_diagnostic) &&
        nrow(correlation_diagnostic) == 1L &&
        identical(correlation_diagnostic$Status, "not estimable")) {
      cat("\nNote: ", correlation_diagnostic$Explanation, "\n", sep = "")
    }
  }
  invisible(table)
}

.qapproach_extract_bootstrap_scores <- function(results, bootstrap) {
  embedded <- bootstrap$`bootstrap consensus priority scores`
  if (!is.null(embedded)) {
    return(list(
      scores = embedded$`bootstrap cp-scores`,
      requested_steps = embedded$`requested steps`,
      valid_steps = embedded$`valid steps`,
      discarded_steps = embedded$`discarded steps`
    ))
  }
  raw <- bootstrap$`bootstrap results`
  if (is.null(raw$full.bts.res) || !length(raw$full.bts.res)) {
    stop("bootstrap does not contain full bootstrap results.")
  }
  extracted <- .extract_bootstrap_cps_batch(results, raw)
  score_matrix <- if (length(extracted$scores)) {
    do.call(cbind, extracted$scores)
  } else {
    matrix(
      numeric(), nrow = length(results$`cp-scores`), ncol = 0L
    )
  }
  rownames(score_matrix) <- names(results$`cp-scores`)
  list(
    scores = score_matrix,
    requested_steps = extracted$requested_steps,
    valid_steps = ncol(score_matrix),
    discarded_steps = extracted$requested_steps - ncol(score_matrix)
  )
}

.checked_spearman_correlation <- function(x, y, context) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) {
    stop("Spearman correlation inputs must have the same length.")
  }
  finite <- is.finite(x) & is.finite(y)
  finite_pairs <- sum(finite)
  unique_x <- length(unique(x[finite]))
  unique_y <- length(unique(y[finite]))

  reason <- NULL
  if (finite_pairs < 2L) {
    reason <- paste0(
      "Spearman rank correlation for ", context,
      " was not estimable because fewer than two finite paired ",
      "observations were available."
    )
  } else if (unique_x < 2L || unique_y < 2L) {
    constant_inputs <- c(
      if (unique_x < 2L) "the first input",
      if (unique_y < 2L) "the second input"
    )
    reason <- paste0(
      "Spearman rank correlation for ", context,
      " was not estimable because ",
      paste(constant_inputs, collapse = " and "),
      if (length(constant_inputs) == 1L) " has" else " have",
      " zero variance across the finite paired observations."
    )
  }

  value <- if (is.null(reason)) {
    stats::cor(x[finite], y[finite], method = "spearman")
  } else {
    NA_real_
  }
  explanation <- if (is.null(reason)) {
    paste0(
      "Spearman rank correlation for ", context, " was estimated from ",
      finite_pairs, " finite paired observations."
    )
  } else {
    reason
  }

  list(
    value = value,
    diagnostic = data.frame(
      Status = if (is.null(reason)) "estimated" else "not estimable",
      Explanation = explanation,
      `Finite pairs` = finite_pairs,
      `Unique first-input values` = unique_x,
      `Unique second-input values` = unique_y,
      check.names = FALSE,
      row.names = NULL
    )
  )
}

.qapproach_group_perspective_stability <- function(
    results, bootstrap, confidence_level = 0.95,
    zscore_instability_threshold = 0.2) {
  batches <- bootstrap$`bootstrap batches`
  if (is.null(batches)) batches <- list(bootstrap$`bootstrap results`)
  raw <- batches[[1L]]
  factor_count <- results$`Q method results`$brief$nfactors
  statement_count <- results$`Q method results`$brief$nstat
  factor_names <- paste0("factor", seq_len(factor_count))
  perspective_names <- rownames(results$perspectives)
  if (is.null(perspective_names)) perspective_names <- factor_names
  alpha <- (1 - confidence_level) / 2
  interval_probabilities <- c(alpha, 1 - alpha)

  bootstrap_zscores <- lapply(seq_len(factor_count), function(factor) {
    do.call(cbind, lapply(batches, function(batch) {
      as.matrix(batch$full.bts.res[[factor]]$zsc)
    }))
  })
  bootstrap_average_zscores <- do.call(rbind, lapply(
    bootstrap_zscores, function(zscores) {
      apply(zscores, 1L, function(values) {
        values <- values[is.finite(values)]
        if (length(values)) mean(values) else NA_real_
      })
    }
  ))
  rownames(bootstrap_average_zscores) <- perspective_names
  colnames(bootstrap_average_zscores) <- colnames(results$perspectives)
  ranking_distribution <- distributiondetermination(statement_count)$ranking
  bootstrap_factors <- data.frame(t(apply(
    bootstrap_average_zscores, 1L, function(zscores) {
      if (any(!is.finite(zscores))) return(rep(NA_real_, statement_count))
      output <- numeric(statement_count)
      output[order(zscores, seq_along(zscores))] <- sort(ranking_distribution)
      output
    }
  )), check.names = FALSE)
  rownames(bootstrap_factors) <- perspective_names
  colnames(bootstrap_factors) <- colnames(results$perspectives)
  perspective_changes <- bootstrap_factors - results$perspectives
  perspective_correlation_checks <- lapply(
    seq_len(factor_count), function(factor) {
      .checked_spearman_correlation(
        as.numeric(results$perspectives[factor, ]),
        as.numeric(bootstrap_factors[factor, ]),
        context = paste0("perspective ", factor, " rank-order stability")
      )
    }
  )
  perspective_correlations <- vapply(
    perspective_correlation_checks, `[[`, numeric(1), "value"
  )
  names(perspective_correlations) <- perspective_names
  perspective_correlation_diagnostics <- do.call(rbind, lapply(
    seq_len(factor_count), function(factor) {
      data.frame(
        Perspective = perspective_names[factor],
        perspective_correlation_checks[[factor]]$diagnostic,
        check.names = FALSE,
        row.names = NULL
      )
    }
  ))

  original_zscores <- data.frame(
    t(results$`Q method results`$zsc), check.names = FALSE
  )
  rownames(original_zscores) <- perspective_names
  zscore_bias <- abs(original_zscores - bootstrap_average_zscores)
  zscore_unstable <- zscore_bias >= zscore_instability_threshold

  factor_statistics <- lapply(seq_len(factor_count), function(factor) {
    loadings <- do.call(cbind, lapply(batches, function(batch) {
      as.matrix(batch$full.bts.res[[factor]]$loa)
    }))
    eigenvalues <- colSums(loadings^2, na.rm = TRUE)
    eigenvalues[colSums(is.finite(loadings)) == 0L] <- NA_real_
    flagged <- do.call(cbind, lapply(batches, function(batch) {
      as.matrix(batch$full.bts.res[[factor]]$flagged)
    }))
    agreeing_counts <- colSums(flagged & loadings > 0, na.rm = TRUE)
    opposing_counts <- colSums(flagged & loadings < 0, na.rm = TRUE)
    agreeing_counts[colSums(!is.na(flagged)) == 0L] <- NA_real_
    opposing_counts[colSums(!is.na(flagged)) == 0L] <- NA_real_
    valid_eigenvalues <- eigenvalues[is.finite(eigenvalues)]
    valid_agreeing <- agreeing_counts[is.finite(agreeing_counts)]
    valid_opposing <- opposing_counts[is.finite(opposing_counts)]
    eigen_ci <- if (length(valid_eigenvalues)) {
      stats::quantile(
        valid_eigenvalues, interval_probabilities,
        names = FALSE, type = 7
      )
    } else c(NA_real_, NA_real_)
    agreeing_ci <- if (length(valid_agreeing)) {
      stats::quantile(
        valid_agreeing, interval_probabilities,
        names = FALSE, type = 7
      )
    } else c(NA_real_, NA_real_)
    opposing_ci <- if (length(valid_opposing)) {
      stats::quantile(
        valid_opposing, interval_probabilities,
        names = FALSE, type = 7
      )
    } else c(NA_real_, NA_real_)
    data.frame(
      Perspective = perspective_names[factor],
      Eigenvalue =
        results$`Q method results`$f_char$characteristics$eigenvals[factor],
      `Bootstrap eigenvalue mean` = mean(valid_eigenvalues),
      `Bootstrap eigenvalue SE` = stats::sd(valid_eigenvalues),
      `Bootstrap eigenvalue CI lower` = eigen_ci[1L],
      `Bootstrap eigenvalue CI upper` = eigen_ci[2L],
      `Rankings agreeing` = sum(results$`Q method results`$flagged[, factor] &
        results$`Q method results`$loa[, factor] > 0, na.rm = TRUE),
      `Bootstrap agreeing mean` = mean(valid_agreeing),
      `Bootstrap agreeing CI lower` = agreeing_ci[1L],
      `Bootstrap agreeing CI upper` = agreeing_ci[2L],
      `Rankings opposing` = sum(results$`Q method results`$flagged[, factor] &
        results$`Q method results`$loa[, factor] < 0, na.rm = TRUE),
      `Bootstrap opposing mean` = mean(valid_opposing),
      `Bootstrap opposing CI lower` = opposing_ci[1L],
      `Bootstrap opposing CI upper` = opposing_ci[2L],
      `Perspective rank correlation` = perspective_correlations[factor],
      check.names = FALSE,
      row.names = NULL
    )
  })
  factor_statistics <- do.call(rbind, factor_statistics)

  extracted <- .qapproach_extract_bootstrap_scores(results, bootstrap)
  alignment_method <- bootstrap$`alignment method`
  if (is.null(alignment_method)) {
    alignment_method <- bootstrap$`bootstrap diagnostics`$alignment_method
  }
  if (is.null(alignment_method)) {
    alignment_method <- if (factor_count == 1L) {
      "none (single-factor solution; inferred from legacy bootstrap)"
    } else {
      "qindtest (inferred from legacy bootstrap)"
    }
  }
  alignment_caution <- if (
      factor_count > 3L && !grepl("Procrustes", alignment_method, fixed = TRUE)
  ) {
    paste(
      "This supplied bootstrap does not report orthogonal Procrustes",
      "alignment. Solutions above three perspectives require a general",
      "alignment method before definitive factor-stability conclusions."
    )
  } else {
    NA_character_
  }

  list(
    "factor stability table" = factor_statistics,
    "perspectives" = results$perspectives,
    "bootstrap average perspectives" = bootstrap_factors,
    "perspective position changes" = perspective_changes,
    "perspective rank-correlation diagnostics" =
      perspective_correlation_diagnostics,
    "bootstrap average z-scores" = bootstrap_average_zscores,
    "absolute z-score bias" = zscore_bias,
    "z-scores exceeding instability threshold" = zscore_unstable,
    "factor stability index by batch" = lapply(batches, `[[`, "fsi"),
    "bootstrap loading statistics by batch" =
      lapply(batches, `[[`, "loa.stats"),
    "alignment diagnostics by batch" =
      lapply(batches, `[[`, "indet.tests"),
    "requested bootstrap iterations" = extracted$requested_steps,
    "valid aligned iterations" = extracted$valid_steps,
    "discarded iterations" = extracted$discarded_steps,
    "bootstrap diagnostics" = bootstrap$`bootstrap diagnostics`,
    "alignment method" = alignment_method,
    "alignment caution" = alignment_caution,
    "confidence level" = confidence_level,
    "z-score instability threshold" = zscore_instability_threshold
  )
}

.qapproach_input_mean_sensitivity <- function(
    results, rank_cutoffs = c(1L, 3L, 5L),
    confidence_level = 0.95) {
  original_scores <- results$`cp-scores`
  input_means <- rowMeans(results$`Q method results`$dataset)
  if (length(names(original_scores)) && length(names(input_means))) {
    matching <- match(names(original_scores), names(input_means))
    if (anyNA(matching)) {
      stop("cp-scores and input-ranking means have incompatible statement labels.")
    }
    input_means <- input_means[matching]
  }
  ranking_distribution <- distributiondetermination(nrow(
    results$`Q method results`$dataset
  ))$ranking
  distribution_sd <- stats::sd(ranking_distribution)
  if (!is.finite(distribution_sd) || distribution_sd <= 0) {
    stop("The required ranking distribution must have positive variation.")
  }
  input_mean_z <- (
    input_means - mean(ranking_distribution)
  ) / distribution_sd
  normalized_means <- stats::setNames(
    stats::pnorm(input_mean_z), names(input_means)
  )
  statement_count <- length(original_scores)
  rank_cutoffs <- sort(unique(as.integer(rank_cutoffs)))
  rank_cutoffs <- rank_cutoffs[rank_cutoffs <= statement_count]
  cps_rank <- rank(-original_scores, ties.method = "average")
  mean_rank <- rank(-normalized_means, ties.method = "average")
  comparison <- data.frame(
    Statement = names(original_scores),
    cps = as.numeric(original_scores),
    `Normalized input mean` = as.numeric(normalized_means),
    `cp-scores rank` = as.numeric(cps_rank),
    `Input-mean rank` = as.numeric(mean_rank),
    `Rank change (input means minus cp-scores)` = as.numeric(mean_rank - cps_rank),
    check.names = FALSE
  )
  top_overlap <- vapply(rank_cutoffs, function(cutoff) {
    cps_top <- names(sort(original_scores, decreasing = TRUE))[seq_len(cutoff)]
    mean_top <- names(sort(normalized_means, decreasing = TRUE))[seq_len(cutoff)]
    length(intersect(cps_top, mean_top)) / cutoff
  }, numeric(1))
  names(top_overlap) <- paste0("top ", rank_cutoffs, " overlap")

  correlation_check <- .checked_spearman_correlation(
    original_scores,
    normalized_means,
    context = "consensus priority scores and normalized input-ranking means"
  )

  list(
    "statement comparison table" = comparison,
    "normalized input means" = normalized_means,
    "input-mean z-scores" = input_mean_z,
    "top-rank overlap" = top_overlap,
    "Spearman rank correlation" = correlation_check$value,
    "Spearman rank-correlation diagnostic" = correlation_check$diagnostic,
    "interpretation" = paste(
      "The comparison of the cp-scores with the input means is a sensitivity",
      "analysis, not a test of whether the consensus priority scores are",
      "valid. Differences can reflect the",
      "intended perspective-based weighting: input means treat every ranking",
      "equally, whereas cp-scores explicitly represent group perspectives.",
      "A small difference may indicate that perspective-based weighting has",
      "little effect in this dataset, which is itself informative."
    )
  )
}

#' Validate group perspectives and consensus priority scores
#'
#' Runs three complementary procedures: bootstrap stability of the group
#' perspectives, bootstrap stability of the consensus priority scores and
#' ranks, and a sensitivity comparison with input-ranking means transformed
#' onto the same fixed standard-normal cumulative-probability scale.
#' Bootstrap factor alignment uses orthogonal Procrustes sign alignment for one
#' perspective, `qindtest` for two or three perspectives, and the package's
#' orthogonal Procrustes implementation for larger solutions. If `qindtest`
#' fails, the affected batch is rerun with orthogonal Procrustes alignment and
#' reported with a message referring to this help page. The fallback preserves
#' the intended factor correspondence when `qindtest` cannot provide a unique
#' alignment; its use and the original error are retained in the diagnostics.
#' Recognized underlying conditions, routine alignment corrections, invalid
#' iterations, and discard reasons are retained silently in
#' `validation$diagnostics`. Unclassified conditions are emitted as warnings so
#' that they can be reported and reviewed.
#'
#' @param results An object returned by `qapproach()`.
#' @param bootstrap Optional object returned by `qaboots()`. If omitted, one is
#'   generated for the group-perspective diagnostics and reused for consensus priority scores
#'   validation wherever possible.
#' @param bootstrap_scores Optional object returned by
#'   `bootstrap_consensus_priority_scores()`.
#' @param statement_labels Optional statement labels in analysis order.
#' @param confidence_level Confidence level for bootstrap intervals.
#' @param rank_cutoffs Rank cutoffs used for top probabilities and sensitivity
#'   comparisons; defaults to 1, 3, and 5.
#' @param target_valid_steps Target number of valid bootstrap iterations of the consensus priority scores.
#'   By default this is `valid_steps_per_ranking` times the number of rankings.
#' @param valid_steps_per_ranking Default number of valid iterations per input
#'   ranking.
#' @param seed Integer bootstrap seed or `NULL`. The default `NULL` uses the
#'   current random-number state. Supply an integer, such as `42L`, for a
#'   reproducible validation run.
#' @param max_batch_steps,max_attempt_multiplier Passed to the adaptive
#'   bootstrap of the consensus priority scores when additional valid iterations are required.
#' @param progress Whether to display bootstrap progress. The default uses
#'   `interactive()`, so progress is shown in interactive R sessions and hidden
#'   in non-interactive use. Set explicitly to `TRUE` or `FALSE` to override it.
#' @param zscore_instability_threshold Absolute z-score bias used for the
#'   descriptive instability flag.
#' @return A list containing three validation-procedure results and centralized
#'   diagnostics in `validation$diagnostics`. These diagnostics contain the
#'   alignment methods and fallbacks, recognized bootstrap conditions, invalid
#'   iterations and their discard reasons, and the input-mean correlation
#'   diagnostic.
#' @export
validate <- function(
    results, bootstrap = NULL, bootstrap_scores = NULL,
    statement_labels = NULL, confidence_level = 0.95,
    rank_cutoffs = c(1L, 3L, 5L),
    target_valid_steps = NULL, valid_steps_per_ranking = 40L,
    seed = NULL, max_batch_steps = 500L, max_attempt_multiplier = 10L,
    zscore_instability_threshold = 0.2, progress = interactive()) {
  if (!is.list(results) || is.null(results$`Q method results`) ||
      is.null(results$perspectives) || is.null(results$`cp-scores`)) {
    stop("results must be an object returned by qapproach().")
  }
  ranking_count <- ncol(results$`Q method results`$dataset)
  if (is.null(target_valid_steps)) {
    target_valid_steps <- max(
      1000L,
      valid_steps_per_ranking * ranking_count
    )
  }
  integer_values <- c(
    target_valid_steps = target_valid_steps,
    valid_steps_per_ranking = valid_steps_per_ranking,
    max_batch_steps = max_batch_steps,
    max_attempt_multiplier = max_attempt_multiplier
  )
  valid_integers <- is.finite(integer_values) & integer_values >= 1 &
    integer_values == as.integer(integer_values)
  if (!all(valid_integers)) {
    stop(
      paste(names(integer_values)[!valid_integers], collapse = ", "),
      " must be positive whole numbers."
    )
  }
  target_valid_steps <- as.integer(target_valid_steps)
  if (!is.numeric(zscore_instability_threshold) ||
      length(zscore_instability_threshold) != 1L ||
      !is.finite(zscore_instability_threshold) ||
      zscore_instability_threshold < 0) {
    stop("zscore_instability_threshold must be one non-negative number.")
  }

  if (is.null(bootstrap)) {
    bootstrap <- qaboots(
      results, steps = target_valid_steps,
      method = "manual", seed = seed,
      max_batch_steps = max_batch_steps,
      max_attempt_multiplier = max_attempt_multiplier,
      progress = progress
    )
  }
  if (!is.list(bootstrap) || is.null(bootstrap$`bootstrap results`)) {
    stop("bootstrap must be an object returned by qaboots().")
  }

  procedure_1 <- .qapproach_group_perspective_stability(
    results, bootstrap,
    confidence_level = confidence_level,
    zscore_instability_threshold = zscore_instability_threshold
  )

  initial_scores <- if (is.null(bootstrap_scores)) {
    embedded <- bootstrap$`bootstrap consensus priority scores`
    if (!is.null(embedded)) {
      embedded
    } else {
      extracted <- .qapproach_extract_bootstrap_scores(results, bootstrap)
      list(
        "bootstrap cp-scores" = extracted$scores,
        "weighted z-scores" = results$`weighted z-scores`,
        "cp-scores" = results$`cp-scores`,
        "requested steps" = extracted$requested_steps,
        "generated valid steps" = extracted$valid_steps,
        "discarded steps" = extracted$discarded_steps,
        "batches" = 1L
      )
    }
  } else {
    bootstrap_scores
  }
  score_matrix <- as.matrix(initial_scores$`bootstrap cp-scores`)
  if (nrow(score_matrix) != length(results$`cp-scores`) &&
      ncol(score_matrix) == length(results$`cp-scores`)) {
    score_matrix <- t(score_matrix)
  }
  if (nrow(score_matrix) != length(results$`cp-scores`)) {
    stop("bootstrap_scores must contain one row per statement.")
  }
  finite_columns <- if (ncol(score_matrix)) {
    apply(score_matrix, 2L, function(values) all(is.finite(values)))
  } else logical()
  score_matrix <- score_matrix[, finite_columns, drop = FALSE]

  additional <- NULL
  if (ncol(score_matrix) < target_valid_steps) {
    remaining <- target_valid_steps - ncol(score_matrix)
    additional_seed <- if (is.null(seed)) NULL else as.integer(seed) + 100000L
    additional <- bootstrap_consensus_priority_scores(
      results,
      target_valid_steps = remaining,
      seed = additional_seed,
      max_batch_steps = max_batch_steps,
      max_attempt_multiplier = max_attempt_multiplier,
      progress = progress
    )
    score_matrix <- cbind(
      score_matrix, additional$`bootstrap cp-scores`
    )
  }
  initial_generated <- initial_scores$`generated valid steps`
  if (is.null(initial_generated)) initial_generated <- ncol(
    initial_scores$`bootstrap cp-scores`
  )
  additional_generated <- if (is.null(additional)) 0L else
    additional$`generated valid steps`
  generated_valid_steps <- initial_generated + additional_generated
  score_matrix <- score_matrix[, seq_len(target_valid_steps), drop = FALSE]
  rownames(score_matrix) <- names(results$`cp-scores`)
  colnames(score_matrix) <- paste0("valid_step", seq_len(target_valid_steps))

  initial_requested <- initial_scores$`requested steps`
  if (is.null(initial_requested)) initial_requested <- ncol(
    initial_scores$`bootstrap cp-scores`
  )
  additional_requested <- if (is.null(additional)) 0L else
    additional$`requested steps`
  initial_discarded <- initial_scores$`discarded steps`
  if (is.null(initial_discarded)) initial_discarded <- 0L
  additional_discarded <- if (is.null(additional)) 0L else
    additional$`discarded steps`
  initial_batches <- initial_scores$batches
  if (is.null(initial_batches)) initial_batches <- 1L
  additional_batches <- if (is.null(additional)) 0L else additional$batches

  procedure_2 <- list(
    "bootstrap cp-scores" = score_matrix,
    "weighted z-scores" = results$`weighted z-scores`,
    "cp-scores" = results$`cp-scores`,
    "diagnostics" = results$diagnostics,
    "bootstrap diagnostics" = list(
      initial = if (is.null(initial_scores$`bootstrap diagnostics`)) {
        bootstrap$`bootstrap diagnostics`
      } else {
        initial_scores$`bootstrap diagnostics`
      },
      additional = if (is.null(additional)) NULL else
        additional$`bootstrap diagnostics`
    ),
    "target valid steps" = target_valid_steps,
    "requested steps" = initial_requested + additional_requested,
    "valid steps" = target_valid_steps,
    "generated valid steps" = generated_valid_steps,
    "discarded steps" = initial_discarded + additional_discarded,
    "unused valid steps" = generated_valid_steps - target_valid_steps,
    "batches" = initial_batches + additional_batches
  )

  procedure_3 <- .qapproach_input_mean_sensitivity(
    results,
    rank_cutoffs = rank_cutoffs,
    confidence_level = confidence_level
  )
  validation <- list(
    "group perspective stability" = procedure_1,
    "consensus priority score stability" = procedure_2,
    "input mean sensitivity" = procedure_3,
    "diagnostics" = list(
      "perspectives" = procedure_1$`bootstrap diagnostics`,
      "consensus priority scores" = procedure_2$`bootstrap diagnostics`,
      "means" = procedure_3$`Spearman rank-correlation diagnostic`
    )
  )
  validation[[2L]]$`validation table` <-
    .consensus_priority_validation_table(
      validation,
      statement_labels = statement_labels,
      confidence_level = confidence_level,
      rank_cutoffs = rank_cutoffs,
      include_bottom = TRUE
    )
  validation
}
