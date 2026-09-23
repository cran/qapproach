############################################################################
### Main Q approach analysis and data-preparation functions              ###
############################################################################
### https://doi.org/10.5281/zenodo.11518485                              ###
############################################################################
### Generalized implementation supporting any statement count.           ###
############################################################################


#' Determine the required Q-sort ranking distribution
#'
#' Returns the fixed ranking distribution used by the Q approach for a given
#' number of statements. This distribution provides the basis for preparing
#' data-collection materials and validating completed rankings.
#'
#' @param nstat Number of statements. Must be one whole number of at least 3.
#' @return A list containing the number of statements per ranking value
#'   (`distr`), the ranking values (`values`), and the complete ranking
#'   gradient (`ranking`).
#' @export
distributiondetermination <- function(nstat){
  if (!is.numeric(nstat) || length(nstat) != 1L || is.na(nstat) ||
      nstat < 3 || nstat != as.integer(nstat)) {
    stop("nstat must be a single integer of at least 3.")
  }
  nstat <- as.integer(nstat)

  bins <- round(nstat/5)
  if(nstat > 29){bins <- round(nstat/8)}
  distr <-suppressWarnings(qmethod::make.distribution(nstat = nstat, max.bin = bins))
  # sum(distr)
  if(sum(distr) != nstat){
    if((nstat - sum(distr)) == 1){distr[ceiling(length(distr)/2)] <- distr[ceiling(length(distr)/2)] + 1}
    else {
      bins <- bins - 1
      distr <-suppressWarnings(qmethod::make.distribution(nstat = nstat, max.bin = bins))
      if((nstat - sum(distr)) < distr[ceiling(length(distr)/2)-1] - distr[ceiling(length(distr)/2)]){
        bins <- bins - 1
        distr <-suppressWarnings(qmethod::make.distribution(nstat = nstat, max.bin = bins))
        }
      distr[ceiling(length(distr)/2)] <- distr[ceiling(length(distr)/2)] + (nstat - sum(distr))
      }
  }
  # sum(distr)
  if(0 %in% distr){distr <- distr[-which(distr == 0)]}
  # sum(distr)

  values <- c(bins:0,0:bins)
  values <- values[-ceiling(length(values)/2)]
  values[(ceiling(length(values)/2)+1):length(values)] <- values[(ceiling(length(values)/2)+1):length(values)] * -1
  rankingvalues <- rev(values)
  
  rankinggradient <- NA
  for(i in seq_along(distr)){
    rankinggradient <- c(rankinggradient, rep(values[i], distr[i]))
  }
  rankinggradient <- rev(rankinggradient[-which(is.na(rankinggradient))])
  
  distribution <- list(distr, rankingvalues, rankinggradient)
  names(distribution) <- c("distr", "values", "ranking")
  
  if(distr[1] != 1){
    warning("The distribution does not support a clear priority statement.")
  }
  
  return(distribution)
}

.distributiondetermination <- distributiondetermination

prepare_rankings <- function(dataset, idcolumn = "ID",
                             statement_columns = NULL, add = list(NULL),
                             orientation = c("auto", "participant_rows", "statement_rows")){
  orientation <- match.arg(orientation)
  if (!is.data.frame(dataset) && !is.matrix(dataset)) {
    stop("The dataset must be a data frame or matrix.")
  }
  dataset <- as.data.frame(dataset, check.names = FALSE)

  has_id <- !is.null(idcolumn) && length(idcolumn) == 1L && !is.na(idcolumn) &&
    idcolumn %in% names(dataset)
  if (!is.null(idcolumn) && length(idcolumn) == 1L && !is.na(idcolumn) &&
      !has_id && orientation == "participant_rows") {
    stop("idcolumn '", idcolumn, "' was not found in dataset")
  }
  if (has_id && anyDuplicated(dataset[[idcolumn]])) {
    stop("Values in idcolumn must be unique.")
  }

  expected_distribution <- function(statement_count) {
    distribution <- distributiondetermination(statement_count)
    values <- as.numeric(distribution$values)
    gradient <- as.numeric(distribution$distr)
    stats::setNames(gradient, values)
  }
  format_distribution <- function(counts) {
    paste0(as.integer(counts), "x ", names(counts), collapse = ", ")
  }
  matches_distribution <- function(values, expected) {
    if (length(values) != sum(expected) || anyNA(values) ||
        !all(is.finite(values))) return(FALSE)
    observed <- table(factor(values, levels = as.numeric(names(expected))))
    identical(as.integer(observed), as.integer(expected))
  }
  distribution_error <- function(statement_count, ids, observed_axis) {
    expected <- expected_distribution(statement_count)
    bad <- if (length(ids)) paste(ids, collapse = ", ") else "one or more rankings"
    stop(
      "Error: For ", statement_count,
      " statements, the rankings must follow this distribution: ",
      format_distribution(expected), ". \nAffected rankings: ", bad, ".",
      call. = FALSE
    )
  }

  if (has_id) {
    row.names(dataset) <- as.character(dataset[[idcolumn]])
    dataset[[idcolumn]] <- NULL
    orientation <- "participant_rows"
  }

  if (is.null(statement_columns)) {
    if (orientation == "statement_rows") {
      statement_columns <- names(dataset)
    } else if (has_id || orientation == "participant_rows") {
      statement_columns <- names(dataset)
    } else if (all(vapply(dataset, is.numeric, logical(1)))) {
      row_expected <- if (ncol(dataset) >= 3L) expected_distribution(ncol(dataset)) else NULL
      col_expected <- if (nrow(dataset) >= 3L) expected_distribution(nrow(dataset)) else NULL
      row_valid <- if (is.null(row_expected)) rep(FALSE, nrow(dataset)) else
        vapply(seq_len(nrow(dataset)), function(i)
          matches_distribution(as.numeric(dataset[i, ]), row_expected), logical(1))
      col_valid <- if (is.null(col_expected)) rep(FALSE, ncol(dataset)) else
        vapply(seq_len(ncol(dataset)), function(i)
          matches_distribution(as.numeric(dataset[[i]]), col_expected), logical(1))
      if (all(col_valid) && !all(row_valid)) {
        orientation <- "statement_rows"
        statement_columns <- names(dataset)
      } else if (all(row_valid) && !all(col_valid)) {
        orientation <- "participant_rows"
        statement_columns <- names(dataset)
      } else if (nrow(dataset) < ncol(dataset) &&
                 (length(row.names(dataset)) &&
                  !all(row.names(dataset) == as.character(seq_len(nrow(dataset)))))) {
        orientation <- "statement_rows"
        statement_columns <- names(dataset)
      } else {
        stop("Statement columns could not be identified unambiguously. Supply statement_columns or an ID column.")
      }
    } else {
      stop("Statement columns could not be identified. Supply statement_columns or an ID column.")
    }
  }
  missing_columns <- setdiff(statement_columns, names(dataset))
  if (length(missing_columns)) stop("Statement columns not found: ", paste(missing_columns, collapse = ", "))
  if (!all(vapply(dataset[, statement_columns, drop = FALSE], is.numeric, logical(1)))) stop("All statement columns must be numeric.")

  if (orientation == "statement_rows") {
    rankings <- dataset[, statement_columns, drop = FALSE]
    expected <- expected_distribution(nrow(rankings))
    valid <- vapply(seq_len(ncol(rankings)), function(i) matches_distribution(rankings[[i]], expected), logical(1))
    if (!all(valid)) distribution_error(nrow(rankings), names(rankings)[!valid], rankings)
  } else {
    rankings_input <- dataset[, statement_columns, drop = FALSE]
    statement_count <- ncol(rankings_input)
    expected <- expected_distribution(statement_count)
    valid <- vapply(seq_len(nrow(rankings_input)), function(i) matches_distribution(as.numeric(rankings_input[i, ]), expected), logical(1))
    if (!all(valid)) distribution_error(statement_count, row.names(rankings_input)[!valid], rankings_input)
    rankings <- data.frame(t(rankings_input), check.names = FALSE)
  }

  additions <- Filter(Negate(is.null), add)
  if (length(additions)) {
    statement_count <- nrow(rankings)
    expected <- expected_distribution(statement_count)
    additions <- lapply(seq_along(additions), function(add_index) {
      x <- additions[[add_index]]
      if (is.vector(x) && !is.list(x)) x <- data.frame(additional = x)
      x <- as.data.frame(x, check.names = FALSE)
      add_has_id <- !is.null(idcolumn) && length(idcolumn) == 1L &&
        !is.na(idcolumn) && idcolumn %in% names(x)
      if (add_has_id) {
        if (anyDuplicated(x[[idcolumn]])) {
          stop("The values in added ranking object ", add_index,
               "'s ID column must be unique.", call. = FALSE)
        }
        row.names(x) <- as.character(x[[idcolumn]])
        x[[idcolumn]] <- NULL
      }
      if (!all(vapply(x, is.numeric, logical(1)))) {
        stop("The added ranking object ", add_index, " must contain only numeric values.")
      }
      row_valid <- if (ncol(x) == statement_count) {
        vapply(seq_len(nrow(x)), function(i)
          matches_distribution(as.numeric(x[i, ]), expected), logical(1))
      } else logical()
      col_valid <- if (nrow(x) == statement_count) {
        vapply(seq_len(ncol(x)), function(i)
          matches_distribution(as.numeric(x[[i]]), expected), logical(1))
      } else logical()
      if (length(col_valid) && all(col_valid) && !(length(row_valid) && all(row_valid))) {
        prepared_addition <- x
      } else if (length(row_valid) && all(row_valid) && !(length(col_valid) && all(col_valid))) {
        prepared_addition <- data.frame(t(x), check.names = FALSE)
      } else if (nrow(x) == statement_count && ncol(x) == 1L &&
                 matches_distribution(as.numeric(x[[1L]]), expected)) {
        prepared_addition <- x
      } else {
        stop(
          "Added ranking object ", add_index,
          " could not be identified as participant-row or statement-row input, "
          , "or does not follow the required distribution for ", statement_count,
          " statements: ", format_distribution(expected), ".", call. = FALSE
        )
      }
      if (!is.null(rownames(rankings)) && !is.null(rownames(prepared_addition)) &&
          length(rownames(prepared_addition)) == statement_count &&
          !identical(rownames(rankings), rownames(prepared_addition))) {
        stop("Added ranking object ", add_index, " has statement names/order that do not match the main dataset.", call. = FALSE)
      }
      rownames(prepared_addition) <- rownames(rankings)
      prepared_addition
    })
    rankings <- do.call(cbind, c(list(rankings), additions))
  }
  rankings
}

.qmethod_condition_classification <- function(text, type) {
  if (grepl(
      "qindtest alignment failed; the batch was repeated with orthogonal Procrustes alignment",
      text, fixed = TRUE
  )) {
    return("qindtest alignment fallback")
  }
  if (identical(type, "message")) return("informational message")
  patterns <- c(
    "OK:" = "alignment diagnostic",
    "rotation method selected is not standard" = "quartimax rotation",
    "Matrix was not positive definite, smoothing was done" =
      "correlation-matrix smoothing",
    "negative loadings are flagged" = "negative flagging",
    "Only one factor selected. No distinguishing and consensus statements" =
      "one-factor limitation",
    "bootstrapping is an advanced technique in Q methodology" =
      "bootstrap methodology notice",
    "this index is uncommon in Q methodology publications" =
      "bootstrap methodology notice",
    "factors extracted have no flagged Q-sorts" =
      "bootstrap factor without flagged rankings",
    "model inverse times the r matrix is singular" =
      "singular model inverse; fit may be unreliable",
    "Comparisons for distinguishing and consensus statements exclude the factor(s) for which there were no flags" =
      "bootstrap factor without flagged rankings",
    "Orthogonal Procrustes bootstrap iteration" =
      "invalid bootstrap iteration",
    "qindtest could not uniquely align bootstrap iteration" =
      "invalid bootstrap iteration",
    "Invalid bootstrap iterations discarded" =
      "invalid bootstrap iteration",
    "negative loading while flagging" =
      "negative-loading flag",
    "factor without at least two positively flagging rankings" =
      "bootstrap factor without flagged rankings",
    "no non-missing arguments to min; returning Inf" =
      "invalid bootstrap iteration",
    "no non-missing arguments to max; returning -Inf" =
      "invalid bootstrap iteration",
    "NaNs produced" =
      "invalid bootstrap iteration"
  )
  matched <- vapply(names(patterns), function(pattern) {
    grepl(pattern, text, fixed = TRUE)
  }, logical(1))
  if (any(matched)) unname(patterns[which(matched)[1L]]) else
    "unclassified warning"
}

.run_qmethod_candidate_with_diagnostics <- function(
    dataset, nfactors, rotation) {
  warnings <- character()
  messages <- character()
  result <- withCallingHandlers(
    qmethod::qmethod(
      dataset, nfactors = nfactors, rotation = rotation, silent = TRUE
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    },
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )
  types <- c(rep("warning", length(warnings)), rep("message", length(messages)))
  conditions <- c(warnings, messages)
  diagnostics <- data.frame(
    nfactors = rep(as.integer(nfactors), length(conditions)),
    Type = types,
    Classification = vapply(
      seq_along(conditions), function(index) {
        .qmethod_condition_classification(conditions[index], types[index])
      }, character(1)
    ),
    Condition = conditions,
    check.names = FALSE,
    row.names = NULL
  )
  list(result = result, diagnostics = diagnostics)
}

.factor_selection_diagnostics <- function(candidate_runs, rotation) {
  nonempty <- Filter(
    function(run) is.data.frame(run$diagnostics) && nrow(run$diagnostics),
    candidate_runs
  )
  conditions <- if (length(nonempty)) {
    do.call(rbind, lapply(nonempty, `[[`, "diagnostics"))
  } else {
    data.frame(
      nfactors = integer(), Type = character(), Classification = character(),
      Condition = character(), check.names = FALSE
    )
  }
  distinct <- unique(conditions[c("Type", "Classification", "Condition")])
  unclassified <- unique(conditions$Condition[
    conditions$Classification == "unclassified warning"
  ])
  diagnostics <- list(
    rotation = rotation,
    candidates_with_conditions = if (nrow(conditions)) {
      sort(unique(conditions$nfactors))
    } else integer(),
    distinct_conditions = distinct,
    conditions_by_candidate = conditions,
    unclassified_warnings = unclassified
  )

  known <- distinct[
    distinct$Classification != "unclassified warning", , drop = FALSE
  ]
  if (length(unclassified)) {
    for (text in unclassified) {
      warning(
        "The automatic factor selection produced an unclassified warning: ",
        text, " Diagnostics are available in ",
        "results$diagnostics$factor_selection.",
        call. = FALSE
      )
    }
  }
  diagnostics
}

.optimize_factor_selection <- function(dataset, rotation = "quartimax",
                                      load_perc = 0.8, min_load_perc = 0.5,
                                      morethan5 = FALSE){
  rotation <- match.arg(rotation, c("varimax", "quartimax"))
  if (!is.numeric(load_perc) || length(load_perc) != 1L ||
      is.na(load_perc) || load_perc < 0 || load_perc > 1) {
    stop("load_perc must be a single number between 0 and 1.")
  }
  if (!is.numeric(min_load_perc) || length(min_load_perc) != 1L ||
      is.na(min_load_perc) || min_load_perc < 0 || min_load_perc > load_perc) {
    stop("min_load_perc must be between 0 and load_perc.")
  }

  run_qmethod <- function(k) {
    .run_qmethod_candidate_with_diagnostics(dataset, k, rotation)
  }

  one_run <- run_qmethod(1L)
  one <- one_run$result
  one_positive <- as.matrix(one$flagged) & as.matrix(one$loa) > 0
  one_factor_loadings <- colSums(one_positive, na.rm = TRUE)
  one_coverage <- sum(one_factor_loadings) / ncol(dataset)
  if (one_coverage == 1) {
    diagnostics <- .factor_selection_diagnostics(list(one_run), rotation)
    diagnostics$single_loading_exception <- list(
      used = FALSE, selected_perspectives = integer(), explanation = NULL
    )
    return(list(
      nfactors = 1L,
      requested_load_perc = load_perc,
      effective_load_perc = load_perc,
      achieved_load_perc = one_coverage,
      threshold_adjusted = FALSE,
      factor_loadings = one_factor_loadings,
      opposing_loadings = colSums(
        as.matrix(one$flagged) & as.matrix(one$loa) < 0,
        na.rm = TRUE
      ),
      single_loading_exception_used = FALSE,
      candidates = data.frame(
        nfactors = 1L, achieved_load_perc = one_coverage,
        minimum_two_loadings_per_factor = TRUE,
        single_loading_exception_eligible = FALSE,
        single_loading_exception_used = FALSE
      ),
      diagnostics = diagnostics
    ))
  }

  max_factors <- if (isTRUE(morethan5)) 10L else 5L
  max_factors <- min(max_factors, ncol(dataset) - 1L, nrow(dataset) - 1L)
  if (max_factors < 2L) {
    stop(
      "At least 3 statements and 3 complete rankings are required to ",
      "evaluate a multi-factor solution."
    )
  }
  candidates <- seq.int(2L, max_factors)

  candidate_runs <- lapply(candidates, run_qmethod)
  solutions <- lapply(candidate_runs, `[[`, "result")
  loading_counts <- lapply(solutions, function(x) {
    positive <- as.matrix(x$flagged) & as.matrix(x$loa) > 0
    colSums(positive, na.rm = TRUE)
  })
  opposing_counts <- lapply(solutions, function(x) {
    opposing <- as.matrix(x$flagged) & as.matrix(x$loa) < 0
    colSums(opposing, na.rm = TRUE)
  })
  coverage <- vapply(loading_counts, sum, numeric(1)) / ncol(dataset)
  has_no_single_factor <- vapply(loading_counts, function(x) all(x > 1L), logical(1))
  exception_eligible <- vapply(seq_along(loading_counts), function(index) {
    agreeing <- loading_counts[[index]]
    opposing <- opposing_counts[[index]]
    all(agreeing >= 1L) &&
      any(agreeing == 1L) &&
      all(opposing[agreeing == 1L] == 0L)
  }, logical(1))
  candidate_summary <- data.frame(
    nfactors = candidates,
    achieved_load_perc = coverage,
    minimum_two_loadings_per_factor = has_no_single_factor,
    single_loading_exception_eligible = exception_eligible,
    single_loading_exception_used = FALSE
  )

  effective_load_perc <- load_perc
  eligible <- which(coverage >= effective_load_perc & has_no_single_factor)

  if (!length(eligible)) {
    strict_usable <- which(has_no_single_factor & coverage >= min_load_perc)
    strict_best <- if (length(strict_usable)) max(coverage[strict_usable]) else -Inf
    exception_usable <- which(
      exception_eligible & coverage >= min_load_perc &
        coverage > strict_best + sqrt(.Machine$double.eps)
    )
    usable <- c(strict_usable, exception_usable)
    if (!length(usable)) {
      stop(
        "No factor solution achieved the minimum loading proportion of ",
        round(min_load_perc, 3),
        " under the factor-support criteria."
      )
    }
    effective_load_perc <- max(coverage[usable])
    eligible <- usable[
      coverage[usable] >= effective_load_perc - sqrt(.Machine$double.eps)
    ]
  }

  # Select lexicographically by numbers of loadings: maximize the strongest
  # factor first, then the second strongest, and so on. Prefer fewer factors
  # if the loading profiles are otherwise equal.
  profiles <- lapply(loading_counts[eligible], function(x) sort(x, decreasing = TRUE))
  max_length <- max(lengths(profiles))
  profile_matrix <- t(vapply(profiles, function(x) {
    c(x, rep(-Inf, max_length - length(x)))
  }, numeric(max_length)))
  remaining <- seq_along(eligible)
  for (level in seq_len(ncol(profile_matrix))) {
    best <- max(profile_matrix[remaining, level])
    remaining <- remaining[profile_matrix[remaining, level] == best]
    if (length(remaining) == 1L) break
  }
  selected_index <- eligible[remaining[1L]]
  exception_used <- !has_no_single_factor[selected_index]
  candidate_summary$single_loading_exception_used[selected_index] <- exception_used
  diagnostics <- .factor_selection_diagnostics(
    c(list(one_run), candidate_runs), rotation
  )
  diagnostics$single_loading_exception <- list(
    used = exception_used,
    selected_perspectives = if (exception_used) {
      which(loading_counts[[selected_index]] == 1L)
    } else integer(),
    explanation = if (exception_used) {
      paste(
        "A perspective with exactly one agreeing ranking and no opposing",
        "rankings was retained because it produced a higher effective",
        "consensus than every otherwise eligible solution."
      )
    } else NULL
  )
  list(
    nfactors = candidates[selected_index],
    requested_load_perc = load_perc,
    effective_load_perc = effective_load_perc,
    achieved_load_perc = coverage[selected_index],
    threshold_adjusted = effective_load_perc < load_perc,
    factor_loadings = loading_counts[[selected_index]],
    opposing_loadings = opposing_counts[[selected_index]],
    single_loading_exception_used = exception_used,
    candidates = candidate_summary,
    diagnostics = diagnostics
  )
}

#' Calculate consensus priority scores from perspective z-scores
#'
#' @param statementzscores Matrix of perspective z-scores, with statements in columns or rows.
#' @param factoreigenvalues Numeric eigenvalues for the perspectives.
#' @details The function first calculates the eigenvalue-weighted mean z-score
#'   for every statement and then applies the fixed standard-normal cumulative
#'   distribution function. A score of 0.5 therefore represents neutral
#'   prioritization across the group perspectives; values above or below 0.5
#'   represent relatively higher or lower priority. Values 0 and 1 are
#'   theoretical boundaries for finite z-scores. Comparisons across analyses
#'   require the same statements and meanings, ranking distribution,
#'   instructions, data preparation, and analytical settings.
#' @return A named numeric vector of consensus priority scores on a fixed
#'   standard-normal cumulative-probability scale from 0 to 1. A value of 0.5
#'   represents neutral prioritization across the group perspectives.
#' @export
cpscores <- function(statementzscores, factoreigenvalues){
  stats::pnorm(.weighted_z_scores(statementzscores, factoreigenvalues))
} # helper function used in qapproach()

.weighted_z_scores <- function(statementzscores, factoreigenvalues) {
  zscores <- as.matrix(statementzscores)
  storage.mode(zscores) <- "double"
  eigenvalues <- as.numeric(factoreigenvalues)
  if (!length(eigenvalues) || any(!is.finite(eigenvalues))) {
    stop("factoreigenvalues must contain finite numeric values.")
  }
  if (nrow(zscores) != length(eigenvalues)) {
    if (ncol(zscores) == length(eigenvalues)) {
      zscores <- t(zscores)
    } else {
      stop("One dimension of statementzscores must equal the number of factor eigenvalues.")
    }
  }
  if (any(!is.finite(zscores))) {
    stop("statementzscores must contain finite values.")
  }
  eigenvalue_sum <- sum(eigenvalues)
  if (!is.finite(eigenvalue_sum) || eigenvalue_sum <= 0) {
    stop("factoreigenvalues must have a positive finite sum.")
  }
  stats::setNames(
    colSums(zscores * (eigenvalues / eigenvalue_sum)),
    colnames(zscores)
  )
}

.check_perspective_distributions <- function(perspectives, nstat) {
  perspectives <- as.data.frame(perspectives, check.names = FALSE)
  target <- distributiondetermination(nstat)$ranking
  values <- sort(unique(c(target, as.numeric(as.matrix(perspectives)))))
  expected <- vapply(values, function(value) sum(target == value), integer(1))
  details <- vector("list", nrow(perspectives))
  broken <- logical(nrow(perspectives))

  for (perspective in seq_len(nrow(perspectives))) {
    observed <- vapply(
      values,
      function(value) sum(
        as.numeric(perspectives[perspective, ]) == value,
        na.rm = TRUE
      ),
      integer(1)
    )
    difference <- observed - expected
    broken[perspective] <- any(difference != 0L)
    details[[perspective]] <- data.frame(
      Perspective = paste("Perspective", perspective),
      `Ranking value` = values,
      Expected = expected,
      Observed = observed,
      Difference = difference,
      check.names = FALSE,
      row.names = NULL
    )
  }

  list(
    valid = !broken,
    broken_indices = which(broken),
    broken_perspectives = paste("Perspective", which(broken)),
    details = do.call(rbind, details),
    target_ranking = target
  )
}

.bootstrap_distribution_references <- function(
    dataset, qmethod_result, perspective_indices, target_valid_steps,
    seed = NULL, max_attempt_multiplier = 10L, max_batch_steps = 500L) {
  dataset <- as.data.frame(dataset, check.names = FALSE)
  target_valid_steps <- as.integer(target_valid_steps)
  max_attempts <- target_valid_steps * as.integer(max_attempt_multiplier)
  statement_count <- nrow(dataset)
  collected <- lapply(
    perspective_indices,
    function(index) matrix(numeric(), nrow = statement_count, ncol = 0L)
  )
  requested_steps <- 0L
  valid_steps <- 0L
  batch_count <- 0L
  next_batch_steps <- min(max_batch_steps, target_valid_steps)
  batch_diagnostics <- list()
  batch_alignment_methods <- character()

  if (!is.null(seed)) withr::local_seed(as.integer(seed))

  while (valid_steps < target_valid_steps && requested_steps < max_attempts) {
    batch_count <- batch_count + 1L
    remaining_attempts <- max_attempts - requested_steps
    batch_steps <- min(next_batch_steps, remaining_attempts)
    if (!is.null(seed)) set.seed(as.integer(seed) + batch_count - 1L)
    invisible(utils::capture.output(
    bootstrap_run <- .run_aligned_bootstrap_with_diagnostics(
        dataset,
        qmethod_result = qmethod_result,
        nsteps = batch_steps
      )
    ))
    batch <- bootstrap_run$result
    condition_types <- c(
      rep("warning", length(bootstrap_run$warnings)),
      rep("message", length(bootstrap_run$messages))
    )
    batch_alignment_methods[batch_count] <- bootstrap_run$alignment_method
    condition_text <- c(bootstrap_run$warnings, bootstrap_run$messages)
    batch_diagnostics[[batch_count]] <- data.frame(
      Batch = rep(batch_count, length(condition_text)),
      Type = condition_types,
      Condition = condition_text,
      check.names = FALSE,
      row.names = NULL
    )
    requested_steps <- requested_steps + batch_steps

    matrices <- lapply(perspective_indices, function(index) {
      as.matrix(batch$full.bts.res[[index]]$zsc)
    })
    valid_columns <- Reduce(`&`, lapply(matrices, function(values) {
      apply(values, 2L, function(column) all(is.finite(column)))
    }))
    if (any(valid_columns)) {
      remaining <- target_valid_steps - valid_steps
      selected <- which(valid_columns)[seq_len(min(sum(valid_columns), remaining))]
      for (index in seq_along(collected)) {
        collected[[index]] <- cbind(
          collected[[index]], matrices[[index]][, selected, drop = FALSE]
        )
      }
      valid_steps <- ncol(collected[[1L]])
    }

    if (valid_steps < target_valid_steps) {
      observed_rate <- valid_steps / requested_steps
      remaining_valid <- target_valid_steps - valid_steps
      estimated_attempts <- ceiling(
        remaining_valid / max(observed_rate, 0.05) * 1.05
      )
      next_batch_steps <- min(
        max_batch_steps, max(40L, estimated_attempts)
      )
    }
  }

  if (valid_steps < target_valid_steps) {
    stop(
      "Only ", valid_steps, " valid aligned bootstrap iterations were ",
      "produced after ", requested_steps, " attempts; ",
      target_valid_steps, " were required for distribution repair."
    )
  }
  averages <- do.call(rbind, lapply(collected, rowMeans))
  rownames(averages) <- paste("Perspective", perspective_indices)
  colnames(averages) <- rownames(dataset)
  combined_diagnostics <- .combine_qmboots_diagnostics(batch_diagnostics)
  alignment_method <- if (any(grepl(
      "fallback", batch_alignment_methods, fixed = TRUE
  ))) {
    "qindtest with orthogonal Procrustes fallback"
  } else {
    unique(batch_alignment_methods)[1L]
  }
  combined_diagnostics$alignment_method <- alignment_method
  combined_diagnostics$alignment_methods_by_batch <- data.frame(
    Batch = seq_along(batch_alignment_methods),
    Method = unname(batch_alignment_methods),
    check.names = FALSE
  )
  list(
    average_zscores = averages,
    requested_steps = requested_steps,
    valid_steps = valid_steps,
    discarded_steps = requested_steps - valid_steps,
    batches = batch_count,
    alignment_method = alignment_method,
    diagnostics = combined_diagnostics
  )
}

.automatically_repair_perspective_distribution <- function(
    original_ranking, bootstrap_average_zscores, target_ranking,
    statement_names = names(original_ranking)) {
  original_ranking <- as.numeric(original_ranking)
  bootstrap_average_zscores <- as.numeric(bootstrap_average_zscores)
  target_ranking <- as.numeric(target_ranking)
  statement_count <- length(original_ranking)
  if (length(bootstrap_average_zscores) != statement_count ||
      length(target_ranking) != statement_count ||
      any(!is.finite(bootstrap_average_zscores))) {
    stop("The distribution repair inputs have incompatible or non-finite values.")
  }
  if (is.null(statement_names) || length(statement_names) != statement_count) {
    statement_names <- paste0("stat", seq_len(statement_count))
  }

  expected_ranking <- numeric(statement_count)
  expected_ranking[
    order(bootstrap_average_zscores, seq_len(statement_count))
  ] <- sort(target_ranking)
  values <- sort(unique(c(original_ranking, target_ranking)))
  expected_counts <- stats::setNames(vapply(
    values, function(value) sum(target_ranking == value), integer(1)
  ), as.character(values))
  observed_counts <- stats::setNames(vapply(
    values, function(value) sum(original_ranking == value), integer(1)
  ), as.character(values))
  deficits <- rep(
    values,
    pmax(expected_counts - observed_counts, 0L)
  )
  surplus_remaining <- pmax(observed_counts - expected_counts, 0L)
  corrected <- original_ranking
  available <- rep(TRUE, statement_count)
  changes <- vector("list", length(deficits))

  for (change_index in seq_along(deficits)) {
    replacement <- deficits[change_index]
    original_keys <- as.character(original_ranking)
    candidates <- which(
      available & surplus_remaining[original_keys] > 0L
    )
    if (!length(candidates)) {
      stop("No surplus ranking value is available for automatic repair.")
    }
    cost_change <-
      abs(expected_ranking[candidates] - replacement) -
      abs(expected_ranking[candidates] - original_ranking[candidates])
    candidate_order <- order(
      cost_change,
      abs(bootstrap_average_zscores[candidates]),
      candidates
    )
    selected <- candidates[candidate_order[1L]]
    original_value <- original_ranking[selected]
    corrected[selected] <- replacement
    surplus_remaining[as.character(original_value)] <-
      surplus_remaining[as.character(original_value)] - 1L
    available[selected] <- FALSE
    changes[[change_index]] <- data.frame(
      Statement = statement_names[selected],
      `Statement index` = selected,
      `Original value` = original_value,
      `Repaired value` = replacement,
      `Bootstrap-reference value` = expected_ranking[selected],
      check.names = FALSE,
      row.names = NULL
    )
  }

  if (!identical(sort(corrected), sort(target_ranking))) {
    stop("The automatic distribution repair did not reproduce the target gradient.")
  }
  list(
    ranking = corrected,
    changes = if (length(changes)) do.call(rbind, changes) else data.frame(),
    bootstrap_reference_ranking = expected_ranking
  )
}

.run_qmboots_with_diagnostics <- function(..., progress_callback = NULL) {
  observed_warnings <- character()
  observed_messages <- character()
  utils::capture.output(
    result <- withCallingHandlers(
      qmethod::qmboots(...),
      warning = function(condition) {
        observed_warnings <<- c(observed_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      },
      message = function(condition) {
        observed_messages <<- c(observed_messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    ),
    type = "output"
  )
  if (is.function(progress_callback) && is.list(result$full.bts.res)) {
    nvalid <- if (length(result$full.bts.res)) {
      sum(vapply(seq_len(ncol(result$full.bts.res[[1L]]$zsc)), function(i)
        all(vapply(result$full.bts.res, function(x) all(is.finite(x$zsc[, i])), logical(1))), logical(1)))
    } else 0L
    for (i in seq_len(nvalid)) progress_callback()
  }
  list(
    result = result,
    warnings = unique(observed_warnings),
    messages = unique(observed_messages)
  )
}

.bootstrap_alignment_method <- function(nfactors) {
  nfactors <- as.integer(nfactors)
  if (length(nfactors) != 1L || is.na(nfactors) || nfactors < 1L) {
    stop("nfactors must be one positive whole number.")
  }
  if (nfactors >= 2L && nfactors <= 3L) return("qindtest")
  "orthogonal_procrustes"
}

.orthogonal_procrustes_loadings <- function(loadings, target) {
  loadings <- as.matrix(loadings)
  target <- as.matrix(target)
  storage.mode(loadings) <- "double"
  storage.mode(target) <- "double"
  if (!identical(dim(loadings), dim(target)) ||
      !nrow(loadings) || !ncol(loadings) ||
      any(!is.finite(loadings)) || any(!is.finite(target))) {
    stop("The Procrustes loadings and target must be equally sized finite matrices.")
  }
  decomposition <- svd(crossprod(loadings, target))
  rotation <- decomposition$u %*% t(decomposition$v)
  aligned <- loadings %*% rotation
  factor_signs <- sign(colSums(aligned * target))
  factor_signs[!is.finite(factor_signs) | factor_signs == 0] <- 1
  aligned <- sweep(aligned, 2L, factor_signs, `*`)
  rotation <- rotation %*% diag(factor_signs, nrow = length(factor_signs))
  target_norm <- sqrt(sum(target^2))
  residual <- sqrt(sum((aligned - target)^2)) /
    if (target_norm > 0) target_norm else 1
  congruence <- vapply(seq_len(ncol(aligned)), function(factor) {
    denominator <- sqrt(
      sum(aligned[, factor]^2) * sum(target[, factor]^2)
    )
    if (denominator > 0) {
      sum(aligned[, factor] * target[, factor]) / denominator
    } else {
      NA_real_
    }
  }, numeric(1))
  list(
    loadings = aligned,
    rotation = rotation,
    residual = residual,
    congruence = congruence
  )
}

.run_procrustes_bootstrap_with_diagnostics <- function(
    dataset, qmethod_result, nsteps, progress_callback = NULL) {
  dataset <- as.data.frame(dataset, check.names = FALSE)
  nfactors <- as.integer(qmethod_result$brief$nfactors)
  nsteps <- as.integer(nsteps)
  statement_count <- nrow(dataset)
  ranking_count <- ncol(dataset)
  ranking_ids <- colnames(dataset)
  if (is.null(ranking_ids)) ranking_ids <- paste0("ranking_", seq_len(ranking_count))
  statement_ids <- rownames(dataset)
  if (is.null(statement_ids)) {
    statement_ids <- paste0("statement_", seq_len(statement_count))
  }
  target_loadings <- as.matrix(qmethod_result$loa)
  if (!identical(dim(target_loadings), c(ranking_count, nfactors))) {
    stop("The original Q method loadings do not match the bootstrap inputs.")
  }

  step_names <- paste0("step_", seq_len(nsteps))
  full_results <- lapply(seq_len(nfactors), function(factor) {
    list(
      flagged = data.frame(
        matrix(
          NA, nrow = ranking_count, ncol = nsteps,
          dimnames = list(ranking_ids, step_names)
        ),
        check.names = FALSE
      ),
      zsc = data.frame(
        matrix(
          NA_real_, nrow = statement_count, ncol = nsteps,
          dimnames = list(statement_ids, step_names)
        ),
        check.names = FALSE
      ),
      loa = data.frame(
        matrix(
          NA_real_, nrow = ranking_count, ncol = nsteps,
          dimnames = list(ranking_ids, step_names)
        ),
        check.names = FALSE
      )
    )
  })
  names(full_results) <- paste0("factor", seq_len(nfactors))

  balanced_indices <- sample(
    rep(seq_len(ranking_count), times = nsteps),
    size = ranking_count * nsteps,
    replace = FALSE
  )
  resamples <- matrix(
    balanced_indices, nrow = ranking_count, ncol = nsteps,
    dimnames = list(NULL, paste0("bsampl_", seq_len(nsteps)))
  )
  alignment_summary <- data.frame(
    Iteration = seq_len(nsteps),
    Valid = FALSE,
    `Relative residual` = NA_real_,
    `Minimum factor congruence` = NA_real_,
    Error = NA_character_,
    check.names = FALSE
  )
  rotations <- vector("list", nsteps)
  observed_warnings <- character()
  observed_messages <- character()

  run_iteration <- function(step) {
    sampled <- resamples[, step]
    sampled_ids <- make.unique(ranking_ids[sampled], sep = ".bootstrap")
    bootstrap_data <- dataset[, sampled, drop = FALSE]
    colnames(bootstrap_data) <- sampled_ids
    target <- target_loadings[sampled, , drop = FALSE]
    rownames(target) <- sampled_ids
    fitted <- qmethod::qmethod(
      bootstrap_data,
      nfactors = nfactors,
      rotation = qmethod_result$brief$rotation,
      silent = TRUE
    )
    aligned <- .orthogonal_procrustes_loadings(fitted$loa, target)
    rownames(aligned$loadings) <- sampled_ids
    colnames(aligned$loadings) <- paste0("f", seq_len(nfactors))
    flagged <- qmethod::qflag(
      loa = aligned$loadings,
      nstat = statement_count
    )
    negative_flagged <- flagged & is.finite(aligned$loadings) & aligned$loadings < 0
    positive_counts <- colSums(flagged & !negative_flagged)
    if (any(positive_counts < 2L)) {
      stop("The factor has fewer than two positively flagging rankings.")
    }
    recalculated <- qmethod::qzscores(
      dataset = bootstrap_data,
      nfactors = nfactors,
      loa = aligned$loadings,
      flagged = flagged,
      forced = TRUE
    )
    list(
      sampled = sampled,
      loadings = as.matrix(aligned$loadings),
      flagged = as.matrix(flagged),
      zscores = as.matrix(recalculated$zsc),
      rotation = aligned$rotation,
      residual = aligned$residual,
      congruence = aligned$congruence
    )
  }

  for (step in seq_len(nsteps)) {
    iteration <- tryCatch(
      withCallingHandlers(
        run_iteration(step),
        warning = function(condition) {
          observed_warnings <<- c(
            observed_warnings, conditionMessage(condition)
          )
          invokeRestart("muffleWarning")
        },
        message = function(condition) {
          observed_messages <<- c(
            observed_messages, conditionMessage(condition)
          )
          invokeRestart("muffleMessage")
        }
      ),
      error = function(error) {
        alignment_summary$Error[step] <<- conditionMessage(error)
        NULL
      }
    )
    if (is.null(iteration)) next
    if (any(!is.finite(iteration$zscores))) {
      alignment_summary$Error[step] <-
        "Recalculated z-scores contained non-finite values."
      next
    }

    for (factor in seq_len(nfactors)) {
      full_results[[factor]]$zsc[, step] <- iteration$zscores[, factor]
      for (ranking in seq_len(ranking_count)) {
        sampled_position <- match(ranking, iteration$sampled)
        if (!is.na(sampled_position)) {
          full_results[[factor]]$loa[ranking, step] <-
            iteration$loadings[sampled_position, factor]
          full_results[[factor]]$flagged[ranking, step] <-
            iteration$flagged[sampled_position, factor]
        }
      }
    }
    rotations[[step]] <- iteration$rotation
    alignment_summary$Valid[step] <- TRUE
    alignment_summary$`Relative residual`[step] <- iteration$residual
    finite_congruence <- iteration$congruence[
      is.finite(iteration$congruence)
    ]
    alignment_summary$`Minimum factor congruence`[step] <-
      if (length(finite_congruence)) min(finite_congruence) else NA_real_
    if (is.function(progress_callback)) progress_callback()
  }

  failed <- which(!alignment_summary$Valid)
  if (length(failed)) {
    observed_warnings <- c(
      observed_warnings,
      paste0(
        "Orthogonal Procrustes bootstrap iteration ", failed,
        " was invalid: ", alignment_summary$Error[failed]
      )
    )
  }
  result <- list(
    "zscore-stats" = NULL,
    "full.bts.res" = full_results,
    "indet.tests" = list(
      method = "orthogonal Procrustes (qapproach implementation)",
      summary = alignment_summary,
      rotations = rotations
    ),
    "resamples" = as.data.frame(resamples),
    "orig.res" = qmethod_result,
    "q.array" = sort(as.numeric(dataset[, 1L])),
    "loa.stats" = NULL,
    "fsi" = NULL
  )
  list(
    result = result,
    warnings = unique(observed_warnings),
    messages = unique(observed_messages),
    alignment_diagnostics = result$indet.tests,
    alignment_method = "orthogonal Procrustes (qapproach implementation)"
  )
}

.run_aligned_bootstrap_with_diagnostics <- function(
    dataset, qmethod_result, nsteps, progress_callback = NULL) {
  alignment <- .bootstrap_alignment_method(qmethod_result$brief$nfactors)
  if (identical(alignment, "orthogonal_procrustes")) {
    return(.run_procrustes_bootstrap_with_diagnostics(
      dataset, qmethod_result, nsteps, progress_callback
    ))
  }
  output <- tryCatch(
    .run_qmboots_with_diagnostics(
      dataset,
      nfactors = qmethod_result$brief$nfactors,
      nsteps = nsteps,
      rotation = qmethod_result$brief$rotation,
      indet = alignment,
      progress_callback = progress_callback
    ),
    error = function(error) {
      qindtest_error <- conditionMessage(error)
      fallback <- .run_procrustes_bootstrap_with_diagnostics(
        dataset, qmethod_result, nsteps, progress_callback
      )
      fallback$messages <- unique(c(
        fallback$messages,
        paste0(
          "qindtest alignment failed; the batch was repeated with " ,
          "orthogonal Procrustes alignment: ", qindtest_error
        )
      ))
      fallback$alignment_fallback <- list(
        primary_method = "qindtest",
        fallback_method = "orthogonal Procrustes (qapproach implementation)",
        fallback_succeeded = TRUE,
        qindtest_error = qindtest_error
      )
      fallback$alignment_method <-
        "qindtest with orthogonal Procrustes fallback"
      fallback
    }
  )
  fallback_diagnostics <- output$alignment_fallback
  initial_alignment_diagnostics <- output$alignment_diagnostics
  order_log <- tryCatch(
    output$result$indet.tests[[2L]]$torder,
    error = function(error) NULL
  )
  invalid_iterations <- if (is.data.frame(order_log) && ncol(order_log)) {
    which(vapply(order_log, function(value) {
      any(grepl("ERROR in ORDER swap", as.character(value), fixed = TRUE))
    }, logical(1)))
  } else {
    integer()
  }
  if (length(invalid_iterations)) {
    for (factor in seq_along(output$result$full.bts.res)) {
      for (component in c("flagged", "zsc", "loa")) {
        output$result$full.bts.res[[factor]][[component]][
          , invalid_iterations
        ] <- NA
      }
    }
    output$warnings <- unique(c(
      output$warnings,
      paste0(
        "qindtest could not uniquely align bootstrap iteration",
        if (length(invalid_iterations) == 1L) " " else "s ",
        paste(invalid_iterations, collapse = ", "), "."
      )
    ))
  }
  negative_iterations <- integer()
  no_positive_flag_iterations <- integer()
  for (factor in seq_along(output$result$full.bts.res)) {
    loa <- output$result$full.bts.res[[factor]]$loa
    flg <- output$result$full.bts.res[[factor]]$flagged
    if (!is.matrix(loa) || !is.matrix(flg)) next
    negative_cells <- flg & is.finite(loa) & loa < 0
    positive_cells <- flg & is.finite(loa) & loa >= 0
    negative_cells[is.na(negative_cells)] <- FALSE
    positive_cells[is.na(positive_cells)] <- FALSE
    negative_iterations <- union(negative_iterations, which(colSums(negative_cells) > 0))
    no_positive_flag_iterations <- union(no_positive_flag_iterations, which(colSums(positive_cells) < 2L))
  }
  # Negative flagged loadings are valid numerical bootstrap outcomes. They
  # remain available for opposition summaries and confidence intervals, but
  # do not count as positive agreement. Only alignment failures and factors
  # without two positive flags invalidate an iteration.
  strict_invalid <- sort(unique(c(invalid_iterations, no_positive_flag_iterations)))
  if (length(strict_invalid)) {
    for (factor in seq_along(output$result$full.bts.res)) {
      for (component in c("flagged", "zsc", "loa")) {
        output$result$full.bts.res[[factor]][[component]][, strict_invalid] <- NA
      }
    }
    output$warnings <- unique(c(output$warnings,
      paste0("Invalid bootstrap iterations discarded: ",
             paste(strict_invalid, collapse = ", "),
             ". Reasons are recorded in bootstrap diagnostics.")))
  }
  output$alignment_diagnostics <- c(list(
    method = if (is.null(fallback_diagnostics)) {
      "qindtest"
    } else {
      "qindtest with orthogonal Procrustes fallback"
    },
    primary_method = "qindtest",
    fallback_method = if (is.null(fallback_diagnostics)) NA_character_ else
      fallback_diagnostics$fallback_method,
    fallback_succeeded = if (is.null(fallback_diagnostics)) FALSE else TRUE,
    qindtest_error = if (is.null(fallback_diagnostics)) NA_character_ else
      fallback_diagnostics$qindtest_error,
    invalid_iterations = invalid_iterations,
    valid_iterations = setdiff(seq_len(as.integer(nsteps)), strict_invalid),
    strict_invalid_iterations = strict_invalid,
    invalid_reasons = list(
      alignment = invalid_iterations,
      negative_loading_flag = negative_iterations,
      factor_without_two_positive_flags = no_positive_flag_iterations
    )
  ), if (is.null(fallback_diagnostics)) list() else list(
    procrustes = initial_alignment_diagnostics
  ))
  output$alignment_method <- if (is.null(fallback_diagnostics)) {
    "qindtest"
  } else {
    "qindtest with orthogonal Procrustes fallback"
  }
  output$alignment_fallback <- NULL
  output
}

.combine_qmboots_diagnostics <- function(batch_diagnostics) {
  nonempty <- Filter(function(x) is.data.frame(x) && nrow(x), batch_diagnostics)
  conditions <- if (length(nonempty)) {
    do.call(rbind, nonempty)
  } else {
    data.frame(
      Batch = integer(), Type = character(), Condition = character(),
      check.names = FALSE
    )
  }
  conditions$Classification <- if (nrow(conditions)) {
    vapply(seq_len(nrow(conditions)), function(index) {
      .qmethod_condition_classification(
        conditions$Condition[index], conditions$Type[index]
      )
    }, character(1))
  } else character()
  conditions <- conditions[
    c("Batch", "Type", "Classification", "Condition")
  ]
  unique_conditions <- unique(
    conditions[c("Type", "Classification", "Condition")]
  )
  unclassified <- unique(conditions$Condition[
    conditions$Classification == "unclassified warning"
  ])
  list(
    batches_with_conditions = if (nrow(conditions)) {
      length(unique(conditions$Batch))
    } else 0L,
    condition_count = nrow(conditions),
    distinct_conditions = unique_conditions,
    conditions_by_batch = conditions,
    known_conditions = unique_conditions[
      unique_conditions$Classification != "unclassified warning", ,
      drop = FALSE
    ],
    unclassified_warnings = unclassified
  )
}

.signal_qmboots_diagnostics <- function(
    diagnostics, context = "Bootstrap",
    diagnostics_path = "bootstrap$diagnostics") {
  if (!is.list(diagnostics)) return(invisible(diagnostics))
  known <- diagnostics$known_conditions
  if (is.data.frame(known) && nrow(known)) {
    message(
      "Information:\n\n",
      context, " encountered recognized diagnostic conditions.\n",
      "See results$diagnostics for details, including alignment fallbacks ",
      "and any invalid iterations and reasons for their discard."
    )
  }
  unclassified <- diagnostics$unclassified_warnings
  if (length(unclassified)) {
    for (text in unclassified) {
      warning(
        "\n", context, " produced an unclassified warning: ", text,
        "\nDiagnostics are available in ", diagnostics_path, ".",
        call. = FALSE
      )
    }
  }
  invisible(diagnostics)
}

.run_final_qmethod <- function(dataset, nfactors, rotation) {
  ranking_correlations <- stats::cor(dataset)
  correlation_eigenvalues <- eigen(
    ranking_correlations, symmetric = TRUE, only.values = TRUE
  )$values
  smallest_eigenvalue <- min(correlation_eigenvalues)
  eigenvalue_tolerance <- 100 * nrow(ranking_correlations) *
    .Machine$double.eps * max(1, max(abs(correlation_eigenvalues)))
  numerical_singularity <- smallest_eigenvalue >= -eigenvalue_tolerance

  observed_warnings <- character()
  known_warning <- function(text) {
    grepl("rotation method selected is not standard", text, fixed = TRUE) ||
      grepl("Matrix was not positive definite, smoothing was done", text,
            fixed = TRUE) ||
      grepl("negative loadings are flagged", text, fixed = TRUE)
  }
  qmethod_result <- withCallingHandlers(
    qmethod::qmethod(
      dataset, nfactors = nfactors, rotation = rotation, silent = TRUE
    ),
    warning = function(condition) {
      text <- conditionMessage(condition)
      observed_warnings <<- c(observed_warnings, text)
      smoothing_warning <- grepl(
        "Matrix was not positive definite, smoothing was done",
        text,
        fixed = TRUE
      )
      if (known_warning(text) &&
          (!smoothing_warning || numerical_singularity)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  observed_warnings <- unique(observed_warnings)

  rotation_warning <- any(grepl(
    "rotation method selected is not standard",
    observed_warnings,
    fixed = TRUE
  ))
  smoothing_warning <- any(grepl(
    "Matrix was not positive definite, smoothing was done",
    observed_warnings,
    fixed = TRUE
  ))
  negative_flagging_warning <- any(grepl(
    "negative loadings are flagged",
    observed_warnings,
    fixed = TRUE
  ))

  negative_flagging <- data.frame(
    `Ranking ID` = character(),
    Perspective = character(),
    Loading = numeric(),
    check.names = FALSE
  )
  if (!is.null(qmethod_result$flagged) && !is.null(qmethod_result$loa)) {
    flagged <- as.matrix(qmethod_result$flagged)
    loadings <- as.matrix(qmethod_result$loa)
    negative_cells <- which(flagged & loadings < 0, arr.ind = TRUE)
    if (nrow(negative_cells)) {
      ranking_ids <- row.names(loadings)
      if (is.null(ranking_ids)) ranking_ids <- as.character(seq_len(nrow(loadings)))
      negative_flagging <- data.frame(
        `Ranking ID` = ranking_ids[negative_cells[, "row"]],
        Perspective = paste("Perspective", negative_cells[, "col"]),
        Loading = loadings[negative_cells],
        check.names = FALSE,
        row.names = NULL
      )
    }
  }

  routine_information <- character()
  if (smoothing_warning && numerical_singularity) {
    routine_information <- c(
      routine_information,
      paste0(
        "The ranking correlation matrix was numerically smoothed because it ",
        "was not positive definite; the observed negative eigenvalues were ",
        "within numerical tolerance."
      )
    )
  }

  known_patterns <- c(
    "rotation method selected is not standard",
    "Matrix was not positive definite, smoothing was done",
    "negative loadings are flagged"
  )
  known_indices <- vapply(observed_warnings, function(text) {
    any(vapply(known_patterns, grepl, logical(1), x = text, fixed = TRUE))
  }, logical(1))
  diagnostics <- list(
    rotation = list(
      method = rotation,
      intentionally_selected = identical(rotation, "quartimax"),
      qmethod_information_observed = rotation_warning
    ),
    correlation_matrix = list(
      smoothing_performed = smoothing_warning,
      smallest_eigenvalue = smallest_eigenvalue,
      numerical_tolerance = eigenvalue_tolerance,
      within_numerical_tolerance = numerical_singularity
    ),
    negative_flagging = negative_flagging,
    qmethod_warnings = observed_warnings,
    unclassified_qmethod_warnings = observed_warnings[!known_indices]
  )
  list(qmethod_result = qmethod_result, diagnostics = diagnostics)
}

.write_screeplot <- function(file, principal_component_fit) {
  grDevices::pdf(file, paper = "a4r")
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  rankings_eigenvalues <- principal_component_fit$sdev^2
  variance_proportions <- rankings_eigenvalues / sum(rankings_eigenvalues)
  factor_indices <- seq_along(variance_proportions)
  graphics::par(mar = c(5.1, 6.4, 1, 1))
  graphics::plot(
    factor_indices, variance_proportions,
    type = "b", main = "", xlab = "Unrotated factors", ylab = "",
    xaxt = "n", yaxt = "n"
  )
  graphics::axis(1, at = factor_indices)
  variance_ticks <- pretty(c(0, max(variance_proportions)))
  graphics::axis(
    2, at = variance_ticks,
    labels = paste0(round(100 * variance_ticks), "%"), las = 1
  )
  graphics::mtext("Proportion of total variance", side = 2, line = 4.8)
  graphics::abline(h = 0.05, col = "darkgrey", lty = 2)
  graphics::lines(
    stats::predict(stats::lm(variance_proportions ~ factor_indices)),
    col = "red"
  )
  invisible(file)
}

qapproach <- function(dataset, nfactors = "criteria", rotation = "quartimax",
                      load_perc = 0.8, min_load_perc = 0.5, morethan5 = FALSE,
                      screeplot_file = NULL,
                      repair_distributions = TRUE,
                      distribution_repair_steps = NULL,
                      distribution_repair_seed = NULL,
                      distribution_repair_max_attempt_multiplier = 10L){
  datasetname <- paste(deparse(substitute(dataset)), collapse = "")
  dataset <- as.data.frame(dataset)
  if (nrow(dataset) < 3L || ncol(dataset) < 2L) {
    stop("dataset must contain at least 3 statements and 2 rankings.")
  }
  if (!all(vapply(dataset, is.numeric, logical(1))) || anyNA(dataset)) {
    stop("dataset must contain only numeric, non-missing ranking values.")
  }
  if (!is.logical(repair_distributions) || length(repair_distributions) != 1L ||
      is.na(repair_distributions)) {
    stop("repair_distributions must be TRUE or FALSE.")
  }
  if (!is.null(distribution_repair_steps) &&
      (!is.numeric(distribution_repair_steps) ||
       length(distribution_repair_steps) != 1L ||
       !is.finite(distribution_repair_steps) ||
       distribution_repair_steps < 1L ||
       distribution_repair_steps != as.integer(distribution_repair_steps))) {
    stop("distribution_repair_steps must be NULL or one positive whole number.")
  }
  if (!is.null(distribution_repair_seed) &&
      (!is.numeric(distribution_repair_seed) ||
       length(distribution_repair_seed) != 1L ||
       !is.finite(distribution_repair_seed) ||
       distribution_repair_seed != as.integer(distribution_repair_seed))) {
    stop("distribution_repair_seed must be NULL or one whole number.")
  }
  if (!is.numeric(distribution_repair_max_attempt_multiplier) ||
      length(distribution_repair_max_attempt_multiplier) != 1L ||
      !is.finite(distribution_repair_max_attempt_multiplier) ||
      distribution_repair_max_attempt_multiplier < 1L ||
      distribution_repair_max_attempt_multiplier !=
        as.integer(distribution_repair_max_attempt_multiplier)) {
    stop(
      "distribution_repair_max_attempt_multiplier must be a positive whole number."
    )
  }
  if(grepl("prepare_rankings", datasetname) == TRUE){
    datasetname <- gsub("prepare_rankings(", "", datasetname, fixed = TRUE)
    datasetname <- gsub(")", "", datasetname, fixed = TRUE)
  }
  if(grepl("add", datasetname) == TRUE){
    datasetname <- strsplit(datasetname, ",", fixed = TRUE)[[1L]][1L]
    datasetname <- trimws(datasetname)
  }
  
  nfactors <- match.arg(as.character(nfactors), c(paste0(1:10), "criteria"))
  nfactors_forced <- !identical(nfactors, "criteria")
  requested_nfactors <- if (nfactors_forced) as.integer(nfactors) else NA_integer_
  nfactors_capped <- FALSE
  rotation <- match.arg(rotation, c("varimax", "quartimax"))
  
  principal_component_fit <- stats::prcomp(dataset)
  principalcomponents <- base::summary(principal_component_fit)
  if (!is.null(screeplot_file)) {
    if (!is.character(screeplot_file) || length(screeplot_file) != 1L ||
        is.na(screeplot_file) || !nzchar(trimws(screeplot_file))) {
      stop("screeplot_file must be NULL or one non-empty file path.")
    }
    if (tolower(tools::file_ext(screeplot_file)) != "pdf") {
      stop("screeplot_file must have a .pdf extension.")
    }
    screeplot_parent <- dirname(screeplot_file)
    if (!dir.exists(screeplot_parent)) {
      dir.create(screeplot_parent, recursive = TRUE)
    }
    .write_screeplot(screeplot_file, principal_component_fit)
  }
  
  factor_selection <- NULL
  if(nfactors %in% paste0(1:10)){nfactors <- as.numeric(nfactors)}
  if(nfactors == "criteria"){
    factor_selection <- .optimize_factor_selection(
      dataset,
      rotation = rotation,
      load_perc = load_perc,
      min_load_perc = min_load_perc,
      morethan5 = morethan5
    )
    nfactors <- factor_selection$nfactors
    if (nfactors == 1) {
      message("Note: There is a consensus perspective.\n")
    }
    if (isTRUE(factor_selection$threshold_adjusted)) {
      message(
        "The factor-loading threshold was automatically adjusted from ",
        round(factor_selection$requested_load_perc, 3),
        " to ", round(factor_selection$effective_load_perc, 3),
        ". The selected ", nfactors, "-factor solution flags ",
        round(100 * factor_selection$achieved_load_perc, 1),
        "% of rankings.\n"
      )
    }
    }
  
  max_factors <- min(nrow(dataset), ncol(dataset))
  if (length(nfactors) != 1L || is.na(nfactors) || nfactors < 1L) {
    stop("nfactors must be a whole number from 1 to 10.")
  }
  if (isTRUE(nfactors_forced) && nfactors > max_factors) {
    message(
      "The selected dataset can support at most ", max_factors,
      " perspectives based on its number of statements and rankings. " ,
      "The analysis will use ", max_factors, " perspectives.\n"
    )
    nfactors <- max_factors
    nfactors_capped <- TRUE
  }
  discarded_perspectives <- 0L
  repeat {
    final_qmethod <- tryCatch(
      .run_final_qmethod(dataset, nfactors, rotation),
      error = function(error) error
    )
    if (!inherits(final_qmethod, "error")) break
    recoverable <- grepl(
      "statementzscores|no flagged", conditionMessage(final_qmethod),
      ignore.case = TRUE
    )
    if (!recoverable || nfactors <= 1L) stop(final_qmethod)
    nfactors <- nfactors - 1L
    discarded_perspectives <- discarded_perspectives + 1L
  }
  Q <- final_qmethod$qmethod_result
  diagnostics <- final_qmethod$diagnostics
  positive_by_perspective <- colSums(
    as.matrix(Q$flagged) & as.matrix(Q$loa) > 0, na.rm = TRUE
  )
  if (any(positive_by_perspective < 1L)) {
    retained_factors <- sum(positive_by_perspective >= 1L)
    if (retained_factors < 1L) {
      stop("The forced factor solution contains no perspective with agreeing rankings.")
    }
    discarded_perspectives <- discarded_perspectives + nfactors - retained_factors
    final_qmethod <- .run_final_qmethod(dataset, retained_factors, rotation)
    Q <- final_qmethod$qmethod_result
    diagnostics <- final_qmethod$diagnostics
    nfactors <- retained_factors
  }
  positive_flagged <- as.matrix(Q$flagged) & as.matrix(Q$loa) > 0
  negative_flagged_count <- sum(as.matrix(Q$flagged) & as.matrix(Q$loa) < 0, na.rm = TRUE)
  flagging <- sum(positive_flagged)
  factorloading <- 1/ncol(dataset)*flagging
  # summary(Q)
  factors <- data.frame(t(Q$zsc_n), row.names = paste0(datasetname, "_f", 1:Q$brief$nfactors))
  
  distribution_check <- .check_perspective_distributions(
    factors, Q$brief$nstat
  )
  distribution_repair <- list(
    enabled = repair_distributions,
    check_performed = TRUE,
    repair_needed = length(distribution_check$broken_indices) > 0L,
    affected_perspectives = distribution_check$broken_perspectives,
    repaired_perspectives = character(),
    original_perspectives = if (length(distribution_check$broken_indices)) {
      factors[distribution_check$broken_indices, , drop = FALSE]
    } else NULL,
    distribution_differences = distribution_check$details[
      distribution_check$details$Difference != 0L, , drop = FALSE
    ],
    changes = data.frame(),
    bootstrap_diagnostics = NULL,
    target_valid_iterations = 0L,
    attempted_iterations = 0L,
    valid_iterations = 0L,
    discarded_iterations = 0L,
    verified = all(distribution_check$valid),
    cps_recalculated = FALSE,
    seed = distribution_repair_seed,
    status = if (all(distribution_check$valid)) "not needed" else "pending"
  )

  if (length(distribution_check$broken_indices) &&
      !isTRUE(repair_distributions)) {
    distribution_repair$status <- "disabled"
    for (perspective in distribution_check$broken_perspectives) {
      warning(
        perspective,
        " does not follow the expected ranking distribution.\n",
        "Automatic correction is disabled. To correct the distribution ",
        "automatically using bootstrap results, rerun qapproach() with ",
        "repair_distributions = TRUE. Alternatively, use ",
        "manually_repair_perspective_distributions() to inspect and ",
        "control the repair explicitly.",
        call. = FALSE
      )
    }
  }

  if (length(distribution_check$broken_indices) &&
      isTRUE(repair_distributions)) {
    target_valid_steps <- if (is.null(distribution_repair_steps)) {
      40L * ncol(dataset)
    } else {
      as.integer(distribution_repair_steps)
    }
    distribution_repair$target_valid_iterations <- target_valid_steps
    repair_error <- NULL
    bootstrap_reference <- tryCatch(
      .bootstrap_distribution_references(
        dataset = dataset,
        qmethod_result = Q,
        perspective_indices = distribution_check$broken_indices,
        target_valid_steps = target_valid_steps,
        seed = distribution_repair_seed,
        max_attempt_multiplier =
          distribution_repair_max_attempt_multiplier
      ),
      error = function(error) {
        repair_error <<- conditionMessage(error)
        NULL
      }
    )
    if (!is.null(bootstrap_reference)) {
      distribution_repair$attempted_iterations <-
        bootstrap_reference$requested_steps
      distribution_repair$valid_iterations <- bootstrap_reference$valid_steps
      distribution_repair$discarded_iterations <-
        bootstrap_reference$discarded_steps
      distribution_repair$bootstrap_diagnostics <-
        bootstrap_reference$diagnostics
      .signal_qmboots_diagnostics(
        distribution_repair$bootstrap_diagnostics,
        context = "Automatic distribution-repair bootstrap",
        diagnostics_path = "results$`distribution repair`$bootstrap_diagnostics"
      )
      change_log <- list()
      repair_failed <- FALSE
      repaired_factors <- factors
      for (position in seq_along(distribution_check$broken_indices)) {
        perspective_index <- distribution_check$broken_indices[position]
        repaired <- tryCatch(
          .automatically_repair_perspective_distribution(
            original_ranking = as.numeric(factors[perspective_index, ]),
            bootstrap_average_zscores =
              bootstrap_reference$average_zscores[position, ],
            target_ranking = distribution_check$target_ranking,
            statement_names = colnames(factors)
          ),
          error = function(error) {
            repair_error <<- conditionMessage(error)
            NULL
          }
        )
        if (is.null(repaired)) {
          repair_failed <- TRUE
          break
        }
        repaired_factors[perspective_index, ] <- repaired$ranking
        if (nrow(repaired$changes)) {
          repaired$changes <- data.frame(
            Perspective = paste("Perspective", perspective_index),
            repaired$changes,
            check.names = FALSE,
            row.names = NULL
          )
          change_log[[length(change_log) + 1L]] <- repaired$changes
        }
      }
      repaired_check <- if (repair_failed) NULL else
        .check_perspective_distributions(repaired_factors, Q$brief$nstat)
      repaired_indices_valid <- !is.null(repaired_check) && all(
        repaired_check$valid[distribution_check$broken_indices]
      )
      if (repaired_indices_valid) {
        factors <- repaired_factors
        distribution_repair$repaired_perspectives <-
          distribution_check$broken_perspectives
        distribution_repair$changes <- if (length(change_log)) {
          do.call(rbind, change_log)
        } else data.frame()
        distribution_repair$verified <- TRUE
        distribution_repair$status <- "repaired"
        message(
          "Note: Broken ranking distribution",
          if (length(distribution_check$broken_indices) == 1L) "" else "s",
          " detected in ",
          paste(distribution_check$broken_perspectives, collapse = ", "),
          " and automatically repaired using ",
          bootstrap_reference$valid_steps,
          " valid bootstrap iterations. ",
          nrow(distribution_repair$changes),
          " statement value",
          if (nrow(distribution_repair$changes) == 1L) " was" else "s were",
          " changed. Consensus priority scores were not recalculated. ",
          "Set repair_distributions = FALSE to retain and investigate the ",
          "original distribution",
          if (length(distribution_check$broken_indices) == 1L) "." else "s.",
          "\n"
        )
      } else if (is.null(repair_error)) {
        repair_error <- "The repaired distribution did not pass verification."
      }
    }
    if (distribution_repair$status != "repaired") {
      distribution_repair$status <- "failed"
      distribution_repair$error <- repair_error
      warning(
        "The automatic ranking-distribution repair failed: ", repair_error,
        " The original perspective rankings were retained.",
        call. = FALSE
      )
    }
  }
  
  weighted_zscores <- .weighted_z_scores(
    Q$zsc, Q$f_char$characteristics$eigenvals
  )
  cpscores <- stats::pnorm(weighted_zscores)
  
  positive_by_ranking <- apply(
    as.matrix(Q$flagged) & as.matrix(Q$loa) > 0, 1L, any, na.rm = TRUE
  )
  negative_by_ranking <- apply(
    as.matrix(Q$flagged) & as.matrix(Q$loa) < 0, 1L, any, na.rm = TRUE
  )
  undecided_rankings <- sum(!(positive_by_ranking | negative_by_ranking))
  if (flagging + negative_flagged_count + undecided_rankings != ncol(dataset)) {
    stop(
      "The agreeing, opposing, and undecided ranking counts do not sum to ",
      "the number of input rankings."
    )
  }
  target_consensus <- if (is.null(factor_selection)) {
    load_perc
  } else {
    factor_selection$requested_load_perc
  }
  effective_consensus <- if (is.null(factor_selection)) {
    factorloading
  } else {
    factor_selection$achieved_load_perc
  }
  analysis_summary <- data.frame(ncol(dataset),
                        (length(which(principalcomponents$importance[2,] >= 0.05))),
                        nfactors,
                        flagging,
                        negative_flagged_count,
                        undecided_rankings,
                        target_consensus,
                        effective_consensus)
  names(analysis_summary) <- c(
    "Input rankings", "Unrotated principal components",
    "Group perspectives analysed",
    "Agreeing rankings", "Opposing rankings", "Undecided rankings",
    "Target consensus", "Effective consensus"
  )
  
  factor_selection_diagnostics <- if (is.null(factor_selection)) NULL else
    factor_selection$diagnostics
  if (!is.null(factor_selection)) factor_selection$diagnostics <- NULL
  centralized_diagnostics <- list(
    analysis = diagnostics,
    factor_selection = factor_selection_diagnostics,
    distribution_repair = distribution_repair$bootstrap_diagnostics,
    negative_flagging = diagnostics$negative_flagging
  )
  results <- list("dataset name" = datasetname,
                  "Q method results" = Q,
                  "summary" = analysis_summary,
                  "factor_selection" = factor_selection,
                  "perspectives" = factors,
                  "weighted z-scores" = weighted_zscores,
                  "cp-scores" = cpscores,
                  "distribution repair" = distribution_repair,
                  `perspectives without agreeing rankings discarded` =
                    discarded_perspectives,
                  `nfactors forced` = nfactors_forced,
                  `requested nfactors` = requested_nfactors,
                  `nfactors capped` = nfactors_capped,
                  `maximum feasible nfactors` = max_factors,
                  "diagnostics" = centralized_diagnostics)
  diagnostic_paths <- character()
  if (length(diagnostics$qmethod_warnings) ||
      length(diagnostics$unclassified_qmethod_warnings) ||
      (!is.null(factor_selection_diagnostics) &&
       length(factor_selection_diagnostics$conditions_by_candidate)) ||
      !is.null(distribution_repair$bootstrap_diagnostics)) {
    message(
      "Information:\n\nDifferent messages and warnings came up. See ",
      "results$diagnostics for details.\n"
    )
  }
  negative_flagging <- results$diagnostics$negative_flagging
  if (is.data.frame(negative_flagging) && nrow(negative_flagging)) {
    message(
      nrow(negative_flagging), " ranking(s) have negative loadings on their ",
      "group perspective(s). This represents statistical opposition rather ",
      "than agreement and may require further dialogue to reach a consensus. These rankings are ",
      "excluded from calculating the positive degree of consensus. The ",
      "affected rankings are available in results$diagnostics$negative_flagging " ,
      "and can be marked in network figures.\n"
    )
  }
  return(results)
}

manually_repair_perspective_distributions <- function(
    results, bootstrap = NULL, perspective = NULL,
    statement = NULL, value = NULL, interactive = TRUE,
    verbose = interactive){
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("verbose must be TRUE or FALSE.")
  }
  check <- .check_perspective_distributions(
    results$perspectives,
    results$`Q method results`$brief$nstat
  )
  if (!length(check$broken_indices)) {
    if (isTRUE(verbose)) cat("All ranking distributions look fine.\n")
    return(invisible(results))
  }

  if (isTRUE(verbose)) {
    cat(
      "The following ranking distribution(s) do not follow the required ",
      "distribution:\n",
      paste(check$broken_perspectives, collapse = " / "), "\n\n",
      sep = ""
    )
  }
  broken_details <- check$details[
    check$details$Perspective %in% check$broken_perspectives,
    ,
    drop = FALSE
  ]
  if (isTRUE(verbose)) print(broken_details, row.names = FALSE)

  if (is.null(bootstrap)) {
    stop(
      "Supply the output of qaboots() as bootstrap to repair the broken ",
      "distribution(s).",
      call. = FALSE
    )
  }

  if (is.numeric(perspective) && length(perspective) == 1L) {
    if (!is.finite(perspective) || perspective != as.integer(perspective) ||
        perspective < 1L || perspective > nrow(results$perspectives)) {
      stop("perspective is not a valid perspective number.", call. = FALSE)
    }
    perspective <- row.names(results$perspectives)[as.integer(perspective)]
  }
  if (is.null(perspective) && length(check$broken_perspectives) == 1L) {
    perspective <- check$broken_perspectives
  }
  if (is.null(perspective) && isTRUE(interactive)) {
    perspective <- readline(
      prompt = paste0(
        "Enter the perspective to repair (",
        paste(check$broken_perspectives, collapse = ", "), "): "
      )
    )
  }
  if (is.null(perspective)) {
    stop(
      "Supply perspective when more than one distribution is broken and ",
      "interactive = FALSE.",
      call. = FALSE
    )
  }
  if (length(perspective) != 1L ||
      !(perspective %in% check$broken_perspectives)) {
    stop(
      "perspective must identify one of the broken distributions: ",
      paste(check$broken_perspectives, collapse = ", "),
      call. = FALSE
    )
  }

  nstat <- results$`Q method results`$brief$nstat
  distribution_error_factor <- which(row.names(results$perspectives) == perspective)
  distribution_error_comparison <- t(data.frame(
    `standard Q` = t(results$perspectives)[, distribution_error_factor],
    `bootstrap` = bootstrap$`bootstrap results`$`zscore-stats`$
      `Bootstraped factor scores`[, distribution_error_factor]
  ))
  distributiontobe <- distributiondetermination(nstat)
  perspective_details <- broken_details[
    broken_details$Perspective == perspective,
    ,
    drop = FALSE
  ]
  too_often <- perspective_details$`Ranking value`[
    perspective_details$Difference > 0L
  ]
  too_few <- perspective_details$`Ranking value`[
    perspective_details$Difference < 0L
  ]
  if (isTRUE(verbose)) {
    print(distribution_error_comparison)
    cat(paste0(
      "\nIn the results of ", perspective,
      ", the distribution is incorrect as follows:\n",
      "ranking value too often: ", too_often, "\n",
      "ranking value too few times: ", too_few, "\n"
    ))
  }
  select_stat <- statement
  if (is.null(select_stat) && isTRUE(interactive)) {
    select_stat <- suppressWarnings(as.numeric(readline(
      prompt = "Enter which statement you want to edit: "
    )))
  }
  if (is.null(select_stat)) stop("Supply statement when interactive = FALSE.")
  if (!(select_stat %in% seq_len(nstat))) {
    if (!isTRUE(interactive)) stop("statement must be between 1 and ", nstat)
    repeat {
      select_stat <- suppressWarnings(as.numeric(readline(prompt = paste0(
        "The statement must be one from 1 to ", nstat, ". Enter again: "
      ))))
      if (select_stat %in% seq_len(nstat)) break
    }
  }
  select_value <- value
  if (is.null(select_value) && isTRUE(interactive)) {
    select_value <- suppressWarnings(as.numeric(readline(prompt = paste0(
      "Enter the value you want to give statement ", select_stat, ": "
    ))))
  }
  if (is.null(select_value)) stop("Supply value when interactive = FALSE.")
  valid_values <- seq(min(distributiontobe$values), max(distributiontobe$values))
  if (!(select_value %in% valid_values)) {
    if (!isTRUE(interactive)) {
      stop("value must be between ", min(valid_values), " and ", max(valid_values), ".")
    }
    repeat {
      select_value <- suppressWarnings(as.numeric(readline(prompt = paste0(
        "The value must be one from ", min(distributiontobe$values), " to ",
        max(distributiontobe$values), ". Enter again: "
      ))))
      if (select_value %in% valid_values) break
    }
  }
  results$perspectives[distribution_error_factor, select_stat] <- select_value
  if (isTRUE(verbose)) {
    cat(paste0(
      "The distribution error of ", perspective,
      " is corrected as specified.\n"
    ))
  }
  repaired_check <- .check_perspective_distributions(
    results$perspectives,
    nstat
  )
  if (isTRUE(verbose)) {
    if (!length(repaired_check$broken_indices)) {
      cat("All ranking distributions look fine after the edit.\n")
    } else {
      cat(
        "The following ranking distribution(s) still require attention:\n",
        paste(repaired_check$broken_perspectives, collapse = " / "), "\n",
        sep = ""
      )
    }
  }
  results
}

not_agreeing <- function(results, status = FALSE) {
  if (!is.logical(status) || length(status) != 1L || is.na(status)) {
    stop("status must be TRUE or FALSE.")
  }
  qmethod_results <- results$`Q method results`
  flagged <- as.matrix(qmethod_results$flagged)
  loadings <- as.matrix(qmethod_results$loa)
  if (!identical(dim(flagged), dim(loadings))) {
    stop("The flagging and loading matrices in results must have matching dimensions.")
  }

  agreeing <- rowSums(
    flagged & is.finite(loadings) & loadings > 0,
    na.rm = TRUE
  ) > 0L
  opposing <- rowSums(
    flagged & is.finite(loadings) & loadings < 0,
    na.rm = TRUE
  ) > 0L
  selected <- which(!agreeing)
  if (!length(selected)) {
    message("All rankings agree with a group perspective.")
    return(invisible(NULL))
  }

  ranking_ids <- rownames(flagged)
  if (is.null(ranking_ids)) ranking_ids <- colnames(qmethod_results$dataset)
  if (is.null(ranking_ids)) ranking_ids <- as.character(seq_len(nrow(flagged)))
  selected_ids <- ranking_ids[selected]
  dataset_columns <- match(selected_ids, colnames(qmethod_results$dataset))
  if (anyNA(dataset_columns)) {
    stop("The ranking identifiers do not match the Q method dataset columns.")
  }
  nf <- data.frame(t(
    qmethod_results$dataset[, dataset_columns, drop = FALSE]
  ))
  nf[] <- lapply(nf, as.numeric)
  row.names(nf) <- selected_ids
  if (isTRUE(status)) {
    nf <- data.frame(
      Status = ifelse(
        opposing[selected], "Opposing", "Undecided"
      ),
      nf,
      check.names = FALSE,
      row.names = selected_ids
    )
  }
  nf
}

#' Print the Q approach analysis summary
#'
#' @param results An object returned by [qapproach()].
#' @param print_table Print the summary table; set to `FALSE` to return it
#'   silently.
#' @param file Optional CSV file path or name. The three tables are written
#'   below one another in a single padded CSV file, separated by two blank rows.
#'   If the `.csv` extension is omitted, it is added automatically.
#' @return Invisibly, a named list containing the analysis summary, group
#'   perspectives, and consensus priority scores tables. The summary cp-scores
#'   and cp-score range are rounded to two decimal places; the source values in
#'   `results` remain unchanged.
#' @export
summary <- function(results, print_table = TRUE, file = NULL) {
  if (!is.list(results) || !is.data.frame(results$summary)) {
    stop("results must be an object returned by qapproach().")
  }
  if (!is.logical(print_table) || length(print_table) != 1L ||
      is.na(print_table)) {
    stop("print_table must be TRUE or FALSE.")
  }
  if (!is.null(file) &&
      (!is.character(file) || length(file) != 1L ||
       is.na(file) || !nzchar(trimws(file)))) {
    stop("file must be NULL or one non-empty file path or name.")
  }

  statement_names <- colnames(results$perspectives)
  if (is.null(statement_names) || any(!nzchar(statement_names))) {
    statement_names <- names(results$`cp-scores`)
  }
  if (is.null(statement_names) || any(!nzchar(statement_names))) {
    statement_names <- paste("Statement", seq_len(ncol(results$perspectives)))
  }
  perspectives <- data.frame(
    Result = paste("Perspective", seq_len(nrow(results$perspectives))),
    as.data.frame(results$perspectives, check.names = FALSE),
    check.names = FALSE,
    row.names = NULL
  )
  names(perspectives)[-1L] <- statement_names
  cp_scores <- data.frame(
    Result = "cp-scores",
    as.list(round(as.numeric(results$`cp-scores`), digits = 2L)),
    check.names = FALSE,
    row.names = NULL
  )
  names(cp_scores)[-1L] <- statement_names
  analysis_summary <- results$summary[, setdiff(
    names(results$summary), "Consensus target adjusted"
  ), drop = FALSE]
  cp_score_range <- range(results$`cp-scores`, na.rm = TRUE)
  analysis_summary$`cp-score range` <- paste0(
    "[", formatC(cp_score_range[1L], format = "f", digits = 2L),
    ", ", formatC(cp_score_range[2L], format = "f", digits = 2L), "]"
  )
  tables <- list(
    `Analysis summary` = analysis_summary,
    `Group perspectives` = perspectives,
    `Consensus priority scores` = cp_scores
  )

  if (!is.null(file)) {
    path <- if (grepl("[.]csv$", file, ignore.case = TRUE)) {
      file
    } else {
      paste0(file, ".csv")
    }
    parent <- dirname(path)
    if (!dir.exists(parent)) dir.create(parent, recursive = TRUE)
    maximum_columns <- max(vapply(tables, ncol, integer(1)))
    pad_row <- function(values = character()) {
      c(as.character(values), rep("", maximum_columns - length(values)))
    }
    report_rows <- list()
    for (table_index in seq_along(tables)) {
      table <- tables[[table_index]]
      report_rows[[length(report_rows) + 1L]] <- pad_row(names(tables)[table_index])
      report_rows[[length(report_rows) + 1L]] <- pad_row(names(table))
      table_rows <- lapply(seq_len(nrow(table)), function(row_index) {
        pad_row(unlist(table[row_index, , drop = FALSE], use.names = FALSE))
      })
      report_rows <- c(report_rows, table_rows)
      if (table_index < length(tables)) {
        report_rows <- c(report_rows, list(pad_row(), pad_row()))
      }
    }
    report <- do.call(rbind, report_rows)
    utils::write.table(
      report,
      path,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = TRUE,
      na = ""
    )
    attr(tables, "csv_file") <- normalizePath(path, mustWork = TRUE)
  }
  if (isTRUE(print_table)) {
    for (table_name in names(tables)) {
      cat(table_name, "\n", sep = "")
      print(tables[[table_name]], row.names = FALSE, right = TRUE)
      cat("\n")
    }
  }
  invisible(tables)
}

nfactordetermination <- function(dataset, rotation, load_perc,
                                 morethan5 = FALSE, min_load_perc = 0.5){
  selection <- .optimize_factor_selection(
    dataset = dataset,
    rotation = rotation,
    load_perc = load_perc,
    min_load_perc = min_load_perc,
    morethan5 = morethan5
  )
  factors <- selection$nfactors
  attr(factors, "factor_selection") <- selection
  factors
}
