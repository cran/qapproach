#' Prepare rankings and run a Q approach analysis
#'
#' `prepare_rankings()` converts participant-by-statement input into the
#' statement-by-ranking orientation used by `qapproach()`. `qapproach()` fits
#' perspectives and computes consensus priority scores. The result retains the
#' underlying eigenvalue-weighted mean z-scores as a technical output. The
#' analysis stores recognized Q method conditions, factor-selection details,
#' automatic distribution-repair diagnostics, and opposing rankings silently
#' in `results$diagnostics`. In particular, the affected ranking identifiers,
#' perspectives, and loadings are available in
#' `results$diagnostics$negative_flagging`. Unclassified conditions remain
#' visible as warnings so that they can be reported and reviewed. The
#' remaining aliases
#' provide factor selection, unflagged rankings, and manual distribution repair.
#'
#' @param dataset A data frame or matrix. Participant rows or already prepared statement rows are accepted.
#' @param id_column Unique participant identifier column, or `NULL`.
#' @param add A list of optional additional rankings.
#' @param status Whether `not_agreeing()` adds a status column distinguishing
#'   opposing and undecided rankings.
#' @param statement_columns Character vector selecting statement columns.
#' @param orientation Either "auto", "participant_rows", or "statement_rows". Automatic mode uses IDs, names, dimensions, and distribution checks.
#' @param nfactors `"criteria"` or an integer from 1 to 10. Automatic selection
#'   normally requires at least two positively and uniquely flagging rankings
#'   per perspective. If no strict solution reaches the target consensus, a
#'   perspective with exactly one agreeing ranking and no opposing rankings may
#'   be retained when it raises the effective consensus above every otherwise
#'   eligible solution. The exception is recorded in the factor-selection
#'   result and diagnostics.
#' @param rotation Either `"quartimax"` (default for the Q approach) or `"varimax"`.
#' @param load_perc Requested proportion of significantly loading rankings.
#' @param min_load_perc Minimum acceptable loading proportion.
#' @param morethan5 Whether automatic selection may evaluate up to ten factors.
#' @param screeplot_file Optional PDF output path for the unrotated-factor
#'   scree plot. `NULL` writes no file.
#' @param repair_distributions Whether to repair broken perspective gradients.
#' @param distribution_repair_steps Target valid repair iterations, or `NULL`.
#' @param distribution_repair_seed Integer seed used for automatic distribution
#'   repair, or `NULL` to use the current random-number state. Defaults to `42L`
#'   so that this exceptional corrective step is reproducible.
#' @param distribution_repair_max_attempt_multiplier Attempt-limit multiplier.
#' @param results A result returned by `qapproach()`.
#' @param bootstrap Optional result from `qaboots()`.
#' @param perspective Perspective name or index to repair.
#' @param statement Statement index to repair.
#' @param value Replacement ranking value.
#' @param interactive Whether to prompt for missing repair choices.
#' @param verbose Whether the manual repair function prints its inspection and
#'   repair details. By default, this follows `interactive`.
#' @return `prepare_rankings()` returns statement-by-ranking data.
#'   `qapproach()` returns the fitted analysis, perspectives, weighted z-scores,
#'   consensus priority scores, repair audit,
#'   and centralized diagnostics in `results$diagnostics`. When automatic
#'   factor selection is used, its details are
#'   available as `<object>$factor_selection`, including the captured diagnostic
#'   messages and warnings at `<object>$diagnostics$factor_selection`. Other
#'   functions return the result described above.
#' @references Geschke, J., Urbach, D., Prescott, G. W., & Fischer, M.(2022). The Q
#' approach to consensus building: integrating diverse perspectives to guide
#' decision-making. *ECOEVORXIV*.
#'   \doi{10.32942/X2F59S}
#' @references Geschke, J. (2024). *Q approach to consensus building*.
#'   \doi{10.5281/zenodo.11518485}
#' @name qapproach
#' @aliases prepare_rankings nfactordetermination not_agreeing manually_repair_perspective_distributions
#' @usage
#' prepare_rankings(dataset, id_column = "ID", statement_columns = NULL,
#'   add = list(NULL), orientation = c("auto", "participant_rows",
#'   "statement_rows"))
#' qapproach(dataset, nfactors = "criteria", rotation = "quartimax",
#'   load_perc = 0.8, min_load_perc = 0.5, morethan5 = FALSE,
#'   screeplot_file = NULL,
#'   repair_distributions = TRUE, distribution_repair_steps = NULL,
#'   distribution_repair_seed = 42L,
#'   distribution_repair_max_attempt_multiplier = 10L)
#' nfactordetermination(dataset, rotation, load_perc, morethan5 = FALSE,
#'   min_load_perc = 0.5)
#' not_agreeing(results, status = FALSE)
#' manually_repair_perspective_distributions(results, bootstrap = NULL,
#'   perspective = NULL, statement = NULL, value = NULL, interactive = TRUE,
#'   verbose = interactive)
#' @examples
#' x <- data.frame(ID = c("A", "B"), stat1 = c(-1, 0),
#'   stat2 = c(0, 1), stat3 = c(1, -1))
#' prepare_rankings(x)
NULL

#' Advanced Q approach bootstrap interfaces
#'
#' Generate Q method bootstrap results or collect a requested number of valid
#' bootstrap iterations of the consensus priority scores. Recognized R messages and
#' warnings emitted by the underlying Q method bootstrap are retained as
#' structured diagnostics without routine console messages; unfamiliar
#' warnings are re-emitted and include the package's bug-report URL.
#' Single-perspective solutions use Procrustes sign alignment. Solutions with
#' two or three perspectives use `qindtest`, with the package's orthogonal
#' Procrustes alignment as a documented fallback if `qindtest` fails; solutions
#' with more than three perspectives use orthogonal Procrustes alignment
#' directly. Flags and z-scores are recalculated after Procrustes alignment.
#'
#' @param results A result returned by `qapproach()`.
#' @param steps Positive bootstrap step count, or `NULL` in the consensus priority score wrapper.
#' @param method Either `"multiplication"` or `"manual"`.
#' @param seed Integer random seed or `NULL`. The default `NULL` uses R's
#'   current random-number state. Supply an integer, such as `42L`, for a
#'   reproducible run.
#' @param target_valid_steps Target valid iterations, or `NULL`.
#' @param valid_steps_per_ranking Default valid iterations per ranking.
#' @param max_batch_steps Maximum iterations requested in one batch.
#' @param max_attempt_multiplier Multiplier limiting total attempts.
#' @param progress Whether to display bootstrap progress. The default uses
#'   `interactive()`. Set explicitly to `TRUE` or `FALSE` to override it.
#' @return A list containing bootstrap estimates, iteration accounting, and
#'   structured diagnostics.
#' @name qapproach-bootstrap
#' @aliases qaboots bootstrap_consensus_priority_scores
#' @usage
#' qaboots(results, steps = 40, method = "multiplication", seed = NULL,
#'   max_batch_steps = 500L, max_attempt_multiplier = 10L,
#'   progress = interactive())
#' bootstrap_consensus_priority_scores(results, steps = NULL, seed = NULL,
#'   target_valid_steps = NULL, valid_steps_per_ranking = 40L,
#'   max_batch_steps = 500L, max_attempt_multiplier = 10L,
#'   progress = interactive())
NULL

#' Plot and export Q approach results
#'
#' Draw perspective rankings, consensus priority scores, networks, spiderwebs, or
#' consensus priority score bootstrap distributions. `write_figure_collection()` combines applicable
#' figures and captions in a multi-page PDF.
#'
#' @param result A Q approach result. For `plot_jitterplot()`, this may instead
#'   be an object returned by `validate()` or a bootstrap result.
#' @param validation Optional object returned by `validate()`, used by
#'   `write_figure_collection()` for the validation jitterplot.
#' @param labels Optional statement labels.
#' @param statement_colors Optional statement colors.
#' @param show_normalized_weighted_z Whether `plot_barplot()` overlays the
#'   weighted z-scores after linearly rescaling them to the observed cp-score
#'   range. This specialist display option defaults to `FALSE`.
#' @param normalized_line_color,normalized_line_width Color and width of the
#'   rescaled weighted-z-score stair line. These settings are used only when
#'   `show_normalized_weighted_z = TRUE`.
#' @param network_labelled Whether ranking identifiers appear in the network.
#' @param network_perspective_label_cex,network_ranking_label_cex Label-size
#'   multipliers for perspective and ranking nodes in `plot_network()`.
#' @param network_dataset_colors,network_synthesis_color,network_cps_color
#'   Optional colors for multi-dataset, synthesis, and cp-score nodes.
#' @param input_network Whether the network represents analytical inputs rather
#'   than agreement and opposition results.
#' @param negative_loadings_marked Whether negative flagged loadings are marked.
#' @param network_arrow_size Arrow-size multiplier for `plot_network()`.
#' @param layout_seed Integer seed for reproducible network-node placement, or
#'   `NULL` to use the current random-number state.
#' @param .mixed_synthesis_inputs Internal logical used by the two-layer network
#'   wrapper when synthesis inputs contain both dataset perspectives and
#'   re-added individual rankings. Manual users should leave this at its
#'   default.
#' @param preset `NULL`, `"sdg"`, `"tca-actions"`, or `"tca-strategies"`.
#' @param file Output path. For individual plot functions, `NULL` draws on the
#'   current graphics device without writing a file. For
#'   `write_figure_collection()`, `file` is required.
#' @param width,height Output dimensions in inches when `file` is supplied.
#' @param figure_assets Optional pre-created internal figure assets.
#' @param two_layered Whether the figure collection contains a two-layered
#'   multi-dataset network caption.
#' @return Plot functions draw on the current device. The writer invisibly
#'   returns the normalized output path.
#' @name qapproach-plots
#' @aliases plot_barplot plot_heatmap plot_jitterplot plot_network plot_spiderweb write_figure_collection
#' @usage
#' plot_barplot(result, labels = NULL, statement_colors = NULL,
#'   network_labelled = FALSE, preset = NULL,
#'   show_normalized_weighted_z = FALSE,
#'   normalized_line_color = "black", normalized_line_width = 1.5,
#'   file = NULL, width = 11.2, height = 6.4)
#' plot_heatmap(result, labels = NULL, statement_colors = NULL,
#'   network_labelled = FALSE, preset = NULL, file = NULL,
#'   width = 11.2, height = 6.4)
#' plot_jitterplot(result, labels = NULL, statement_colors = NULL,
#'   network_labelled = FALSE, preset = NULL, file = NULL,
#'   width = 11.2, height = 6.4)
#' plot_network(result, labels = NULL, statement_colors = NULL,
#'   network_labelled = FALSE, network_perspective_label_cex = 2,
#'   network_ranking_label_cex = 1.1, network_dataset_colors = NULL,
#'   network_synthesis_color = NULL, network_cps_color = NULL,
#'   input_network = FALSE, negative_loadings_marked = TRUE,
#'   network_arrow_size = 0.56, layout_seed = NULL,
#'   .mixed_synthesis_inputs = FALSE,
#'   preset = NULL, file = NULL, width = 11.2, height = 6.4)
#' plot_spiderweb(result, labels = NULL, statement_colors = NULL,
#'   network_labelled = FALSE, preset = NULL, file = NULL,
#'   width = 8.3, height = 8.3)
#' write_figure_collection(file, result, validation = NULL, labels = NULL,
#'   statement_colors = NULL, network_labelled = FALSE,
#'   layout_seed = NULL, figure_assets = NULL, preset = NULL,
#'   two_layered = FALSE)
NULL
