# qapproach 0.1.2

* `qapproach()` now uses `distribution_repair_seed = 42L` by default so that
  its specialized automatic distribution-repair step is reproducible. The
  seed remains locally scoped and does not modify the caller's random-number
  state. Set it to `NULL` to use the current random-number state.
* In `prepare_rankings()`, the `idcolumn` argument was renamed to `id_column`
  for consistency with `statement_columns`.
* Console notifications are simplified. In `qapproach()` and `validate()`,
  recognized conditions, opposition details, invalid iterations, and
  discard reasons remain available in their centralized diagnostics. Messages
  are retained when the requested consensus or factor count is adjusted, an
  automatic distribution repair is performed, or `qindtest` requires the
  orthogonal Procrustes fallback. Unclassified warnings now include the package
  bug-report URL. `validation_means()` likewise no longer prints its general 
  interpretation paragraphs; this guidance is now provided in `?validation_means`.
* `validate()` now always calculates and stores bottom-rank probabilities and
  uses them for the cp-score validation assessments. In `validation_cps()`, the
  new `include_bottom` argument controls only whether the corresponding
  `P bottom 1`, `P bottom 3`, and `P bottom 5` columns are returned, printed,
  and exported, so this display option does not change a statement's assessment
  category.
* The new experimental `agreement_across_levels()` function traces every
  original input ranking, including rankings introduced above the first
  supplied level, through any number of nested analytical levels. It reports
  each level's analysis, agreement status, and perspective, together with a
  compact positive-agreement path, underlying agreement counts by perspective,
  a terminal-path summary, identifiers and statement-ranking values for
  rankings that never agree, and an internal completeness check.
* The new `plot_sdg_cps()` function compares cp-scores across analyses by
  positioning and scaling the 17 official SDG icons on analysis-specific rows.
  Its optional side-by-side style uses an overlap-free priority-ordered grid,
  and horizontal analysis guides can be requested explicitly.
  It draws on the active graphics device by default and writes a PDF only when
  an explicit `file` path is supplied.
* The new `plot_sdg_diamonds()` function exports one TIFF per group
  perspective, arranging the 17 bundled SDG icons in the perspective's
  diamond-shaped Q-sort distribution from lower to higher priority.
* The new `plot_hierarchical_levels()` function visualizes an arbitrary number
  of bottom-up Q approach levels. Individual rankings feed into first- or
  later-level analyses, and separate group-perspective arrows connect the
  subsequent analyses. Node colors and sizes and the two input-arrow colors
  are configurable. An optional agreement mode distinguishes agreement,
  opposition, and undecided inputs through grey, firebrick, and dodger-blue
  arrows. Individual rankings are grouped by their earliest target 
  analysis, analysis nodes use equal horizontal gaps, and optional
  `level_gaps` add user-defined vertical separation after selected levels.
  `ranking_labelled` and `analysis_labelled` control the two node-label types
  independently. Instead of automated plotting, with `network_object = TRUE`, 
  advanced users may retrieve the prepared `igraph` object without plotting 
  and create fully custom network layouts and graphics.

# qapproach 0.1.1

* `validate()`, `qaboots()`, and `bootstrap_consensus_priority_scores()` now
  use `seed = NULL` by default, and `qapproach()` uses
  `distribution_repair_seed = NULL`. Users can supply an integer such as
  `42L` for reproducible validation, bootstrap, and distribution-repair runs.
  Network functions now provide `layout_seed = NULL` for optionally
  reproducible node placement. The general statement palette retains a locally
  scoped deterministic seed because `hues::iwanthue()` uses stochastic palette
  generation to create distinguishable colors. All internal and supplied seeds
  are scoped with `withr`, preserving the caller's random-number state without
  directly modifying `.GlobalEnv`.
* File-output arguments and paths are standardized without implicit writes:
  - In `qapproach()`, `create_screeplot` and `figures_path` changed to the single
    `screeplot_file = NULL` argument. A scree plot is written only when an
    explicit PDF path is supplied.
  - In `write_figure_collection()`, `filename` changed to the required `file`.
  - In `plot_network_two_layered()`, `filename` changed to `file = NULL`.
  - In `plot_barplot()`, `plot_heatmap()`, `plot_jitterplot()`, `plot_network()`,
    and `plot_spiderweb()`, `filename` changed to `file = NULL`.
  - In `validation_cps()`, `validation_perspectives()`, and
    `validation_means()`, `filename` changed to `file = NULL`.
  - In `summary()`, `write_csv` changed to `file = NULL`.
  - Every supplied relative or absolute path is now used exactly as given. The
    package no longer creates or assumes an `outputs` directory.
* Graphics-state handling now restores all temporary `par()` changes on
  function exit. Scree-plot generation in `qapproach()` is isolated in an 
  internal writer so graphics devices and graphical parameters are restored 
  safely even when plotting fails.
* The experimental `consensus_across_levels()` output now reports raw input
  rankings and level-specific input rankings, resulting perspectives, direct
  agreement, underlying pool agreement, and propagated underlying individual
  agreement. Dataset pool counts and underlying individual counts by
  perspective are retained as structured list-columns for every transition.

# qapproach 0.1.0

* Initial package release with data preparation, analysis, validation, and
  visualization functions.
* The initial package release corresponds to the qapproach functions v2.
  Major changes in comparison to v1 are:
  - `qapproach()` now automatically optimizes the number of group perspectives
  to analyse. Even when the target of 80% consensus cannot be achieved, it still
  identifies the factor solution with the highest achievable statistical consensus.
  - The group perspectives are now treated more precisely. The functionality now
  differentiates between statistical agreement, statistical opposition, and
  statistically undecided rankings. Only positively agreeing rankings contribute
  to the degree of consensus. This affects both the analysis and validation.
  - The calculation of the consensus priority scores now uses a fixed standard-normal 
  cumulative-probability scale instead of min-max normalization. This ensures
  comparability of the consensus priority scores across analyses.
  - The validation framework has been completely revised. Equivalence testing for
  the consensus priority scores has been removed. The revised approach focuses on
  bootstrapping the Q approach results and assessing the stability of both the
  resulting group perspectives and the consensus priority scores. The comparison
  between consensus priority scores and input-ranking means remains available and
  now includes more detailed sensitivity statistics.
  - Several figures useful for interpreting the results have been generalized.
  These include a heatmap of the group perspectives (the diamond-style figure used
  in v1 works well for the SDGs but is less suitable for other statement sets),
  a barplot of the consensus priority scores, a network figure showing how rankings
  feed into group perspectives while distinguishing agreement, opposition, and
  undecided rankings, a spiderweb figure of group-perspective z-scores, and a
  boxplot of bootstrap results from the validation.
  - New convenience and reporting functions (`summary()`, `validation_perspectives()`,
  `validation_cps()`, `validation_means()`, and `consensus_across_levels()`) make
  it easier to inspect, summarize, and compare Q approach results and their
  validation outcomes.
