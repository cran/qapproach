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
