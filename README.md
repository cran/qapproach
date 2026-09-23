# qapproach

`qapproach` implements a workflow to support consensus building based on 
Q method. It prepares participant-by-statement rankings, identifies group 
perspectives, computes consensus priority scores (cp-scores), validates results 
by bootstrap resampling, and creates figures.

## Installation

```r
# install.packages("qapproach") # after CRAN publication
# remotes::install_github("jgeschke/qapproach")
```

## Basic workflow

```r
library(qapproach)

raw <- read.csv2("test_data/TCA_strategies.csv")
rankings <- prepare_rankings(raw)
consensusal_priorities <- qapproach(rankings)
consensusal_priorities[["cp-scores"]]
```

cp-scores use a fixed standard-normal cumulative-probability scale. A value of
0.5 represents neutral prioritization across all group perspectives; higher
and lower values represent relatively higher and lower priority. Cross-analysis
comparisons require the same statements and meanings, ranking distribution,
instructions, data preparation, and analytical settings.

For specialist inspection, the barplot can optionally overlay the weighted
z-scores after rescaling them to the observed cp-score range:

```r
plot_barplot(
  consensusal_priorities,
  show_normalized_weighted_z = TRUE,
  normalized_line_color = "black",
  normalized_line_width = 1.5
)
```

See `vignette("qapproach")` for analysis, validation, and visualization
examples.
