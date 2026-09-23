\name{cpscores}
\alias{cpscores}
\title{Calculate consensus priority scores}
\description{Calculates eigenvalue-weighted consensus priority scores from perspective z-scores and eigenvalues on a fixed standard-normal cumulative-probability scale.}
\usage{
cpscores(statementzscores, factoreigenvalues)
}
\arguments{
  \item{statementzscores}{A matrix of perspective z-scores, with statements in columns or rows.}
  \item{factoreigenvalues}{Numeric eigenvalues for the perspectives.}
}
\value{A named numeric vector of consensus priority scores between the theoretical boundaries 0 and 1.}
\details{The function first calculates the eigenvalue-weighted mean z-score for every statement and then applies the fixed standard-normal cumulative distribution function. A score of 0.5 represents neutral prioritization across the group perspectives; values above or below 0.5 represent relatively higher or lower priority. Comparisons across analyses require the same statements and meanings, ranking distribution, instructions, data preparation, and analytical settings.}
