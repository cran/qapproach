\name{consensus_across_levels}
\alias{consensus_across_levels}
\title{Summarize consensus across successive Q approach levels}
\description{\strong{Experimental.} This function is provided for testing and
further refinement. Its interface and calculations may change, and it should
not yet be used for definitive analytical conclusions.

The transition table reports direct agreement at every level and traces the
original individual rankings through successive positive-agreement links.}
\usage{
consensus_across_levels(levels, print_table = TRUE)
}
\arguments{
  \item{levels}{A named list of levels. Each element is a Q approach result or
  a list of Q approach results representing the datasets at that level.}
  \item{print_table}{Logical; print a compact transition table when \code{TRUE}.}
}
\value{A list containing the number of raw input rankings, a transition table,
and convenient final-transition values for underlying pool agreement, pool
counts, underlying individual agreement, and underlying individual counts by
perspective. The transition table stores the pool counts and perspective-level
counts as structured list-columns.}
