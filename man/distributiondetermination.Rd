\name{distributiondetermination}
\alias{distributiondetermination}
\title{Determine the required Q-sort ranking distribution}
\usage{
distributiondetermination(nstat)
}
\arguments{
\item{nstat}{Number of statements. Must be one whole number of at least 3.}
}
\value{
A list containing the number of statements per ranking value (\code{distr}),
the ranking values (\code{values}), and the complete ranking gradient
(\code{ranking}).
}
\description{
Returns the fixed ranking distribution used by the Q approach for a given
number of statements. This distribution provides the basis for preparing
data-collection materials and validating completed rankings.
}
