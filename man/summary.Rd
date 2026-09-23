\name{summary}
\alias{summary}
\title{Print a Q approach analysis summary}
\description{Prints and returns analysis-summary, group-perspective, and consensus-priority-score tables from an object returned by \code{qapproach()}. The analysis summary includes the observed cp-score range in \code{[minimum, maximum]} format with two decimal places.}
\usage{
summary(results, print_table = TRUE, file = NULL)
}
\arguments{
  \item{results}{An object returned by \code{qapproach()}.}
  \item{print_table}{Logical; print the table when \code{TRUE}.}
  \item{file}{Optional CSV file path. The three tables are written below one another in a single padded CSV file, separated by two blank rows. If the \code{.csv} extension is omitted, it is added automatically. The default \code{NULL} writes no file.}
}
\value{Invisibly returns a named list containing the analysis summary, group perspectives, and consensus priority scores tables. Summary cp-scores are rounded to two decimal places; the source values in \code{results} remain unchanged.}
\seealso{\code{\link{qapproach}}}
