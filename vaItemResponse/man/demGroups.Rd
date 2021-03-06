% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odkItemResponse.R
\name{demGroups}
\alias{demGroups}
\title{Identify which demographic groups are eligible for each item.}
\usage{
demGroups(odk_form)
}
\arguments{
\item{odk_form}{A data frame with the ODK instrument}
}
\value{
A list of vectors with ODK items as rows and columns that
indicate if the item can appear in the interview for the
corresponding demographic group.
}
\description{
It may be of interest to stratify item response results by
demographic groups.  This function, used by \code{itemMissing},
makes this task easier.  The demographic groups include
adults, children, neonates, and mothers.
}
