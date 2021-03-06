% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guide_axis2.R
\name{guide_axis2}
\alias{guide_axis2}
\title{Axis guides}
\usage{
guide_axis2(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  position = waiver()
)
}
\arguments{
\item{title}{A character string or expression indicating a title of guide.
If \code{NULL}, the title is not shown. By default
(\code{\link[ggplot2:waiver]{waiver()}}), the name of the scale object or the name
specified in \code{\link[ggplot2:labs]{labs()}} is used for the title.}

\item{check.overlap}{silently remove overlapping labels,
(recursively) prioritizing the first, last, and middle labels.}

\item{angle}{Compared to setting the angle in \code{\link[ggplot2:theme]{theme()}} / \code{\link[ggplot2:element]{element_text()}},
this also uses some heuristics to automatically pick the \code{hjust} and \code{vjust} that
you probably want.}

\item{n.dodge}{The number of rows (for vertical axes) or columns (for
horizontal axes) that should be used to render the labels. This is
useful for displaying labels that would otherwise overlap.}

\item{order}{Used to determine the order of the guides (left-to-right,
top-to-bottom), if more than one  guide must be drawn at the same location.}

\item{position}{Where this guide should be drawn: one of top, bottom,
left, or right.}
}
\value{
A \code{list} with the class \code{"guide"}.
}
\description{
This is just an axis guide that produces the same thing as the regular one.
It is just differently parametrised to allow axis ticks to be controlled
by position modifying theme elements.
}
\examples{
NULL
}
