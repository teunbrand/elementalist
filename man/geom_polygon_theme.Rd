% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_polygon_theme.R
\name{geom_polygon_theme}
\alias{geom_polygon_theme}
\title{Thematic polygons}
\usage{
geom_polygon_theme(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  rule = "evenodd",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  element = NULL
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{rule}{Either \code{"evenodd"} or \code{"winding"}. If polygons with holes are
being drawn (using the \code{subgroup} aesthetic) this argument defines how the
hole coordinates are interpreted. See the examples in \code{\link[grid:grid.path]{grid::pathGrob()}} for
an explanation.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{element}{An \code{element_polygon} object, typically constructed with
\code{element_polygon*} functions. Will inherit from the
\code{elementalist.geom_polygon} theme element. When \code{NULL} this theme
element is instead taken from the plot theme.}
}
\value{
A \code{LayerInstance} object that can be added to a plot.
}
\description{
This geom closely follows \code{geom_polygon}, but take defaults from the
theme and are drawn through theme elements. They use the
\code{elementalist.geom_polygon} theme element.
}
\section{Aesthetics}{

\code{geom_polygon_theme()} understands the following aesthetics (required aesthetics are in bold):
\itemize{
  \item \strong{\code{x}}
  \item \strong{\code{y}}
  \item \code{alpha}
  \item \code{colour}
  \item \code{fill}
  \item \code{group}
  \item \code{linetype}
  \item \code{size}
  \item \code{subgroup}
}
Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}

\examples{
df <- data.frame(
  x = c(0, 0.5, 1, 0.5, 0.25, 0.5, 0.75, 0.5),
  y = c(0.5, 0, 0.5, 1, 0.5, 0.25, 0.5, 0.75),
  sub_id = rep(c(1, 2), each = 4),
  id = rep(1, each = 8)
)

ggplot(df, aes(x, y, group = id, subgroup = sub_id)) +
  geom_polygon_theme() +
  theme(elementalist.geom_polygon = element_polygon_glow(colour = "blue"))
}
