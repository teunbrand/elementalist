#' @name elementalist_extensions
#'
#' @title elementalist extensions to ggplot2
#'
#' @description While the bulk of elementalist's utility comes from theme
#'   related functions, part of the extensions protrude into the domain of
#'   geoms. Elementalist relies on the extension mechanism of ggplot2 through
#'   ggproto class objects, which allows cross-package inheritance of objects,
#'   including geoms. These objects can generally be ignored by users for the
#'   purpose of making plots, since interacting with these objects is preffered
#'   through the various \code{geom_*()} functions.
#'
#' @seealso \link[ggplot2]{ggproto}
NULL
