# horrible hacks caused by using dplyr
utils::globalVariables(c("."))

.onLoad <- function(libname, pkgname) {
    # set metadata_prefix in options
    options("metadata_prefix" = "Metadata")
}

.onAttach <- function(libname, pkgname) {
    # set metadata_prefix in options
    options("metadata_prefix" = "Metadata")
}

#' @import platetools
NULL
