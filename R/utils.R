requires_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(pkg, " is required: install.packages('", pkg, "')")
  }
}
