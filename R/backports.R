version_unavailable <- function(min_version) {
  function(...) {
    fun <- as_label(current_call()[[1]])
    cli::cli_abort("This graphics feature is not available before R v{min_version}")
  }
}

glyphFont <- version_unavailable("4.3.0")
glyphFontList <- version_unavailable("4.3.0")
glyphInfo <- version_unavailable("4.3.0")
glyphAnchor <- version_unavailable("4.3.0")
glyphGrob <- version_unavailable("4.3.0")

on_load({
  # Replace version unavailable functions if found
  if ("glyphFont" %in% getNamespaceExports("grDevices")) {
    glyphFont <- grDevices::glyphFont
  }
  if ("glyphFontList" %in% getNamespaceExports("grDevices")) {
    glyphFontList <- grDevices::glyphFontList
  }
  if ("glyphInfo" %in% getNamespaceExports("grDevices")) {
    glyphInfo <- grDevices::glyphInfo
  }
  if ("glyphAnchor" %in% getNamespaceExports("grDevices")) {
    glyphAnchor <- grDevices::glyphAnchor
  }
  if ("glyphGrob" %in% getNamespaceExports("grid")) {
    glyphGrob <- grid::glyphGrob
  }
})