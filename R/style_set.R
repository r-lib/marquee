#' Create or modify a style set that describes a full markdown text
#'
#' A style set contains information on how to style the various tags in a
#' markdown text. While it is not necessary to provide a style for all tags (it
#' will just inherit the parent if missing), it is required to provide a
#' complete style for the body tag so an option is avialable through inheritance
#' for all tags and all style options. It can often be easier to derive a new
#' style set from an existing one rather than building one from scratch.
#'
#' @param ... Named arguments providing a style for the specific tags. For
#' `modify_style()` a number of style options to change. If the first argument
#' is a marquee style it will overwrite the tag and subsequent arguments are
#' ignored
#' @param style_set A style set to modify
#' @param tag The name of a tag to modify or remove. Tags are internally all
#' lowercase and `tag` will be converted to lowercase before matching
#'
#' @return A style set object
#'
#' @export
#'
#' @examples
#' style_set(body = body_style(), p = style(indent = em(2))) |>
#'   modify_style("p", size = 16) |>
#'   modify_style("str", style(weight = "bold")) |>
#'   remove_style("p")
#'
style_set <- function(...) {
  styles <- list2(...)
  if (!is_named2(styles)) {
    cli::cli_abort("All arguments must be named")
  }
  for (i in seq_along(styles)) {
    if (!is_style(styles[[i]])) {
      stop_input_type(styles[[i]], "a marquee style object", arg = names(styles)[i])
    }
  }
  if (is.null(styles$body)) {
    cli::cli_abort("The style must contain a style for the body tag")
  }
  if (any(vapply(styles$body, is.null, logical(1)))) {
    cli::cli_abort("The body style must be a complete style specification")
  }
  names(styles) <- tolower(names(styles))
  class(styles) <- "marquee_style_set"
  styles
}

is_style_set <- function(x) inherits(x, "marquee_style_set")

#' @export
format.marquee_style_set <- function(x, ...) {
  tags <- names(x)
  options <- format(tags, width = max(nchar(tags)), justify = "right")
  paste0(options, ": ", vapply(x, function(opt) {
    paste(trimws(format(opt, ...)), collapse = ", ")
  }, character(1)))
}

#' @export
print.marquee_style_set <- function(x, ...) {
  cat("A marquee style set\n")
  cat(format(x, ...), sep = "\n")
  invisible(NULL)
}

#' @rdname style_set
#' @export
modify_style <- function(style_set, tag, ...) {
  tag <- tolower(tag)
  if (!is_style_set(style_set)) {
    stop_input_type(style_set, "a style set object")
  }
  check_string(tag)
  if (is_style(..1)) {
    style_set[[tag]] <- ..1
    return(style_set)
  }
  args <- names(list2(...))
  expand <- args %in% c("margin", "padding", "border_size")
  if (any(expand)) {
    args <- c(args[!expand], paste0(rep(args[expand], each = 4), "_", c("top", "right", "bottom", "left")))
  }
  new_style <- style(...)
  old_style <- style_set[[tag]]
  if (is.null(style)) {
    style_set[[tag]] <- new_style
  } else {
    style_set[[tag]][args] <- new_style[args]
  }
  style_set
}

#' @rdname style_set
#' @export
remove_style <- function(style_set, tag) {
  tag <- tolower(tag)
  if (!is_style_set(style_set)) {
    stop_input_type(style_set, "a style set object")
  }
  check_string(tag)
  if (tag == "body") {
    cli::cli_abort("The body style cannot be removed")
  }
  style_set[tag] <- NULL
  style_set
}
