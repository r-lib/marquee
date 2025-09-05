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
#' ignored. This only holds if `x` is a style set.
#' @param x A style or style set to modify
#' @param tag The name of the tag to modify or remove if `x` is a style set. Tags
#' are internally all lowercase and `tag` will be converted to lowercase before
#' matching
#'
#' @return A `marquee_style_set` object
#'
#' @export
#'
#' @examples
#' # Create a style
#' s_set <- style_set(base = base_style(), p = style(indent = em(2)))
#'
#' # Modify an existing tag
#' modify_style(s_set, "p", size = 16)
#'
#' # Add a new tag, supplying a full style object
#' modify_style(s_set, "str", style(weight = "bold"))
#'
#' # Same as above, but style object created implicitly
#' modify_style(s_set, "str", weight = "bold")
#'
#' # Remove a tag style
#' remove_style(s_set, "p")
#'
style_set <- function(...) {
  styles <- list2(...)
  if (length(styles) == 0) {
    styles <- list()
  } else {
    if (!is_named2(styles)) {
      cli::cli_abort("All arguments must be named")
    }
    names(styles) <- tolower(names(styles))
    for (i in seq_along(styles)) {
      if (!is_style(styles[[i]])) {
        stop_input_type(
          styles[[i]],
          "a marquee style object",
          arg = names(styles)[i]
        )
      }
    }
    if (is.null(styles$base)) {
      cli::cli_abort("The style must contain a base style")
    }
    if (any(vapply(styles$base, is.null, logical(1)))) {
      cli::cli_abort("The base style must be a complete style specification")
    }
    styles <- list(styles)
  }
  vctrs::new_vctr(styles, class = "marquee_style_set")
}

is_style_set <- function(x) inherits(x, "marquee_style_set")

#' @export
format.marquee_style_set <- function(x, ...) {
  vapply(
    vctrs::vec_data(x),
    function(x) {
      paste0("<", paste0(names(x), collapse = ", "), ">")
    },
    character(1)
  )
}

#' @rdname style_set
#' @export
modify_style <- function(x, tag, ...) {
  UseMethod("modify_style")
}
#' @export
modify_style.marquee_style_set <- function(x, tag, ...) {
  tag <- tolower(tag)
  check_character(tag)

  opts <- lapply(list2(...), function(x) {
    if (is_list(x) && !is_bare_list(x)) {
      list(x)
    } else {
      x
    }
  })
  opts <- vctrs::vec_recycle_common(x = x, tag = tag, !!!opts)
  x <- opts$x
  tag <- opts$tag
  opts$x <- NULL
  opts$tag <- NULL

  args <- names(opts)
  expand <- args %in% c("margin", "padding", "border_width")
  if (any(expand)) {
    args <- c(
      args[!expand],
      paste0(
        rep(args[expand], each = 4),
        "_",
        c("top", "right", "bottom", "left")
      )
    )
  }

  for (i in seq_along(x)) {
    if (is_style(opts[[1]][[i]])) {
      if (
        tag[i] == "base" && any(vapply(opts[[1]][[i]], is.null, logical(1)))
      ) {
        cli::cli_abort("The base tag must be set to a complete style")
      }
      x[[i]][[tag[i]]] <- opts[[1]][[i]]
    } else {
      new_style <- inject(style(!!!lapply(opts, `[[`, i)))
      old_style <- x[[i]][[tag[i]]]
      if (is.null(old_style)) {
        x[[i]][[tag[i]]] <- new_style
      } else {
        if (
          tag[i] == "base" && any(vapply(new_style[args], is.null, logical(1)))
        ) {
          cli::cli_abort(
            "The base tag cannot have any styles set to {.val NULL}"
          )
        }
        cls <- class(x[[i]][[tag[i]]])
        class(x[[i]][[tag[i]]]) <- NULL
        x[[i]][[tag[i]]][args] <- new_style[args]
        class(x[[i]][[tag[i]]]) <- cls
      }
    }
  }

  x
}

#' @export
modify_style.marquee_style <- function(x, tag, ...) {
  opts <- list2(...)
  args <- names(opts)
  expand <- args %in% c("margin", "padding", "border_width")
  if (any(expand)) {
    args <- c(
      args[!expand],
      paste0(
        rep(args[expand], each = 4),
        "_",
        c("top", "right", "bottom", "left")
      )
    )
  }
  new_style <- style(...)
  cls <- class(x)
  class(x) <- NULL
  x[args] <- new_style[args]
  class(x) <- cls
  x
}

#' @rdname style_set
#' @export
remove_style <- function(x, tag) {
  tag <- tolower(tag)
  if (!is_style_set(x)) {
    stop_input_type(x, "a style set object")
  }
  check_character(tag)
  if (any(tag == "base")) {
    cli::cli_abort("The base style cannot be removed")
  }
  tag <- vctrs::vec_recycle(tag, length(x))
  for (i in seq_along(x)) {
    x[[i]][tag[i]] <- NULL
  }
  x
}
