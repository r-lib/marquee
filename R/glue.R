#' Marquee-aware string interpolation
#'
#' If you want to create your markdown programmatically you'd probably want to
#' use some sort of string interpolation such as `glue()`. However, the custom
#' span syntax of marquee interferes with the standard interpolation syntax of
#' glue. This function let's you use both together.
#'
#' @inheritParams glue::glue_data
#'
#' @return A character vector
#'
#' @details
#' If you choose a different set of delimiters than `"{"` and `"}"` for the
#' interpolation the functions will call the equivalent glue functions directly.
#' However, if you keep the defaults, the functions will use a custom
#' transformer that will make sure to keep the marquee custom span notation. You
#' can both interpolate the content of the span, as well as the span class (see
#' examples)
#'
#' @export
#'
#' @examples
#' # standard use
#' red_text <- "this text will be red"
#' marquee_glue("This will be black and {.red {red_text}}!")
#'
#' # if the span is not valid it will be treated as standard glue interpolation
#' try(
#'   marquee_glue("This will be black and {.red}!")
#' )
#'
#' # You can interpolate the tag name as well
#' col <- "green"
#' marquee_glue("This will be black and {.{col} this text will be {col}}!")
#'
#' # Tag name interpolation must follow a `.` or a `#` as these identify the
#' # bracket pair as a custom span class
#' col <- ".yellow"
#' # This is not what you want probably
#' marquee_glue("This will be black and {{col} this text will be {col}}!")
#'
#' # Tag interpolation should also interpolate the full tag and be followed by
#' # a space in order to be valid
#' part <- "l"
#' marquee_glue("This will be black and {.ye{part}low this text will be {col}}!")
#' try(
#'   marquee_glue("This will be black and {.{part}avender this text will be {col}}!")
#' )
#'
marquee_glue <- function(
  ...,
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = "NA",
  .null = character(),
  .comment = character(),
  .literal = FALSE,
  .transformer = NULL,
  .trim = TRUE
) {
  if (.open != "{" && .close != "}") {
    .transformer <- .transformer %||% glue::identity_transformer
  } else {
    if (!is.null(.transformer)) {
      cli::cli_warn(
        "Ignoring supplied {.arg .transformer} when using {.val {'{'}} and  {.val {'}'}} as delimiters"
      )
    }
    .transformer <- marquee_transformer(.na, .null, .comment, .literal)
  }

  glue::glue(
    ...,
    .sep = .sep,
    .envir = .envir,
    .open = .open,
    .close = .close,
    .na = .na,
    .null = .null,
    .comment = .comment,
    .literal = .literal,
    .transformer = .transformer,
    .trim = .trim
  )
}
#' @rdname marquee_glue
#' @export
marquee_glue_data <- function(
  .x,
  ...,
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = "NA",
  .null = character(),
  .comment = character(),
  .literal = FALSE,
  .transformer = NULL,
  .trim = TRUE
) {
  if (.open != "{" && .close != "}") {
    .transformer <- .transformer %||% glue::identity_transformer
  } else {
    if (!is.null(.transformer)) {
      cli::cli_warn(
        "Ignoring supplied {.arg .transformer} when using {.val {'{'}} and  {.val {'}'}} as delimiters"
      )
    }
    .transformer <- marquee_transformer(.na, .null, .comment, .literal)
  }

  glue::glue_data(
    .x,
    ...,
    .sep = .sep,
    .envir = .envir,
    .open = .open,
    .close = .close,
    .na = .na,
    .null = .null,
    .comment = .comment,
    .literal = .literal,
    .transformer = .transformer,
    .trim = .trim
  )
}

marquee_transformer <- function(
  .na = "NA",
  .null = character(),
  .comment = character(),
  .literal = FALSE
) {
  function(text, envir) {
    first <- substr(text, 1, 1)
    if (first %in% c(".", "#")) {
      if (substr(text, 2, 2) == "{") {
        tag_ends <- regexec("}", text, fixed = TRUE)[[1]]
        if (substr(text, tag_ends + 1L, tag_ends + 1L) != " ") {
          cli::cli_abort(
            "Malformed marquee interpolation block. When interpolating a tag name it must be followed by a space"
          )
        }
        tag <- glue::identity_transformer(substr(text, 2, tag_ends), envir)
        if (grepl("\\s", tag)) {
          cli::cli_abort("evaluated tag ({.val {tag}}) is not a valid tag name")
        }
      } else {
        tag_ends <- regexec("\\s", text)[[1]]
        if (tag_ends == -1) return(glue::identity_transformer(text, envir))
        tag_ends <- tag_ends - 1L
        tag <- substr(text, 2, tag_ends)
      }
      body <- glue::glue(
        substr(text, tag_ends + 2L, nchar(text)),
        .envir = envir,
        .na = .na,
        .null = .null,
        .comment = .comment,
        .literal = .literal,
        .transformer = caller_fn(n = 0)
      )
      paste0("{", first, tag, " ", body, "}")
    } else {
      glue::identity_transformer(text, envir)
    }
  }
}
