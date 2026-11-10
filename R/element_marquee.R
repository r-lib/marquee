#' ggplot2 theme element supporting marquee syntax
#'
#' This theme element is a drop-in replacement for `ggplot2::element_text()`. It
#' works by integrating the various style settings of the element into the base
#' style of the provided style set. If a margin is given, it is set on the body
#' tag with [skip_inherit()]. The default width is `NA` meaning that it will
#' span as long as the given text is, doing no line wrapping. You can set it to
#' any unit to make it fit within a specific width. However, this may not work
#' as expected with rotated text (you may get lucky). Note that you may see
#' small shifts in the visuals when going from `element_text()` to
#' `element_marquee()` as size reporting may differ between the two elements.
#'
#' If a margin is set on the element it replaces any padding on the body tag
#' of the style, so a base-level padding in the style has no effect in this
#' context. Note also that in [classic_style()] the paragraph (`p`) and list
#' item (`li`) tags set their own padding explicitly, so base-level padding
#' only reaches the body (and a few header tags), not individual paragraphs.
#' To control spacing between paragraphs, modify the paragraph style directly,
#' e.g. `modify_style(style, "p", padding = trbl(bottom = em(1)))` (the same
#' applies to margins).
#'
#' @note grid, which marquee, ggplot2, etc are build upon contains a bug that
#' means that the height of a grob is calculated before the grob knows it's
#' width. The result of this is that if the width of an `element_marquee()` is
#' `NULL` (the default), the text may overflow its allocated space with an
#' additional line. Unfortunately there is no great fix for this, other than
#' eyeball the width it has available and pass that to the element (e.g.
#' `element_marquee(width = grid::unit(10, "cm"))`)
#'
#' @param family The font family of the base style
#' @param colour,color The font colour of the base style
#' @param size The font size of the base style
#' @param lineheight The lineheight of the base style
#' @param margin The margin for the body tag. As margins in `element_text()`
#' doesn't rotate along with `angle` we follow this behavior here as well so
#' that the right margin becomes the bottom margin when rotating the text 90
#' degrees and so forth.
#' @param style A style set to base the rendering on
#' @param width The maximum width of the text. See the description for some
#' caveats for this
#' @inheritParams ggplot2::element_text
#'
#' @return An `element_marquee` object that can be used in place of
#' `element_text` in ggplot2 theme specifications
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' p <- ggplot(mtcars) +
#'   geom_point(aes(mpg, disp)) +
#'   labs(title = "A {.red *marquee*} title\n* Look at this bullet list\n\n* great, huh?") +
#'   theme_gray(base_size = 6) +
#'   theme(plot.title = element_marquee())
#'
#' plot(p)
#'
#' ggplot(mtcars) +
#'   geom_histogram(aes(x = mpg)) +
#'   labs(title =
#' "I put a plot in your title so you can plot while you title
#'
#' ![](p)
#'
#' What more could you _possibly_ want?") +
#'   theme(plot.title = element_marquee())
#'
element_marquee <- function(
  family = NULL,
  colour = NULL,
  size = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  color = NULL,
  margin = NULL,
  style = NULL,
  width = NULL,
  inherit.blank = FALSE
) {
  if (!is.null(color)) {
    colour <- color
  }
  n <- max(
    length(family),
    length(colour),
    length(size),
    length(hjust),
    length(vjust),
    length(angle),
    length(lineheight)
  )
  if (n > 1) {
    cli::cli_warn(c(
      "Vectorized input to {.fn element_text} is not officially supported.",
      i = "Results may be unexpected or may change in future versions of ggplot2."
    ))
  }
  if (!is.null(element_env$s7_class)) {
    return(element_env$s7_class(
      family = family,
      colour = colour,
      size = size,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      lineheight = lineheight,
      margin = margin,
      style = style,
      width = width,
      inherit.blank = inherit.blank
    ))
  }
  structure(
    list(
      family = family,
      colour = colour,
      size = size,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      lineheight = lineheight,
      margin = margin,
      style = style,
      width = width,
      inherit.blank = inherit.blank
    ),
    class = c("element_marquee", "element_text", "element")
  )
}

element_env <- new_environment(list(s7_class = NULL))

# From ggplot2 4.0.0 theme elements are S7 classes. For inheritance to work
# correctly (e.g. `title` passing its class on to `plot.title`), element_marquee
# must be a proper S7 subclass of ggplot2::element_text (#97). On older ggplot2
# versions we fall back to the S3 implementation above.
make_element_marquee_s7 <- function() {
  element_env$s7_class <- S7::new_class(
    "element_marquee",
    package = "marquee",
    parent = ggplot2::element_text,
    properties = list(
      style = S7::class_any,
      width = S7::class_any
    ),
    constructor = function(
      family = NULL,
      colour = NULL,
      size = NULL,
      hjust = NULL,
      vjust = NULL,
      angle = NULL,
      lineheight = NULL,
      margin = NULL,
      style = NULL,
      width = NULL,
      inherit.blank = FALSE
    ) {
      if (!ggplot2::is_margin(margin) && !is.null(margin)) {
        margin <- getFromNamespace("as_margin", "ggplot2")(margin)
        cli::cli_warn(
          "The {.arg margin} argument should be constructed using the \\
          {.fn margin} function."
        )
      }
      obj <- S7::new_object(
        S7::S7_object(),
        family = family,
        face = NULL,
        colour = colour,
        size = size,
        hjust = hjust,
        vjust = vjust,
        angle = angle,
        lineheight = lineheight,
        margin = margin,
        debug = NULL,
        inherit.blank = inherit.blank,
        italic = NA_character_,
        fontweight = NA_real_,
        fontwidth = NA_real_,
        style = style,
        width = width
      )
      # ggplot2's own element constructors add the plain S3 aliases to the
      # class vector like this; they are needed for inheritance and S3
      # dispatch (e.g. element_grob) to work
      class(obj) <- union(
        union(
          c(
            "marquee::element_marquee",
            "element_marquee",
            "ggplot2::element_text",
            "element_text"
          ),
          class(obj)
        ),
        "element"
      )
      obj
    }
  )
}

element_grob.element_marquee <- function(
  element,
  label = "",
  x = NULL,
  y = NULL,
  family = NULL,
  colour = NULL,
  size = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL,
  margin_x = FALSE,
  margin_y = FALSE,
  style = NULL,
  width = NULL,
  ...
) {
  if (is.null(label)) {
    return(ggplot2::zeroGrob())
  }
  if (is.expression(label)) {
    element$style <- NULL
    element$width <- NULL
    return(ggplot2::element_grob(
      inject(ggplot2::element_text(
        !!!element
      )),
      label,
      x = x,
      y = y,
      family = family,
      colour = colour,
      size = size,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      lineheight = lineheight,
      margin = margin,
      margin_x = margin_x,
      margin_y = margin_y
    ))
  }
  label <- as.character(label)
  style <- style %||% element$style %||% classic_style()
  style <- modify_style(
    style,
    "base",
    family = family %||% element$family %||% style[[1]]$base$family %||% "",
    color = colour %||% element$colour %||% style[[1]]$base$color %||% "black",
    size = size %||% element$size %||% style[[1]]$base$size %||% 12,
    lineheight = lineheight %||%
      element$lineheight %||%
      style[[1]]$base$lineheight %||%
      1
  )
  margin <- margin %||% element$margin
  angle <- (angle %||% element$angle %||% 0) %% 360

  # Mirror element_text behaviour: hjust also controls the alignment of
  # multi-line text, unless the style sets an explicit alignment
  if (identical(style[[1]]$base$align, "auto")) {
    hjust_val <- hjust %||% element$hjust
    align <- if (is.character(hjust_val)) {
      switch(
        hjust_val[1],
        left = "left",
        center = "center",
        right = "right",
        NULL
      )
    } else if (is.numeric(hjust_val)) {
      if (hjust_val[1] == 0) {
        "left"
      } else if (hjust_val[1] == 0.5) {
        "center"
      } else if (hjust_val[1] == 1) {
        "right"
      }
    }
    if (!is.null(align)) {
      style <- modify_style(style, "base", align = align)
    }
  }

  # Make sure margin doesn't rotate with element as that is how it works for element_text
  new_margin_x <- margin_x
  new_margin_y <- margin_y
  if (angle > 45 && angle <= 135) {
    margin <- margin[c(4, 1, 2, 3)]
    new_margin_x <- margin_y
    new_margin_y <- margin_x
  } else if (angle > 135 && angle <= 225) {
    margin <- margin[c(3, 4, 1, 2)]
  } else if (angle > 225 && angle <= 315) {
    margin <- margin[c(2, 3, 4, 1)]
    new_margin_x <- margin_y
    new_margin_y <- margin_x
  }

  vjust <- vjust %||% element$vjust
  if (all(as.numeric(margin)[c(1, 3)] == 0)) {
    vjust <- ink(vjust)
  }
  if (!is.null(margin)) {
    pad <- skip_inherit(trbl(
      if (new_margin_y) margin[1] else 0,
      if (new_margin_x) margin[2] else 0,
      if (new_margin_y) margin[3] else 0,
      if (new_margin_x) margin[4] else 0
    ))
    style <- modify_style(style, "body", padding = pad)
  }
  just <- rotate_just(
    angle %||% element$angle,
    hjust %||% element$hjust,
    vjust %||% element$vjust
  )
  n <- max(length(x), length(y), 1)
  x <- x %||% rep(just$hjust, n)
  y <- y %||% rep(just$vjust, n)
  width <- width %||% element$width %||% NA

  grob <- marquee_grob(
    label,
    style,
    force_body_margin = TRUE,
    x = x,
    y = y,
    width = width,
    hjust = hjust %||% element$hjust,
    vjust = vjust,
    angle = angle
  )

  if (xor(margin_x, margin_y)) {
    if (margin_x) {
      grob$full_height <- unit(1, "null")
    }
    if (margin_y) grob$full_width <- unit(1, "null")
  }

  grob
}

on_load({
  on_package_load(
    "ggplot2",
    registerS3method(
      "element_grob",
      "element_marquee",
      element_grob.element_marquee,
      asNamespace("ggplot2")
    )
  )
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    if (S7::S7_inherits(ggplot2::element_text)) {
      make_element_marquee_s7()
      merge_element <- ggplot2::merge_element
      # ggplot2's own method tries to fill all empty props of new from old,
      # which fails for the style and width props that a plain element_text
      # doesn't have
      S7::method(
        merge_element,
        list(element_env$s7_class, S7::class_any)
      ) <- function(new, old, ...) {
        idx <- lengths(S7::props(new)) == 0
        idx <- names(idx[idx])
        old_names <- if (S7::S7_inherits(old)) {
          S7::prop_names(old)
        } else {
          names(unclass(old))
        }
        idx <- intersect(idx, old_names)
        if (S7::S7_inherits(old)) {
          S7::props(new)[idx] <- S7::props(old, idx)
        } else {
          S7::props(new)[idx] <- unclass(old)[idx]
        }
        new
      }
      # Support merging legacy S3 element_marquee objects
      S7::method(
        merge_element,
        list(ggplot2::element_text, S7::new_S3_class("element_marquee"))
      ) <- function(new, old) {
        idx <- lengths(S7::props(new)) == 0
        idx <- names(idx[idx])
        S7::props(new)[idx] <- unclass(old)[idx]
        new
      }
    }
  }
})

#' Convert all text elements in a theme to marquee elements
#'
#' While [element_marquee()] should behave similar to `ggplot2::element_text()`
#' when used on plain text (i.e. text without any markdown markup), the reality
#' can be different. This is because the text shaping engine used by marquee
#' ([textshaping::shape_text()]) may differ from the one used by the graphics
#' device (which is responsible for laying out text in `element_text()`).
#' Differences can range from slight differences in letter spacing to using a
#' different font altogether (this is because the font keywords `""`, `"sans"`,
#' `"serif"`, `"mono"`, and `"symbol"` may be mapped to different fonts
#' depending on the shaper). One way to handle this is to provide an explicit
#' font name for the elements, but alternatively you can use this function to
#' convert all text elements in a theme to [element_marquee()]
#'
#' @param theme A (complete) ggplot2 theme
#'
#' @return `theme` with all text elements substituted for marquee elements
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg)) +
#'   ggtitle("How about that") +
#'   marquefy_theme(theme_gray())
#'
marquefy_theme <- function(theme) {
  theme[] <- Map(
    function(elem, name) {
      if (!inherits(elem, "element_text")) {
        return(elem)
      }
      # Only set style when the face requires one, and width on the root text
      # element, so that a later theme() call on a parent element can still
      # propagate these fields to this element
      style <- NULL
      if (!is.null(elem$face)) {
        style <- classic_style()
        if (elem$face %in% c("italic", "bold.italic")) {
          style <- modify_style(style, "base", italic = TRUE)
        }
        if (elem$face %in% c("bold", "bold.italic")) {
          style <- modify_style(style, "base", weight = "bold")
        }
      }
      element_marquee(
        family = elem$family,
        colour = elem$colour,
        size = elem$size,
        hjust = elem$hjust,
        vjust = elem$vjust,
        angle = elem$angle,
        lineheight = elem$lineheight,
        margin = elem$margin,
        style = style,
        width = if (name == "text") NA else NULL,
        inherit.blank = elem$inherit.blank
      )
    },
    theme,
    names(theme)
  )
  theme
}

# Adaption of ggplot2:::rotate_just() to work with additional just keywords
rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  if (is.character(hjust)) {
    hjust <- switch(
      hjust,
      "left" = 0,
      "left-ink" = ink(0),
      "center" = 0.5,
      "center-ink" = ink(0.5),
      "right" = 1,
      "right-ink" = ink(1)
    )
  }
  if (is.character(vjust)) {
    vjust <- switch(
      vjust,
      "bottom" = 0,
      "bottom-ink" = ink(0),
      "last-line" = 0,
      "center" = 0.5,
      "center-ink" = ink(0.5),
      "top" = 1,
      "top-ink" = ink(1),
      "first-line" = 1
    )
  }
  size <- vctrs::vec_size_common(angle, hjust, vjust)
  angle <- vctrs::vec_recycle(angle, size)
  hjust <- vctrs::vec_cast(vctrs::vec_recycle(hjust, size), ink())
  vjust <- vctrs::vec_cast(vctrs::vec_recycle(vjust, size), ink())
  case <- findInterval(angle, c(0, 90, 180, 270, 360))
  hnew <- as.numeric(hjust)
  vnew <- as.numeric(vjust)
  is_case <- which(case == 2)
  hnew[is_case] <- 1 - as.numeric(vjust[is_case])
  vnew[is_case] <- as.numeric(hjust[is_case])
  is_case <- which(case == 3)
  hnew[is_case] <- 1 - as.numeric(hjust[is_case])
  vnew[is_case] <- 1 - as.numeric(vjust[is_case])
  is_case <- which(case == 4)
  hnew[is_case] <- as.numeric(vjust[is_case])
  vnew[is_case] <- 1 - as.numeric(hjust[is_case])
  list(
    hjust = ink(
      hnew,
      ifelse(case == 3, vctrs::vec_data(hjust)$ink, vctrs::vec_data(vjust)$ink)
    ),
    vjust = ink(
      vnew,
      ifelse(case == 3, vctrs::vec_data(vjust)$ink, vctrs::vec_data(hjust)$ink)
    )
  )
}
