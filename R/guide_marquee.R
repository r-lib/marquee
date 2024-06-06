guide_marquee <- function(title = ggplot2::waiver(),
                          # Note: prefixing namespace prevents recursive default argument
                          style = marquee::style(background = NA),
                          detect = FALSE, theme =  NULL, position = "top",
                          override.aes = list(), order = 1) {
  check_installed("ggplot2", version = "3.5.0")
  if (!(ggplot2::is.theme(theme) || is.null(theme))) {
    stop_input_type(theme, "a <theme>")
  }
  if (!is.null(position)) {
    arg_match0(position, c("top", "right", "bottom", "left"))
  }
  if (!(is_style(style) || is_style_set(style))) {
    stop_input_type(style, "a <marquee_style> or <marquee_style_set>")
  }
  if (!is_bare_list(override.aes)) {
    stop_input_type(override.aes, "a bare <list>")
  }
  ggplot2::new_guide(
    title = title,
    available_aes = "any",
    order = order,
    detect = detect,
    position = position,
    style = style,
    theme = theme,
    override.aes = override.aes,
    super = GuideMarquee
  )
}

on_load(
  makeActiveBinding("GuideMarquee", function() guide_env$guide, environment(guide_marquee))
)

guide_env <- new_environment(list(guide = NULL))

make_marquee_guide <- function() {
  if ("GuideLegend" %in% getNamespaceExports("ggplot2")) {
    # We don't use guide classes directly as earlier ggplot2 versions might
    # be loaded where they don't exist.
    parent <- utils::getFromNamespace("GuideLegend", "ggplot2")
    base   <- utils::getFromNamespace("Guide", "ggplot2")
  } else {
    return(NULL)
  }

  guide_env$guide <- ggplot2::ggproto(
    "GuideMarquee", parent,

    params = list(
      title = ggplot2::waiver(), theme = NULL, name = "guide_marquee",
      position = NULL, direction = NULL, order = 0, hash = character(),
      style = style(), detect = FALSE, override.aes = list()
    ),

    elements = list(
      title   = "plot.subtitle",
      spacing = "legend.box.spacing",
      key     = "legend.key"
    ),

    draw = function(self, theme, position = NULL, direction = NULL,
                    params = self$params) {

      params$position <- params$position %||% position

      elems <- base$setup_elements(params, self$elements, theme)
      # Enforce the title to be a marquee element
      if (!inherits(elems$title, "element_marquee")) {
        elems$title <- ggplot2::merge_element(element_marquee(), elems$title)
      }
      # We offset the margin to counteract the legend.box.spacing so that
      # it resembles a regular subtitle better
      i <- match(params$position, c("bottom", "left", "top", "right"))
      elems$title$margin[i] <- elems$title$margin[i] - elems$spacing
      elems$key <- ggplot2::element_grob(elems$key)

      text <- params$title
      labs <- params$key$.label
      check_string(text, arg = "title")
      check_character(labs, arg = "labels")

      # Place image and label tags
      glyphs <- group_glyphs(self, params, elems)
      text   <- weave_glyphs(text, glyphs, labs)
      text   <- replace_tags(text, labs, params$detect)

      # Set style colour
      style <- elems$title$style %||% classic_style()
      style <- recolour_style(style, text, params)

      if (params$position %in% c("top", "bottom")) {
        width <- unit(1, "npc")
      } else {
        width <- ggplot2::calc_element("legend.key.width", theme) * 5
      }

      # TODO: this is a hack until #24 is solved
      f <- element_grob.element_marquee
      fn_env(f) <- list2env(glyphs)
      for (i in names(glyphs)) {
        environment(f)[[i]] <- glyphs[[i]]
      }
      grob <- f(elems$title, label = text, width = width, margin_y = TRUE, style = style)

      gt <- gtable::gtable(widths = width, heights = grobHeight(grob))
      gtable::gtable_add_grob(
        gt, grob, t = 1, l = 1,
        clip = "off", name = params$name
      )
    }
  )
}

on_load(on_package_load("ggplot2", {
  make_marquee_guide()
}))

group_glyphs <- function(self, params, elems) {

  n_layers <- length(params$decor) + 1
  n_breaks <- params$n_breaks <- nrow(params$key)
  size <- convertUnit(unit(elems$title$size, "pt"), "cm", valueOnly = TRUE)

  glyphs <- self$build_decor(params$decor, list(), elems, params)
  glyphs <- split(glyphs, rep(seq_len(n_breaks), each = n_layers))

  # Combine glyphs coming from multiple layers and respect their alotted size
  glyphs <- lapply(glyphs, function(key) {

    width <- lapply(key, attr, which = "width")
    width[lengths(width) != 1] <- 0
    width <- max(unlist(width))

    height <- lapply(key, attr, which = "height")
    height[lengths(height) != 1] <- 0
    height <- max(unlist(height))

    vp <- NULL
    if (width != 0 || height != 0) {
      vp <- viewport(
        width  = unit(max(width,  size), "cm"),
        height = unit(max(height, size), "cm")
      )
    }
    inject(grobTree(!!!key, vp = vp))
  })

  names(glyphs) <- paste0("GLYPH_", params$key$.label)
  glyphs
}

weave_glyphs <- function(text, glyphs, labels) {

  img_tag <- paste0("![](", names(glyphs), ")")
  n <- rev(seq_along(glyphs))
  # TODO: figure out what to do when `any(n %in% labels)`

  # Replace "![](1)" and "![](label)" with glyph images

  if (grepl(x = text, "![](", fixed = TRUE)) {
    num <- paste0("![](", seq_along(glyphs), ")")
    lab <- paste0("![](", labels, ")")
    for (i in n) {
      text <- gsub(x = text, num[i], img_tag[i], fixed = TRUE)
      text <- gsub(x = text, lab[i], img_tag[i], fixed = TRUE)
    }
  }

  if (grepl(x = text, "<<.*>>")) {
    num <- paste0("<<", seq_along(glyphs), ">>")
    lab <- paste0("<<", labels, ">>")
    for (i in n) {
      text <- gsub(x = text, num[i], img_tag[i], fixed = TRUE)
      text <- gsub(x = text, lab[i], img_tag[i], fixed = TRUE)
    }
  }

  text
}

replace_tags <- function(text, labels, detect) {

  n <- rev(seq_along(labels))
  tags <- vctrs::vec_as_names(labels, repair = "universal", quiet = TRUE)

  # Replace "!!1" and "!!labels" with "{.label label}
  relabel <- paste0("{.", tags, " ", labels, "}")
  if (grepl(x = text, "!!", fixed = TRUE)) {
    num <- paste0("!!", seq_along(labels))
    lab <- paste0("!!", labels)
    for (i in n) {
      text <- gsub(x = text, num[i], relabel[i], fixed = TRUE)
      text <- gsub(x = text, lab[i], relabel[i], fixed = TRUE)
    }
  }

  # Replace `"{.1 xxx}` pattern with `"{.label xxx}"` pattern
  retag <- paste0("{.", tags, " ")
  num <- paste0("{.", seq_along(labels), " ")
  lab <- paste0("{.", labels, " ")
  for (i in n) {
    text <- gsub(x = text, num[i], retag[i], fixed = TRUE)
    text <- gsub(x = text, lab[i], retag[i], fixed = TRUE)
  }

  if (isTRUE(detect)) {
    # TODO: this is really naive and might match glyphs and tags
    labels <- regescape(labels)
    for (i in n) {
      text <- gsub(x = text, labels[i], relabel[i])
    }
  }

  text
}

# Based on stringr::str_escape
regescape <- function(x) {
  gsub(x = x, "([.^$\\\\|*+?{}\\[\\]()])", "\\\\\\1", perl = TRUE)
}

recolour_style <- function(style, text, params) {

  key <- params$key
  label <- params$style %||% style()
  if (is_style_set(label)) {
    style <- label
    label <- style(background = NA)
  }

  # Initialise label style
  if (!"label" %in% names(style[[1]])) {
    style <- modify_style(style, "label", label)
  } else {
    label <- style[[1]]$label
  }

  tags <- vctrs::vec_as_names(key$.label, repair = "universal", quiet = TRUE)
  for (tag in setdiff(tags, names(style[[1]]))) {
    style <- modify_style(style, tag, label)
  }

  # Find out if label style allows for recolouring, early exit if it doesn't
  if (!any(c("colour", "fill") %in% names(key))) {
    return(style)
  }

  # Find out which keys are represented in text
  idx <- which(vapply(
    paste0("{.", tags, " "),
    grepl, x = text[1], fixed = TRUE,
    FUN.VALUE = logical(1)
  ))

  if (length(idx) == 0) {
    return(style)
  }

  # Populate re-coloured parameters
  key_color <- key$colour %||% key$fill
  n <- nrow(key)

  fields <- c("color", "border", "background")
  for (i in idx) {
    # Set default fields to key color
    args <- style[[1]][[tags[i]]][fields]
    nms  <- setdiff(fields, names(args)[lengths(args) > 0])
    args[nms] <- key_color[i]
    # Recolour label's style
    style <- modify_style(style, tag = tags[i], !!!args)
  }
  style
}
