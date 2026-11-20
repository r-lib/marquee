#' Construct a grob rendering one or more markdown texts
#'
#' This is the main function of marquee. It takes a vector of markdown strings,
#' parses them with the provided style, and returns a grob capable of rendering
#' the parsed text into rich text and (possibly) images. See [marquee_parse()]
#' for more information about how markdown is parsed and see details below for
#' further information on how rendering proceeds.
#'
#' @param text Either a character vector or a `marquee_parsed` object as created
#' by [marquee_parse()]
#' @inheritParams marquee_parse
#' @param force_body_margin Should the body margin override margin collapsing
#' calculations. See Details.
#' @param clip Should content that overflows a `width`/`height` constraint be
#' clipped at the constraint box? Either `"off"` (never clip, the default),
#' `"on"` (clip), or `"inherit"` (defer to the clipping state of the drawing
#' context). Note that clipping also cuts intentional overflow such as
#' emphasis marks and hanging punctuation.
#' @param x,y The location of the markdown text in the graphics. If numeric it
#' will be converted to units using `default.units`. The default is the
#' flow-start corner: `x = 0` for horizontal and ttb-ltr text, `x = 1` for ttb
#' text, `y = 1` always.
#' @param width,height The width and height of each markdown text. If numeric
#' they are converted to units using `default.units`. Both accept the same
#' three kinds of values:
#'
#' * `"auto"` (the default): resolve per writing direction. The constraint
#' axis (width for horizontal text, height for vertical text) takes the
#' extent of the parent container; the block axis (height for horizontal,
#' width for vertical) is sized by the content. This makes vertical text
#' work out of the box, wrapping columns at the container height.
#' * `NULL`: the extent of the parent container — a hard constraint.
#' * `NA`: derive the size from the content (for width this avoids soft
#' breaking of lines).
#'
#' Setting both to `NULL` thus constrains the content to the available space
#' (clipping it if `clip = "on"`).
#' @param default.units A string giving the default units to apply to `x`, `y`,
#' and `width`
#' @param hjust The horizontal justification of the markdown with respect to
#' `x`. Can either be a numeric or one of `"left"`, `"left-ink"`, `"center"`,
#' `"center-ink"`, `"right-ink"`, or `"right"`
#' @param vjust The vertical justification of the markdown with respect to
#' `y`. Can either be a numeric or one of `"bottom"`, `"bottom-ink"`,
#' `"last-line"`, `"center"`, `"center-ink"`, `"first-line"`, `"top-ink"`,
#' `"top"`
#' @param angle The angle of rotation (in degrees) around `x` and `y`
#' @param vp An optional viewport to assign to the grob
#' @param name The name for the grob. If `NULL` a unique name will be generated
#'
#' @return A grob of class `marquee`
#'
#' @details
#' # Rendering
#' marquee is first and foremost developed with the new 'glyph' rendering
#' features in 4.3.0 in mind. However, not all graphics devices supports this,
#' and while some might eventually do, it is quite concievable that some never
#' will. Because of this, marquee has a fallback where it will render text as a
#' mix of polygons and rasters (depending on the font in use) if the device
#' doesn't report 'glyphs' capabilities. The upside is that it works (almost)
#' everywhere, but the downside is that the fallback is much slower and with
#' poorer visual quality. Because of this it is advisable to use a modern
#' graphics device with glyphs support if at all possible.
#'
#' # Rendering style
#' The rendering more or less adheres to the styling provided by
#' [marquee_parse()], but has some intricacies as detailed below:
#'
#' **Tight lists**
#'
#' If a list is tight, the bottom margin of each `li` tag will be set so the
#' spacing matches the lineheight. Further, the top margin will be set to 0.
#'
#' **Block images**
#'
#' In markdown, image tags are span elements so they can be placed inline.
#' However, if an image tag is the only thing that is contained inside a p tag
#' marquee determines that it should be considered a block element. In that
#' case, the parent p element inherits the styling from the image element so
#' that the image can e.g. adhere to `align` properties, or provide their own
#' padding.
#'
#' **Horizontal rulers**
#'
#' These elements are rendered as an empty block. The standard style sets a
#' bottom border size and no size for the other sides.
#'
#' **Margin collapsing**
#'
#' Margin calculations follows the margin collapsing rules of HTML. Read more
#' about these at [mdn](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_box_model/Mastering_margin_collapsing).
#' Margin collapsing means that elements with margin set to 0 might end up with
#' a margin. Specifically for the body element this can be a problem if you want
#' to enforce a tight box around your text. Because of this the
#' `force_body_margin` argument allows you to overwrite the margins
#' for the body element with the original values after collapsing has been
#' performed.
#'
#' **Underline and strikethrough**
#'
#' Underlines are placed according to the font specification. Strikethrough are
#' placed 0.3em above the baseline. The width of the line is set according to
#' the fonr specification for underline width, both for underline and
#' strikethrough. It inherits the color of the text.
#'
#' **Spans with background**
#'
#' Consecutive spans with the same background and border settings are merged
#' into a single rectangle. The padding of the span defines the size of the
#' background.
#'
#' **Bullet position**
#'
#' Bullets are placed, right-aligned, 0.25em to the left of the first line in
#' the li element if the text direction is ltr. For rtl text it is placed,
#' left-aligned, 0.25 em to the right of the first line.
#'
#' **Border with border radius**
#'
#' If borders are not the same on all sides they are drawn one by one. In this
#' case the border radius is ignored.
#'
#' # Image rendering
#' The image tag can be used to place images. There are support for both png,
#' jpeg, and svg images. If the path instead names a grob, ggplot, or patchwork
#' object then this is rendered instead. If the file cannot be read, if it d
#' oesn't exist, or if the path names an object that is not a grob, ggplot or
#' patchwork, a placeholder is rendered in it's place (black square with red
#' cross).
#'
#' **Image sizing**
#'
#' There is no standard in markdown for specifying the size of images. By
#' default, block-level images fill the width of it's container and maintain
#' it's aspect ratio. Inline images have a default width of 0.65em and a height
#' matching the aspect ration.
#'
#' However, if you wish to control sizing, you can instead provide the image as
#' a grob with a viewport with fixed dimensions, in which case this will be used
#' as long as the width doesn't exceed the width of the container (in which case
#' it will get downsized). If a rastergrob is provided without absolute sizing,
#' the aspect ratio will match the raster, otherwise the aspect ratio will be
#' taken from the styling of the element (defaults to 1.65)
#'
#' # Table rendering
#' While marquee does not support the extended table syntax for markdown it does
#' allow you to include tables in the output. It does so by supporting gt
#' objects as valid paths in image tags in the same way as ggplots etc. This
#' meeans that you can style your tables any way you wish and with the full
#' power of gt, which is much more flexible than the markdown table syntax.
#'
#' # Math rendering
#' Math expressions written with standard markdown math syntax are rendered
#' using the math shaper in textshaping (`textshaping::shape_math()`). This
#' functionality is only available if the installed version of textshaping
#' provides `shape_math()`; if the text contains math expressions and
#' textshaping is too old, an error is raised informing you of this. Both
#' LaTeX and Typst math syntax are supported (see `textshaping::shape_math()`
#' for the full specification).
#'
#' Inline math (`$expression$`) flows with the surrounding text, while display
#' math (`$$expression$$`) is typeset in display style. A display math
#' expression standing alone in a block behaves like a block-level image: it
#' can be aligned through its style (it is centered by default with
#' [classic_style()]). Math is typeset in the system math font using the size
#' and color of the surrounding text. A different math font can be selected by
#' setting the family of the `eq`/`eqd` styles (e.g.
#' `modify_style(classic_style(), "eq", family = "Latin Modern Math")`); for
#' best results use a font with an OpenType MATH table. Elements within an
#' expression can be colored individually with e.g.
#' `\textcolor{color}{content}`.
#'
#' # Vertical typesetting
#' marquee supports vertical writing modes through the `text_direction` style
#' property: `"ttb"` sets text in top-to-bottom columns that progress
#' right-to-left (as in Japanese and traditional Chinese typesetting), and
#' `"ttb-ltr"` sets the same columns with left-to-right progression (the
#' Mongolian convention). The root writing mode is taken from the base style;
#' any tag can override it, and the resolved value cascades down the block
#' tree. A block whose direction differs from its parent's is laid out in its
#' own transposed coordinate system and positioned as a box in the parent's
#' flow, which makes it possible to mix directions freely — e.g. a tategaki
#' quotation in a horizontal document, or a yokogaki code block inside
#' vertical text.
#'
#' In vertical mode both axes transpose: the constraint axis is the height
#' (columns wrap when they reach it) and the block axis runs horizontally.
#' Margins and padding remain physical ([trbl()]), so in ttb mode margin
#' collapsing happens between the right margin of the left block and the left
#' margin of the right block. Decorations follow the conventions of vertical
#' typesetting: bullets hang at the column head, underlines are drawn as
#' bōsen to the right of the column in ttb (left in ttb-ltr), and emphasis
#' marks (see the `emphasis` argument of [style()]) sit to the right
#' of each character. Horizontal-only runs (Latin script, digits) inside
#' vertical text are handled according to the `mixed` style property, and CJK
#' line breaking follows the `kinsoku`/`kinsoku_style` properties.
#'
#' The [vertical_style()] constructor provides a ready-made style set with
#' traditional defaults (kenten emphasis, kanji list numbering, hanging
#' bullets, horizontal code blocks) for both directions.
#'
#' A few limitations apply: stroked text (the `outline_*` style properties)
#' does not account for glyph rotation, bitmap glyphs (color emoji) are not
#' rotated in the fallback rendering path, and the `"first-line"`/
#' `"last-line"` justification anchors remain y-based for vertical text.
#'
#' # Textbox justification
#' The justification options exceeds the classic ones provided by grid. While
#' numeric values are available as always, the number of possible text values
#' are larger. Horizontal justification add `"left-ink"`, `"center-ink"`, and
#' `"right-ink"` which uses the left-most and right-most positioned glyph (or
#' halfway between them) as anchors. Vertical justification has the equivalent
#' `"bottom-ink"`, `"center-ink"`, and `"top-ink"` anchors, but also
#' `"first-line"` and `"last-line"` which sets the anchor at the baseline of the
#' first or last line respectively.
#'
#' @export
marquee_grob <- function(
  text,
  style = classic_style(),
  ignore_html = TRUE,
  force_body_margin = FALSE,
  clip = "off",
  x = NULL,
  y = 1,
  width = "auto",
  height = "auto",
  default.units = "npc",
  hjust = NULL,
  vjust = "top",
  angle = 0,
  vp = NULL,
  name = NULL
) {
  # Resolve direction-dependent defaults
  if (is.null(hjust) || is.null(x)) {
    ### Default anchor and justification are at the flow-start corner: right
    ### for ttb, left otherwise
    root_dir <- if (is_parsed(text)) {
      text$text_direction[text$indentation == 1]
    } else {
      vapply(
        style,
        function(s) s$base$text_direction %||% "auto",
        character(1),
        USE.NAMES = FALSE
      )
    }
    ttb <- root_dir == "ttb"
    if (is.null(hjust)) {
      hjust <- ifelse(ttb, "right", "left")
    }
    if (is.null(x)) {
      x <- ifelse(ttb, 1, 0)
    }
  }

  # Basic input checking
  if (
    is.character(hjust) &&
      !all(
        hjust %in%
          c("left", "left-ink", "center", "center-ink", "right-ink", "right")
      )
  ) {
    cli::cli_abort(c(
      "{.arg hjust} must be a valid justification",
      i = "Use either numerics or one of {.or {.val {c('left', 'left-ink', 'center', 'center-ink', 'right-ink', 'right')}}}"
    ))
  } else if (!is.numeric(hjust) && !is.character(hjust) && !is_ink(hjust)) {
    cli::cli_abort("{.arg hjust} must either be numeric or a character vector")
  }
  if (
    is.character(vjust) &&
      !all(
        vjust %in%
          c(
            "bottom",
            "bottom-ink",
            "last-line",
            "center",
            "center-ink",
            "first-line",
            "top-ink",
            "top"
          )
      )
  ) {
    cli::cli_abort(c(
      "{.arg vjust} must be a valid justification",
      i = "Use either a numerics or one of {.or {.val {c('bottom', 'bottom-ink', 'last-line', 'center', 'center-ink', 'first-line', 'top-ink', 'top')}}}"
    ))
  } else if (!is.numeric(vjust) && !is.character(vjust) && !is_ink(vjust)) {
    cli::cli_abort("{.arg vjust} must either be numeric or a character vector")
  }
  check_bool(force_body_margin)
  clip <- arg_match0(clip, c("on", "off", "inherit"))

  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }
  if (identical(width, NULL)) {
    width <- unit(1, "npc")
  }
  if (identical(height, NULL)) {
    height <- unit(1, "npc")
  }

  # Parse input
  parsed <- if (!is_parsed(text)) {
    marquee_parse(text, style, ignore_html)
  } else {
    text
  }

  # Add spacing for inline padding
  parsed <- add_inline_padding(parsed)

  # Set flow-end margin for tight list `li` elements to match the lineheight
  is_tight <- parsed$type == "li" & parsed$tight
  parsed$margin_top[is_tight] <- 0
  tight_spacing <- parsed$size[is_tight] * (parsed$lineheight[is_tight] - 1)
  ## In vertical flow the spacing goes on the block-end side (left for ttb,
  ## right for ttb-ltr), otherwise on the bottom
  tight_dir <- parsed$text_direction[is_tight]
  parsed$margin_bottom[is_tight] <- ifelse(
    tight_dir %in% c("ttb", "ttb-ltr"),
    0,
    tight_spacing
  )
  parsed$margin_left[is_tight] <- ifelse(
    tight_dir == "ttb",
    tight_spacing,
    parsed$margin_left[is_tight]
  )
  parsed$margin_right[is_tight] <- ifelse(
    tight_dir == "ttb-ltr",
    tight_spacing,
    parsed$margin_right[is_tight]
  )

  # Handle image tags
  images <- list(
    index = which(parsed$type == "img")
  )
  images$path <- parsed$text[images$index]
  parsed$text[images$index] <- NA
  ## Determine if tag is inline or on it's own
  images$inline <- vapply(
    images$index,
    function(i) {
      block <- parsed$block == parsed$block[i]
      sum(block) != 3 || any(nchar(parsed$text[block]) > 0, na.rm = TRUE)
    },
    logical(1)
  )
  ## If not inline, the prior p tag inherits the image styling so you can center it or add padding
  parsed[images$index[!images$inline] - 1, names(style[[1]][[1]])] <- parsed[
    images$index[!images$inline],
    names(style[[1]][[1]])
  ]
  ## Convert all "paths" to grobs
  env <- caller_env()
  images$grobs <- images_as_grobs(images$path, env)

  # Handle math tags
  maths <- list(index = which(parsed$type %in% c("eq", "eqd")))
  if (length(maths$index) != 0) {
    check_math_support()
    maths$display <- parsed$type[maths$index] == "eqd"
    maths$text <- parsed$text[maths$index]
    parsed$text[maths$index] <- NA
    ## Determine if tag is inline or on it's own
    maths$inline <- vapply(
      maths$index,
      function(i) {
        block <- parsed$block == parsed$block[i]
        sum(block) != 3 || any(nchar(parsed$text[block]) > 0, na.rm = TRUE)
      },
      logical(1)
    )
    ## If not inline, the prior p tag inherits the math styling so you can
    ## center it or add padding
    parsed[maths$index[!maths$inline] - 1, names(style[[1]][[1]])] <- parsed[
      maths$index[!maths$inline],
      names(style[[1]][[1]])
    ]
    ## Resolve the math font. If the style set provides an explicit family for
    ## the eq/eqd tag it takes precedence, otherwise the system math font
    style <- vctrs::vec_recycle(style, max(parsed$id))
    maths$family <- vapply(
      seq_along(maths$index),
      function(i) {
        tag_style <- style[[parsed$id[maths$index[i]]]][[
          if (maths$display[i]) "eqd" else "eq"
        ]]
        tag_style$family %||% "math"
      },
      character(1)
    )
    ## Shape the math expressions up front. Width doesn't affect math layout so
    ## this can be done before the width of the blocks are known
    maths$shape <- shape_math_safe(
      maths$text,
      family = maths$family,
      size = parsed$size[maths$index],
      inline = !maths$display,
      color = parsed$color[maths$index]
    )
    ## Failed expressions revert to literal text
    failed <- !vapply(maths$shape, is.list, logical(1))
    if (any(failed)) {
      parsed$text[maths$index[failed]] <- maths$text[failed]
      maths$index <- maths$index[!failed]
      maths$display <- maths$display[!failed]
      maths$inline <- maths$inline[!failed]
      maths$text <- maths$text[!failed]
      maths$family <- maths$family[!failed]
      maths$shape <- maths$shape[!failed]
    }
  }

  # Determine location of blocks, their indent, direction, and location in
  # overall parsed text
  bl <- rle(parsed$block)$lengths
  bs <- cumsum(c(0, bl[-length(bl)])) + 1
  blocks <- list(
    length = bl,
    start = bs,
    indent = parsed$indentation[bs],
    direction = parsed$text_direction[bs]
  )

  # Resolve "auto" sizing per root direction: parent extent on the constraint
  # axis (width for horizontal, height for vertical), content on the block
  # axis. Explicit NULL means parent extent on both axes, NA content-sized
  root_vertical <- blocks$direction[blocks$indent == 1] %in%
    c("ttb", "ttb-ltr")
  resolve_auto <- function(vert) {
    if (all(vert)) {
      return(list(unit(NA, "npc"), unit(1, "npc")))
    }
    if (!any(vert)) {
      return(list(unit(1, "npc"), unit(NA, "npc")))
    }
    list(
      inject(unit.c(
        !!!lapply(vert, function(v) {
          if (v) unit(NA, "npc") else unit(1, "npc")
        })
      )),
      inject(unit.c(
        !!!lapply(vert, function(v) {
          if (v) unit(1, "npc") else unit(NA, "npc")
        })
      ))
    )
  }
  auto_size <- resolve_auto(root_vertical)
  if (identical(width, "auto")) {
    width <- auto_size[[1]]
  }
  if (identical(height, "auto")) {
    height <- auto_size[[2]]
  }
  if (!is.unit(width)) {
    width <- unit(width, default.units)
  }
  if (!is.unit(height)) {
    height <- unit(height, default.units)
  }

  # Resolve the layout direction and flow axis of each block. "auto" is
  # treated as horizontal for layout purposes (vertical-only scripts shaped as
  # ttb under "auto" are not yet transposed). The flow axis is the block
  # direction of the parent (root blocks flow in their own direction)
  blocks$direction[blocks$direction == "auto"] <- "ltr"
  blocks$vertical <- blocks$direction %in% c("ttb", "ttb-ltr")
  blocks$flow <- blocks$direction
  dir_stack <- character(max(blocks$indent))
  for (i in seq_along(blocks$start)) {
    ind <- blocks$indent[i]
    if (ind > 1) {
      blocks$flow[i] <- dir_stack[ind - 1]
    }
    dir_stack[ind] <- blocks$direction[i]
  }
  blocks$vertical_flow <- blocks$flow %in% c("ttb", "ttb-ltr")

  # Perform margin collapsing. Collapsing happens along the flow axis of the
  # parent: top/bottom for horizontal flow, right/left for ttb, left/right
  # for ttb-ltr
  start_margin <- parsed$margin_top
  end_margin <- parsed$margin_bottom
  has_background <- vapply(
    parsed$background,
    function(x) !(is.character(x) && is.na(x[1])),
    logical(1)
  )
  has_start_border <- !is.na(parsed$border) & parsed$border_width_top != 0
  has_end_border <- !is.na(parsed$border) & parsed$border_width_bottom != 0
  for (i in seq_along(blocks$start)) {
    j <- blocks$start[i]
    sides <- flow_sides(blocks$flow[i])
    start_margin[j] <- parsed[[paste0("margin_", sides[1])]][j]
    end_margin[j] <- parsed[[paste0("margin_", sides[2])]][j]
    has_start_border[j] <- !is.na(parsed$border[j]) &
      parsed[[paste0("border_width_", sides[1])]][j] != 0
    has_end_border[j] <- !is.na(parsed$border[j]) &
      parsed[[paste0("border_width_", sides[2])]][j] != 0
  }
  collapsed_margins <- list(start = start_margin, end = end_margin)
  for (root in blocks$start[blocks$indent == 1]) {
    block_tree <- collect_children(
      root,
      blocks$start,
      parsed$ends,
      parsed$indentation
    )
    collapsed_margins <- set_margins(
      block_tree,
      collapsed_margins,
      has_background,
      has_start_border,
      has_end_border,
      if (force_body_margin) 0 else NULL
    )
  }
  # Body margin wins over any collapsing
  if (force_body_margin) {
    roots <- parsed$indentation == 1
    collapsed_margins$start[roots] <- start_margin[roots]
    collapsed_margins$end[roots] <- end_margin[roots]
  }
  # Map the collapsed start/end margins back to their physical sides
  for (i in seq_along(blocks$start)) {
    j <- blocks$start[i]
    sides <- flow_sides(blocks$flow[i])
    parsed[[paste0("margin_", sides[1])]][j] <- collapsed_margins$start[j]
    parsed[[paste0("margin_", sides[2])]][j] <- collapsed_margins$end[j]
  }

  # Handle bullets
  bullets <- place_bullets(
    parsed$type,
    parsed$indentation,
    parsed$block,
    parsed$text == "",
    parsed$ol_index,
    parsed$bullets
  )
  ## Apply richer ol numbering (templates and script-based numbering)
  bullets <- format_ol_bullets(bullets, parsed)
  ## Bullet shape doesn't depend on width so can be calculated up front
  bullets$shape <- textshaping::shape_text(
    bullets$bullet,
    family = parsed$family[bullets$index],
    italic = parsed$italic[bullets$index],
    weight = parsed$weight[bullets$index],
    width = parsed$width[bullets$index],
    features = parsed$features[bullets$index],
    size = parsed$size[bullets$index],
    res = 600,
    hjust = 1,
    vjust = 1,
    direction = parsed$text_direction[bullets$index],
    # Avoid zero-length args when there are no bullets (match.arg fails)
    mixed = if (length(bullets$index) == 0) {
      "rotate"
    } else {
      parsed$mixed[bullets$index]
    },
    kinsoku = if (length(bullets$index) == 0) {
      "strict"
    } else {
      parsed$kinsoku[bullets$index]
    },
    kinsoku_style = if (length(bullets$index) == 0) {
      "hang"
    } else {
      parsed$kinsoku_style[bullets$index]
    }
  )
  ## Inherit color and id from the relevant block
  bullets$shape$shape$col <- parsed$color[bullets$index[
    bullets$shape$shape$metric_id
  ]]
  bullets$shape$shape$id <- parsed$id[bullets$index[
    bullets$shape$shape$metric_id
  ]]

  ## Reserve first-line indent for list markers so wide markers never
  ## overlap the first line. marker_width: "auto" (uniform reservation of
  ## the widest marker in the list), "natural" (per-item reservation,
  ## ragged first lines), or a fixed length. Wrapped lines use the same
  ## reservation so all text stays aligned
  if (length(bullets$index) != 0) {
    bullet_row <- bullets$placement
    bullet_block <- parsed$block[bullet_row]
    ### The marker's extent along the flow: the sum of its glyph advances
    ### (combined runs like tate-chu-yoko digits count as one cell)
    marker_w <- vapply(
      split(seq_len(nrow(bullets$shape$shape)), bullets$shape$shape$metric_id),
      function(g) sum(bullets$shape$shape$advance[g]),
      numeric(1)
    )[as.character(seq_along(bullet_row))]
    ### Group items by parent list
    list_parent <- integer(length(blocks$start))
    p_stack <- integer(max(blocks$indent))
    for (i in seq_along(blocks$start)) {
      ind <- blocks$indent[i]
      if (ind > 1) {
        list_parent[i] <- p_stack[ind - 1]
      }
      p_stack[ind] <- i
    }
    list_parent <- list_parent[bullet_block]
    for (b in seq_along(bullet_row)) {
      i <- bullet_block[b]
      j <- blocks$start[i]
      mode <- parsed$marker_width[bullet_row[b]]
      if (is.na(mode)) {
        next
      }
      R <- if (mode == "auto") {
        max(marker_w[list_parent == list_parent[b]], na.rm = TRUE)
      } else if (mode == "natural") {
        marker_w[b]
      } else {
        suppressWarnings(as.numeric(mode))
      }
      if (is.finite(R)) {
        parsed$indent[j] <- max(parsed$indent[j], R)
        parsed$hanging[j] <- max(parsed$hanging[j], R)
      }
    }
  }

  grob <- gTree(
    text = parsed,
    blocks = blocks,
    bullets = bullets,
    images = images,
    maths = maths,
    x = rep_along(text, x),
    y = rep_along(text, y),
    width = rep_along(text, width),
    height = rep_along(text, height),
    hjust = rep_along(text, hjust),
    vjust = rep_along(text, vjust),
    clip = rep_along(text, clip),
    angle = rep_along(text, angle),
    vp = vp,
    name = name,
    cl = "marquee_grob"
  )
  # Check if we can do all calculations upfront
  if (
    grDevices::dev.cur() != 1 &&
      all(
        is.na(grob$width) |
          !unitType(absolute.size(grob$width)) %in%
            c("null", "sum", "min", "max", "npc")
      ) &&
      all(
        is.na(grob$height) |
          !unitType(absolute.size(grob$height)) %in%
            c("null", "sum", "min", "max", "npc")
      )
  ) {
    grob <- makeContent.marquee_grob(makeContext.marquee_grob(grob))
    class(grob) <- c("marquee_precalculated_grob", class(grob))
  }
  grob
}

#' @export
makeContext.marquee_grob <- function(x) {
  # Everything is calculated in bigpts (1 inch == 72 bigpts)
  # We inspect the angle, if it is more vertical we use the vertical direction
  # of the viewport for conversion
  width <- ifelse(
    findInterval(x$angle, c(0, c(0, 90, 180, 270) + 45)) %% 2 == 1,
    as_bigpts(x$width),
    as_bigpts(x$width, width = FALSE)
  )
  # Height only constrains vertical text (column length), where it mirrors
  # the handling of width
  height <- ifelse(
    findInterval(x$angle, c(0, c(0, 90, 180, 270) + 45)) %% 2 == 1,
    as_bigpts(x$height, width = FALSE),
    as_bigpts(x$height)
  )

  # Calculate width of each block
  widths <- rep(NA, length(x$blocks$start))
  left_offset <- widths
  indent_stack <- rep(NA, max(x$blocks$indent))

  ## Allocate the column-length constraint down vertical-flow trees, mirroring
  ## the width recursion: each block gets the parent's allocated height minus
  ## its own top/bottom margin and padding
  heights_alloc <- rep(NA_real_, length(x$blocks$start))
  h_stack <- rep(NA_real_, max(x$blocks$indent))
  for (i in seq_along(x$blocks$start)) {
    j <- x$blocks$start[i]
    indent <- x$blocks$indent[i]
    if (indent == 1) {
      heights_alloc[i] <- height[x$text$id[j]] -
        x$text$margin_top[j] -
        x$text$padding_top[j] -
        x$text$padding_bottom[j] -
        x$text$margin_bottom[j]
    } else if (x$blocks$vertical_flow[i]) {
      heights_alloc[i] <- h_stack[indent - 1] -
        x$text$margin_top[j] -
        x$text$padding_top[j] -
        x$text$padding_bottom[j] -
        x$text$margin_bottom[j]
    }
    h_stack[indent] <- heights_alloc[i]
  }

  # Identify blocks in vertical flow (their parent has a vertical direction).
  # The first such block in a chain is a vertical-flow root: it is laid out
  # by the recursive transposed algorithm below together with all its
  # descendants, while the iterative loop treats it as an opaque box (it only
  # needs its physical height to stack subsequent siblings)
  n_blocks <- length(x$blocks$start)
  parent_idx <- integer(n_blocks)
  p_stack <- integer(max(x$blocks$indent))
  for (i in seq_len(n_blocks)) {
    ind <- x$blocks$indent[i]
    if (ind > 1) {
      parent_idx[i] <- p_stack[ind - 1]
    }
    p_stack[ind] <- i
  }
  vflow_root <- x$blocks$vertical_flow &
    (x$blocks$indent == 1 | !x$blocks$vertical_flow[pmax(parent_idx, 1)])
  recursive_tree <- vflow_root
  for (i in seq_len(n_blocks)) {
    if (
      !vflow_root[i] && x$blocks$indent[i] > 1 && recursive_tree[parent_idx[i]]
    ) {
      recursive_tree[i] <- TRUE
    }
  }
  has_vertical_flow <- any(recursive_tree)

  ## Iterate over blocks, calculate offset and width based on parent + margin/padding
  for (i in seq_along(x$blocks$start)) {
    if (x$blocks$vertical_flow[i]) {
      ### Blocks in vertical flow are sized by their content, not the
      ### container. Filled in from the shaping output below
      indent_stack[x$blocks$indent[i]] <- i
      next
    }
    j <- x$blocks$start[i]
    indent <- x$blocks$indent[i]
    if (indent == 1) {
      ### This is the body tag. start fresh
      widths[i] <- width[x$text$id[j]] -
        x$text$margin_left[j] -
        x$text$padding_left[j] -
        x$text$padding_right[j] -
        x$text$margin_right[j]
      left_offset[i] <- x$text$margin_left[j] + x$text$padding_left[j]
    } else {
      ### Find width and offset of parent and base calc on that
      widths[i] <- widths[indent_stack[indent - 1]] -
        x$text$margin_left[j] -
        x$text$padding_left[j] -
        x$text$padding_right[j] -
        x$text$margin_right[j]
      left_offset[i] <- left_offset[indent_stack[indent - 1]] +
        x$text$margin_left[j] +
        x$text$padding_left[j]
    }
    ### Set the index of the current indent to this
    indent_stack[indent] <- i
  }

  # Figure out width and height of images
  size <- x$text$size
  tracking <- x$text$tracking
  images <- x$images
  images$y_adjust <- rep(0, length(images$inline))
  if (sum(images$inline) > 0) {
    # Some fonts are much higher than their point size. We move the image baseline up half the difference
    image_font <- systemfonts::font_info(
      x$text$family[images$index[images$inline]],
      size = size[images$index[images$inline]]
    )
    actual_size <- image_font$max_ascend - image_font$max_descend
    images$y_adjust[images$inline] <- (actual_size -
      size[images$index[images$inline]]) /
      2
  }

  for (i in seq_along(images$index)) {
    vp <- images$grobs[[i]]$vp
    scale <- 1
    ## Start with width. NB: keep these locals away from `width`/`height`,
    ## which hold the grob-level constraints used below
    if (is.null(vp) || unitType(absolute.size(vp$width)) == "null") {
      if (images$inline[i]) {
        ### No absolute size + inline. set width to 1em
        img_w <- x$text$size[images$index[i]]
        # if raster or svg we cap at height instead of width and let aspect ratio determine width
        if (inherits(images$grobs[[i]], "rastergrob")) {
          asp <- ncol(images$grobs[[i]]$raster) / nrow(images$grobs[[i]]$raster)
          img_w <- img_w * asp
        } else if (inherits(images$grobs[[i]], "svg_grob")) {
          asp <- images$grob[[i]]$asp
          img_w <- img_w * asp
        }
      } else {
        ### Not inline. Use block width
        img_w <- widths[x$text$block[images$index[i]]] %|% 0
      }
    } else {
      ### Grob has a width. Use that unless it exceeds the block width
      true_width <- as_bigpts(vp$width)
      img_w <- min(widths[x$text$block[images$index[i]]], true_width)
      scale <- img_w / true_width
    }
    ## Then height
    if (is.null(vp) || unitType(absolute.size(vp$height)) == "null") {
      if (inherits(images$grobs[[i]], "rastergrob")) {
        ### No size and rastergrob. Use aspect ratio of raster
        asp <- ncol(images$grobs[[i]]$raster) / nrow(images$grobs[[i]]$raster)
        img_h <- img_w / asp
      } else if (inherits(images$grobs[[i]], "svg_grob")) {
        asp <- images$grob[[i]]$asp
        img_h <- img_w / asp
      } else if (images$inline[i]) {
        ### Inline. We set a square size
        img_h <- img_w
      } else {
        ### Not inline. We use the aspect ratio from the style
        img_h <- img_w / x$text$img_asp[images$index[i]]
      }
    } else {
      ### Grob has height We use that but scale it based on width downscaling
      img_h <- as_bigpts(vp$height, width = FALSE) * scale
    }
    ## Set dimensions in size and tracking (this is how it is passed to textshaping)
    size[images$index[i]] <- img_h
    tracking[images$index[i]] <- img_w
    ## Create a parent viewport with the correct size. Clipping is inherited
    ## so images are clipped by an outer constraint clip but otherwise
    ## unclipped
    outer_vp <- viewport(
      x = 0,
      y = 0,
      width = unit(img_w, "bigpts"),
      height = unit(img_h, "bigpts"),
      just = c(0, 0),
      clip = "inherit"
    )
    ## Use parent vp directly if no vp, otherwise wrap the grob
    if (is.null(vp)) {
      images$grobs[[i]]$vp <- outer_vp
    } else {
      images$grobs[[i]] <- grobTree(images$grobs[[i]], vp = outer_vp)
    }
  }

  # Figure out width and height of math expressions
  maths <- x$maths
  if (length(maths$index) != 0) {
    math_metrics <- inject(rbind(!!!lapply(maths$shape, `[[`, "metrics")))
    ## Set dimensions in size and tracking (this is how it is passed to textshaping)
    size[maths$index] <- math_metrics$height + math_metrics$depth
    tracking[maths$index] <- math_metrics$width
  }

  # Shape text using widths (heights for vertical blocks, where the
  # constraint is the column length)
  shape_all <- function(heights_alloc, widths) {
    block_max_width <- ifelse(
      x$blocks$vertical,
      heights_alloc / 72,
      widths / 72
    )
    textshaping::shape_text(
      strings = x$text$text,
      id = x$text$block,
      family = x$text$family,
      italic = x$text$italic,
      weight = x$text$weight,
      width = x$text$width,
      features = x$text$features,
      size = size,
      res = 600,
      lineheight = x$text$lineheight,
      align = x$text$align,
      hjust = 0,
      # For vertical text vjust/hjust swap meaning: vjust controls the cross
      # (horizontal) axis with 0 putting the content inside the metrics box
      vjust = ifelse(x$text$text_direction %in% c("ttb", "ttb-ltr"), 0, 1),
      max_width = rep(block_max_width, x$blocks$length),
      tracking = tracking,
      indent = x$text$indent / 72, # These values are provided as bigpts by the style
      hanging = x$text$hanging / 72,
      space_before = 0,
      space_after = 0,
      direction = x$text$text_direction,
      mixed = x$text$mixed,
      kinsoku = x$text$kinsoku,
      kinsoku_style = x$text$kinsoku_style
    )
  }
  shape <- shape_all(heights_alloc, widths)
  if (nrow(shape$shape) == 0) {
    return(nullGrob())
  }

  # If any widths are not defined we grab the text widths from the shaping
  if (any(x$blocks$vertical_flow)) {
    ## Documents with vertical flow derive block sizes from their content, so
    ## widths can be filled in directly without reshaping. Empty blocks (e.g.
    ## a quote's or list's start string) report zero metrics since
    ## textshaping 1.0.5.9000, so they don't inflate the block box. The own
    ## widths are recomputed from the current shape inside fill_widths
    ## because the nested-fit loop reshapes
    pad_mar_lr <- rowSums(x$text[
      x$blocks$start,
      c("padding_left", "padding_right", "margin_left", "margin_right")
    ])
    fill_widths <- function() {
      own_w <- shape$metrics$width
      widths[x$blocks$vertical_flow] <<- NA
      for (i in rev(seq_len(max(x$blocks$indent)))) {
        for (j in which(x$blocks$indent == i & is.na(widths))) {
          k <- which(
            x$blocks$start > x$blocks$start[j] &
              x$blocks$start <= x$text$ends[x$blocks$start[j]] &
              x$blocks$indent == i + 1
          )
          kid_full <- widths[k] +
            pad_mar_lr[k]
          widths[j] <<- if (x$blocks$vertical[j]) {
            ### Children are laid out next to each other: widths add up
            own_w[j] + sum(kid_full)
          } else {
            max(own_w[j], kid_full, na.rm = TRUE)
          }
        }
      }
    }
    if (anyNA(widths)) {
      fill_widths()
    }
    ## Nested vertical blocks in a horizontal parent get the minimal column
    ## length that keeps their laid-out width inside the allocated parent
    ## width. Unbounded shaping produces a single column (minimal width,
    ## maximal height); the minimal height is thus found by spreading the
    ## content over as many columns as the parent width allows
    nested_root <- which(vflow_root & x$blocks$indent > 1)
    if (length(nested_root) != 0) {
      ## Iterative fit: each reshape changes the column packing, so repeat
      ## until stable. The 2%/5% inflation factors below compensate for
      ## columns rarely packing to exactly their estimated length; the
      ## iteration cap guards against oscillation between two packings
      for (iter in seq_len(8)) {
        avail <- widths[parent_idx[nested_root]] - pad_mar_lr[nested_root]
        needs_fit <- nested_root[is.na(heights_alloc[nested_root])]
        overflowing <- nested_root[
          !is.na(heights_alloc[nested_root]) &
            widths[nested_root] > avail
        ]
        if (length(needs_fit) == 0 && length(overflowing) == 0) {
          break
        }
        for (r in unique(c(needs_fit, overflowing))) {
          a <- max(avail[which(nested_root == r)], 1)
          if (is.na(heights_alloc[r])) {
            ### First pass: spread the single-column length over as many
            ### columns as fit the parent width
            n_cols <- max(floor(a / max(shape$metrics$width[r], 1)), 1)
            est <- shape$metrics$height[r] / n_cols * 1.02
          } else {
            ### Later passes: scale by the remaining overflow
            est <- heights_alloc[r] * (widths[r] / a) * 1.05
          }
          ## Constrain the root and its vertical descendants alike
          desc <- which(
            x$blocks$start > x$blocks$start[r] &
              x$blocks$start <= x$text$ends[x$blocks$start[r]] &
              x$blocks$vertical
          )
          targets <- c(r, desc)
          heights_alloc[targets] <- pmax(
            est,
            heights_alloc[targets],
            na.rm = TRUE
          )
        }
        shape <- shape_all(heights_alloc, widths)
        fill_widths()
      }
    }
  }
  if (anyNA(widths)) {
    added_width <- rowSums(x$text[
      x$blocks$start,
      c("padding_left", "padding_right", "margin_left", "margin_right")
    ])
    ## We go back from the most indented to the least and compound the widths
    for (i in rev(seq_len(max(x$blocks$indent)))) {
      blocks <- which(x$blocks$indent == i)
      widths[blocks] <- vapply(
        blocks,
        function(j) {
          k <- which(
            x$blocks$start > x$blocks$start[j] &
              x$blocks$start <= x$text$ends[x$blocks$start[j]]
          )
          max(shape$metrics$width[j], widths[j], widths[k], na.rm = TRUE) +
            added_width[j]
        },
        numeric(1)
      )
    }
    if (anyNA(widths)) {
      cli::cli_abort("Could not resolve width of text")
    }
    x$width <- unit(widths[x$blocks$indent == 1], "bigpts")
    ## Restart evaluation
    return(makeContext.marquee_grob(x))
  }

  # Set correct height for block images
  if (any(!x$images$inline)) {
    block_glyph_match <- match(
      shape$shape$string_id,
      x$images$index[!x$images$inline]
    )
    block_glyphs <- !is.na(block_glyph_match)
    shape$shape$y_offset[block_glyphs] <- -size[x$images$index[
      !x$images$inline
    ][block_glyph_match[block_glyphs]]]
    shape$shape$descender[block_glyphs] <- 0
    shape$metrics$height[x$text$block[x$images$index[
      !x$images$inline
    ]]] <- size[x$images$index[!x$images$inline]]
  }

  # Set correct height for block math
  if (length(maths$index) != 0 && any(!maths$inline)) {
    block_glyph_match <- match(
      shape$shape$string_id,
      maths$index[!maths$inline]
    )
    block_glyphs <- !is.na(block_glyph_match)
    shape$shape$y_offset[block_glyphs] <- -size[maths$index[
      !maths$inline
    ][block_glyph_match[block_glyphs]]]
    shape$shape$descender[block_glyphs] <- 0
    shape$metrics$height[x$text$block[maths$index[
      !maths$inline
    ]]] <- size[maths$index[!maths$inline]]
  }

  # Align inline math baselines with the text baseline. The placeholder glyph
  # shares the baseline of the surrounding text but its ascender/descender may
  # not cover the full extent of the math. Grow the line box to fit
  if (length(maths$index) != 0 && any(maths$inline)) {
    ## Keep track of the shifts already applied so multiple math expressions in
    ## the same block don't over-reserve space
    top_shift <- rep(0, length(shape$metrics$height))
    bottom_shift <- top_shift
    for (i in which(maths$inline)) {
      idx <- which(shape$shape$string_id == maths$index[i])[1]
      if (is.na(idx)) {
        next
      }
      block <- shape$shape$metric_id[idx]
      y0 <- shape$shape$y_offset[idx]
      ascender <- shape$shape$ascender[idx] + top_shift[block]
      descender <- shape$shape$descender[idx] - bottom_shift[block]
      math_h <- math_metrics$height[i]
      depth <- math_metrics$depth[i]
      if (math_h > ascender) {
        ### Math extends above the reserved box. Push the whole block down
        shift <- math_h - ascender
        in_block <- shape$shape$metric_id == block
        shape$shape[in_block, ] <- shift_shape(
          shape$shape[in_block, ],
          dy = -shift
        )
        shape$metrics$height[block] <- shape$metrics$height[block] + shift
        top_shift[block] <- top_shift[block] + shift
        y0 <- y0 - shift
        ascender <- math_h
      }
      if (depth > -descender) {
        ### Math extends below the reserved box. Push the lines below down
        shift <- depth + descender
        below <- shape$shape$metric_id == block & shape$shape$y_offset < y0
        shape$shape[below, ] <- shift_shape(shape$shape[below, ], dy = -shift)
        shape$metrics$height[block] <- shape$metrics$height[block] + shift
        bottom_shift[block] <- bottom_shift[block] + shift
        descender <- -depth
      }
      shape$shape$ascender[idx] <- ascender
      shape$shape$descender[idx] <- descender
    }
  }

  # Inherit color and id from parsed text
  idx <- shape$shape$string_id
  shape$shape$col <- x$text$color[idx]
  shape$shape$id <- x$text$id[idx]
  shape$shape$outline <- x$text$outline[idx]
  shape$shape$outline_width <- x$text$outline_width[idx]
  shape$shape$outline_type <- x$text$outline_type[idx]
  shape$shape$outline_join <- x$text$outline_join[idx]
  shape$shape$outline_mitre <- x$text$outline_mitre[idx]
  bshape <- x$bullets$shape

  # Init height structures
  heights <- rep(NA, length(x$blocks$start))
  top_offset <- heights
  cur_offset <- 0

  # Init justification info
  ink_left <- rep(NA, length(x$x))
  ink_right <- ink_left
  ink_top <- ink_left
  ink_bottom <- ink_left
  first_line <- ink_left
  last_line <- ink_left

  # Init bullet info
  bullet_blocks <- x$text$block[x$bullets$placement]
  y_adjustment <- rep(0, length(x$blocks$start))
  y_bullet_adjustment <- rep(0, length(bullet_blocks))
  x_bullet_adjustment <- rep(0, length(bullet_blocks))
  if (any(bshape$metrics$ltr != shape$metrics$ltr[bullet_blocks])) {
    # Redo bullet shaping with the correct direction
    bshape <- textshaping::shape_text(
      x$bullets$bullet,
      family = x$text$family[x$bullets$index],
      italic = x$text$italic[x$bullets$index],
      weight = x$text$weight[x$bullets$index],
      width = x$text$width[x$bullets$index],
      features = x$text$features[x$bullets$index],
      size = x$text$size[x$bullets$index],
      res = 600,
      hjust = ifelse(shape$metrics$ltr[bullet_blocks], 1, 0),
      vjust = 1,
      direction = ifelse(shape$metrics$ltr[bullet_blocks], "ltr", "rtl"),
      mixed = x$text$mixed[x$bullets$index],
      kinsoku = x$text$kinsoku[x$bullets$index],
      kinsoku_style = x$text$kinsoku_style[x$bullets$index]
    )
  }

  ## Inherit color and id from the relevant block
  idx <- x$bullets$index[bshape$shape$metric_id]
  bshape$shape$col <- x$text$color[idx]
  bshape$shape$id <- x$text$id[idx]
  bshape$shape$outline <- x$text$outline[idx]
  bshape$shape$outline_width <- x$text$outline_width[idx]
  bshape$shape$outline_type <- x$text$outline_type[idx]
  bshape$shape$outline_join <- x$text$outline_join[idx]
  bshape$shape$outline_mitre <- x$text$outline_mitre[idx]

  # make rtl bullets left-justified
  bltr <- bshape$metrics$ltr[bshape$shape$metric_id]
  bshape$shape[!bltr, ] <- shift_shape(
    bshape$shape[!bltr, ],
    dx = bshape$metrics$width[bshape$shape$metric_id[!bltr]]
  )

  # Add sizebased offset
  bshape$shape <- shift_shape(
    bshape$shape,
    dx = ifelse(bltr, -1, 1) * bshape$shape$font_size / 4
  )

  if (has_vertical_flow) {
    ## Children of each block
    kids <- vector("list", n_blocks)
    for (i in seq_len(n_blocks)) {
      if (parent_idx[i] != 0) {
        kids[[parent_idx[i]]] <- c(kids[[parent_idx[i]]], i)
      }
    }
    ## Own content size of each block, from the final shape (the
    ## nested-fit loop may have reshaped since fill_widths was defined)
    own_h <- shape$metrics$height
    own_w <- shape$metrics$width
    ## Full box sizes (content + padding + margin), compounded bottom-up
    full_h <- rep(NA, n_blocks)
    full_w <- rep(NA, n_blocks)
    pad_mar_w <- rowSums(x$text[
      x$blocks$start,
      c("padding_left", "padding_right", "margin_left", "margin_right")
    ])
    pad_mar_h <- rowSums(x$text[
      x$blocks$start,
      c("padding_top", "padding_bottom", "margin_top", "margin_bottom")
    ])
    for (i in rev(seq_len(n_blocks))) {
      if (!recursive_tree[i]) {
        next
      }
      ks <- kids[[i]]
      full_h[i] <- if (x$blocks$vertical[i]) {
        max(own_h[i], full_h[ks]) + pad_mar_h[i]
      } else {
        own_h[i] + sum(full_h[ks]) + pad_mar_h[i]
      }
      full_w[i] <- widths[i] + pad_mar_w[i]
    }
  }
  # The iterative loop only sees horizontal-flow blocks; vertical-flow roots
  # are included for height bookkeeping (as if they were leaves), their
  # descendants are fully handled by the recursion
  h_idx <- which(!recursive_tree | vflow_root)

  # Position blocks underneath each other, calculate justification info, and position bullets
  for (i in h_idx) {
    ## j is the location in x$text where style is given
    j <- x$blocks$start[i]
    indent <- x$blocks$indent[i]

    if (vflow_root[i]) {
      ### Opaque box in horizontal flow: only stack it and move on. The
      ### recursion below positions it and everything inside it
      if (indent == 1) {
        cur_offset <- 0
      }
      heights[i] <- if (!is.na(heights_alloc[i])) {
        ### A given height constrains the column length; the grob occupies it
        heights_alloc[i] + pad_mar_h[i]
      } else {
        full_h[i]
      }
      cur_offset <- cur_offset - heights[i]
      for (k in seq_len(indent - 1)) {
        heights[indent_stack[k]] <- heights[indent_stack[k]] + heights[i]
      }
      next_indent <- if (i == h_idx[length(h_idx)]) {
        1
      } else {
        x$blocks$indent[h_idx[which(h_idx == i) + 1]]
      }
      if (indent > next_indent) {
        for (k in next_indent + seq_len(indent - next_indent) - 1) {
          cur_offset <- cur_offset -
            x$text$padding_bottom[x$blocks$start[indent_stack[k]]] -
            x$text$margin_bottom[x$blocks$start[indent_stack[k]]]
        }
      }
      indent_stack[indent] <- i
      next
    }

    ## Position bullet if attached to this block
    bullet_ind <- which(i == bullet_blocks)
    if (length(bullet_ind) != 0) {
      ### Determine if bullet is higher than text at first line
      first <- which(shape$shape$metric_id == i)
      first <- first[
        shape$shape$y_offset[first] == max(shape$shape$y_offset[first])
      ]
      first <- first[
        if (shape$metrics$ltr[i]) {
          which.min(shape$shape$glyph[first])
        } else {
          which.max(shape$shape$glyph[first])
        }
      ]
      added_height <- shape$shape$y_offset[first] -
        bshape$shape$y_offset[match(bullet_ind, bshape$shape$metric_id)]
      if (added_height > 0) {
        #### If higher, adjust the height to make space and record adjustment for glyphs in block
        y_adjustment[i] <- added_height
        shape$metrics$height[i] <- shape$metrics$height[i] + added_height
      } else {
        #### Otherwise record negative adjustment to put it in line with the text in the list
        y_bullet_adjustment[bullet_ind] <- -1 * added_height
      }
      x_bullet_adjustment[bullet_ind] <- shape$shape$x_offset[first] +
        if (shape$metrics$ltr[i]) 0 else shape$shape$advance[first]
    }

    ## calculate y offset and height
    if (indent == 1) {
      ### Body tag, start again
      cur_offset <- 0
    }

    ### Record offset based on top padding and margin
    top_offset[i] <- cur_offset - x$text$margin_top[j] - x$text$padding_top[j]
    ### Init height as sum of top and bottom margin+padding
    heights[i] <- x$text$margin_top[j] +
      x$text$padding_top[j] +
      x$text$padding_bottom[j] +
      x$text$margin_bottom[j]
    ### If block has content of it's own, add it to the height
    if (nzchar(x$text$text[j]) || x$blocks$length[i] != 1) {
      heights[i] <- heights[i] + shape$metrics$height[i]
    }

    ### Move down offset by height
    cur_offset <- cur_offset - heights[i]

    ### Increase height of parents
    for (k in seq_len(indent - 1)) {
      heights[indent_stack[k]] <- heights[indent_stack[k]] + heights[i]
    }

    h_pos <- which(h_idx == i)
    next_indent <- if (h_pos == length(h_idx)) {
      1
    } else {
      x$blocks$indent[h_idx[h_pos + 1]]
    }
    if (indent > next_indent) {
      ### If exiting a block, add parent blocks bottom padding + margin to offset
      for (k in next_indent + seq_len(indent - next_indent) - 1) {
        cur_offset <- cur_offset -
          x$text$padding_bottom[x$blocks$start[indent_stack[k]]] -
          x$text$margin_bottom[x$blocks$start[indent_stack[k]]]
      }
    } else if (indent < next_indent) {
      ### If block has children, move up offset so it doesn't include bottom margin and padding
      cur_offset <- cur_offset +
        x$text$padding_bottom[j] +
        x$text$margin_bottom[j]
    }
    indent_stack[indent] <- i

    ## Record boundaries
    text_ind <- x$text$id[j]
    y_offsets <- shape$shape$y_offset[shape$shape$metric_id == i]
    if (length(y_offsets) > 0) {
      if (is.na(ink_top[text_ind])) {
        ink_top[text_ind] <- top_offset[i] - shape$metrics$top_bearing[i]
        first_line[text_ind] <- top_offset[i] + max(y_offsets)
      }
      ink_left[text_ind] <- min(
        ink_left[text_ind],
        left_offset[i] + shape$metrics$left_bearing[i],
        na.rm = TRUE
      )
      ink_right[text_ind] <- max(
        ink_right[text_ind],
        left_offset[i] +
          shape$metrics$width[i] -
          shape$metrics$right_bearing[i],
        na.rm = TRUE
      )
      ink_bottom[text_ind] <- min(
        ink_bottom[text_ind],
        top_offset[i] -
          shape$metrics$height[i] +
          shape$metrics$bottom_bearing[i],
        na.rm = TRUE
      )
      last_line[text_ind] <- min(
        last_line[text_ind],
        top_offset[i] + min(y_offsets),
        na.rm = TRUE
      )
    }
  }

  # Lay out blocks in vertical flow. Blocks are positioned as boxes in
  # the flow of their parent, which is transposed to the horizontal axis when
  # the parent's direction is vertical (right-to-left for ttb, left-to-right
  # for ttb-ltr)
  if (has_vertical_flow) {
    ## Place each block, then recurse into children. `edge` is the position
    ## of the flow-start margin edge (x for vertical flow, y for horizontal
    ## flow), `cross` is the parent's content-start position on the cross axis
    ## (content top for vertical flow, content left for horizontal flow)
    glyph_x_base <- left_offset
    place_block <- function(i, edge, cross) {
      j <- x$blocks$start[i]
      marL <- x$text$margin_left[j]
      marR <- x$text$margin_right[j]
      marT <- x$text$margin_top[j]
      padL <- x$text$padding_left[j]
      padR <- x$text$padding_right[j]
      padT <- x$text$padding_top[j]
      if (x$blocks$vertical_flow[i]) {
        ttb <- x$blocks$flow[i] == "ttb"
        if (ttb) {
          content_right <- edge - marR - padR
          content_left <- content_right - widths[i]
        } else {
          content_left <- edge + marL + padL
          content_right <- content_left + widths[i]
        }
      } else {
        content_left <- cross + marL + padL
        content_right <- content_left + widths[i]
      }
      left_offset[i] <<- content_left
      top_offset[i] <<- if (x$blocks$vertical_flow[i]) {
        cross - marT - padT
      } else {
        edge - marT - padT
      }
      ## Own content sits at the flow-start side of the content box, anchored
      ## by the block's own direction (not the parent's flow)
      glyph_x_base[i] <<- if (x$blocks$vertical[i]) {
        glyph_origin_x(x$blocks$direction[i], content_left, widths[i], own_w[i])
      } else {
        content_left
      }
      heights[i] <<- full_h[i]
      ks <- kids[[i]]
      if (length(ks) > 0) {
        if (x$blocks$vertical[i]) {
          child_ttb <- x$blocks$direction[i] == "ttb"
          cur <- flow_start_x(
            x$blocks$direction[i],
            content_left,
            widths[i],
            own_w[i]
          )
          for (c in ks) {
            place_block(c, cur, top_offset[i])
            cur <- if (child_ttb) cur - full_w[c] else cur + full_w[c]
          }
        } else {
          cur_y <- top_offset[i] - own_h[i]
          for (c in ks) {
            place_block(c, cur_y, content_left)
            cur_y <- cur_y - full_h[c]
          }
        }
      }
      ## Record boundaries (same physical interpretation as horizontal flow)
      text_ind <- x$text$id[j]
      y_offsets <- shape$shape$y_offset[shape$shape$metric_id == i]
      if (length(y_offsets) > 0) {
        if (is.na(ink_top[text_ind])) {
          ink_top[text_ind] <<- top_offset[i] - shape$metrics$top_bearing[i]
          first_line[text_ind] <<- top_offset[i] + max(y_offsets)
        }
        ink_left[text_ind] <<- min(
          ink_left[text_ind],
          left_offset[i] + shape$metrics$left_bearing[i],
          na.rm = TRUE
        )
        ink_right[text_ind] <<- max(
          ink_right[text_ind],
          left_offset[i] +
            shape$metrics$width[i] -
            shape$metrics$right_bearing[i],
          na.rm = TRUE
        )
        ink_bottom[text_ind] <<- min(
          ink_bottom[text_ind],
          top_offset[i] -
            shape$metrics$height[i] +
            shape$metrics$bottom_bearing[i],
          na.rm = TRUE
        )
        last_line[text_ind] <<- min(
          last_line[text_ind],
          top_offset[i] + min(y_offsets),
          na.rm = TRUE
        )
      }
    }
    for (root in which(vflow_root)) {
      if (x$blocks$indent[root] == 1) {
        place_block(root, 0, 0)
      } else {
        ### Nested root: place it inside its parent's content box, after (in
        ### flow direction) the parent's own content
        p <- parent_idx[root]
        edge <- flow_start_x(
          x$blocks$flow[root],
          left_offset[p],
          widths[p],
          own_w[p]
        )
        place_block(root, edge, top_offset[p])
      }
    }
    ### Vertical-direction blocks handled by the iterative loop (horizontal
    ### flow) keep their own columns at the flow-start side of their box
    iter_vertical <- which(x$blocks$vertical & !recursive_tree)
    glyph_x_base[iter_vertical] <- glyph_origin_x(
      x$blocks$direction[iter_vertical],
      left_offset[iter_vertical],
      widths[iter_vertical],
      own_w[iter_vertical]
    )
  } else {
    glyph_x_base <- left_offset
  }

  # Update offsets based on calculated width and heights
  shape$shape <- shift_shape(
    shape$shape,
    dx = glyph_x_base[shape$shape$metric_id],
    dy = top_offset[shape$shape$metric_id] -
      y_adjustment[shape$shape$metric_id]
  )

  # Apply baseline shift
  shape$shape <- shift_shape(
    shape$shape,
    dy = x$text$baseline[shape$shape$string_id]
  )

  ## Do the same for bullets
  bshape$shape <- shift_shape(
    bshape$shape,
    dx = left_offset[bullet_blocks[bshape$shape$metric_id]] +
      x_bullet_adjustment[bshape$shape$metric_id],
    dy = top_offset[bullet_blocks[bshape$shape$metric_id]] -
      y_bullet_adjustment[bshape$shape$metric_id]
  )

  ## Bullets in vertical flow occupy the first character cell of the
  ## indentation at the inline start (top) of the first column — the
  ## vertical analogue of sitting in the left indent in horizontal lists
  vert_bullet <- x$blocks$vertical_flow[bullet_blocks]
  if (any(vert_bullet)) {
    for (b in which(vert_bullet)) {
      i <- bullet_blocks[b]
      gl <- which(shape$shape$metric_id == i)
      if (length(gl) == 0) {
        next
      }
      ttb <- x$blocks$flow[i] == "ttb"
      ### First column is right-most for ttb, left-most for ttb-ltr
      ### (identified by cell edges: rotated runs sit off the pen line, so
      ### x_offset is not a reliable column key)
      first_col <- gl[
        if (ttb) {
          which.max(shape$shape$cell_x1[gl])
        } else {
          which.min(shape$shape$cell_x0[gl])
        }
      ]
      bgl <- which(bshape$shape$metric_id == b)
      ### Centre the marker run on the text column
      text_center <- (shape$shape$cell_x0[first_col] +
        shape$shape$cell_x1[first_col]) /
        2
      bullet_w <- max(
        bshape$shape$x_offset[bgl] + bshape$shape$advance[bgl]
      ) -
        min(bshape$shape$x_offset[bgl])
      target_x <- text_center - bullet_w / 2
      ## The marker hangs at the column head, in the space vacated by the
      ## first-column indent (at least one cell above the first glyph)
      target_y <- shape$shape$cell_y1[first_col] +
        max(
          x$text$indent[x$blocks$start[i]] - shape$shape$advance[first_col],
          0
        )
      bshape$shape[bgl, ] <- shift_shape(
        bshape$shape[bgl, ],
        dx = target_x - bshape$shape$x_offset[bgl[1]],
        dy = target_y - bshape$shape$y_offset[bgl[1]]
      )
    }
  }

  # Store info in object
  x$shape <- shape$shape
  widths <- widths +
    rowSums(x$text[
      x$blocks$start,
      c("padding_left", "padding_right", "margin_left", "margin_right")
    ])
  root_content_width <- widths[x$blocks$indent == 1]
  x$widths <- root_content_width
  x$heights <- heights[x$blocks$indent == 1]
  ## Hard constraints: a given width clips vertical text at the block axis, a
  ## given height clips horizontal text (NA means content-sized). For vertical
  ## roots a given height constrains column length (handled in shaping)
  root_vertical <- x$blocks$vertical_flow[x$blocks$indent == 1]
  constrained <- rep(FALSE, length(x$x))
  for (t in seq_along(x$x)) {
    if (root_vertical[t]) {
      if (!is.na(width[t])) {
        x$widths[t] <- width[t]
        constrained[t] <- TRUE
      }
    } else if (!is.na(height[t])) {
      x$heights[t] <- height[t]
      constrained[t] <- TRUE
    }
  }
  x$clip <- constrained & x$clip == "on"
  ## Adjust y so it is zero-justified
  x$shape <- shift_shape(x$shape, dy = x$heights[x$shape$id])
  bshape$shape <- shift_shape(bshape$shape, dy = x$heights[bshape$shape$id])
  top_offset <- top_offset + x$heights[x$text$id[x$blocks$start]]
  ## ttb trees are laid out to the left of the origin by place_block
  ## (content flows right-to-left into negative x). Shift them so the
  ## left-most edge is at zero — justification then works from a
  ## [0, widths] box (ttb-ltr trees already extend right of the origin)
  if (has_vertical_flow) {
    ttb_root <- x$blocks$indent == 1 & x$blocks$flow == "ttb"
    if (any(ttb_root)) {
      ttb_texts <- x$text$id[x$blocks$start[ttb_root]]
      ttb_blocks <- x$text$id[x$blocks$start] %in% ttb_texts
      x_shift <- rep(0, length(x$x))
      x_shift[ttb_texts] <- x$widths[ttb_texts]
      x$shape <- shift_shape(x$shape, dx = x_shift[x$shape$id])
      bshape$shape <- shift_shape(bshape$shape, dx = x_shift[bshape$shape$id])
      left_offset[ttb_blocks] <- left_offset[ttb_blocks] +
        x_shift[x$text$id[x$blocks$start[ttb_blocks]]]
      ink_left[ttb_texts] <- ink_left[ttb_texts] + x_shift[ttb_texts]
      ink_right[ttb_texts] <- ink_right[ttb_texts] + x_shift[ttb_texts]
    }
  }
  ## Combine main text with bullets
  n_text_string_ids <- max(x$shape$string_id)
  bshape$shape$string_id <- bshape$shape$string_id + n_text_string_ids
  x$shape <- rbind(x$shape, bshape$shape)

  first_line <- first_line + x$heights
  ink_top <- ink_top + x$heights
  ink_bottom <- ink_bottom + x$heights
  last_line <- last_line + x$heights
  if (is.character(x$hjust)) {
    x$hjust <- vapply(
      seq_along(x$hjust),
      function(i) {
        just <- x$hjust[i]
        w <- x$widths[i]
        switch(
          just,
          "left" = 0,
          "left-ink" = ink_left[i] / w,
          "center" = 0.5,
          "center-ink" = mean(c(ink_left[i], ink_right[i])) / w,
          "right-ink" = ink_right[i] / w,
          "right" = 1
        )
      },
      numeric(1)
    )
  } else if (inherits(x$hjust, "marquee_ink")) {
    x$hjust <- ifelse(
      vctrs::vec_data(x$hjust)$ink,
      (ink_left + (ink_right - ink_left) * vctrs::vec_data(x$hjust)$val) /
        x$widths,
      vctrs::vec_data(x$hjust)$val
    )
  }
  if (is.character(x$vjust)) {
    x$vjust <- vapply(
      seq_along(x$vjust),
      function(i) {
        just <- x$vjust[i]
        h <- x$heights[i]
        switch(
          just,
          "bottom" = 0,
          "bottom-ink" = ink_bottom[i] / h,
          "last-line" = last_line[i] / h,
          "center" = 0.5,
          "center-ink" = mean(c(ink_bottom[i], ink_top[i])) / h,
          "first-line" = first_line[i] / h,
          "top-ink" = ink_top[i] / h,
          "top" = 1
        )
      },
      numeric(1)
    )
  } else if (inherits(x$vjust, "marquee_ink")) {
    x$vjust <- ifelse(
      vctrs::vec_data(x$vjust)$ink,
      (ink_bottom + (ink_top - ink_bottom) * vctrs::vec_data(x$vjust)$val) /
        x$heights,
      vctrs::vec_data(x$vjust)$val
    )
  }

  # Perform justification
  x$shape <- shift_shape(
    x$shape,
    dx = -(x$widths * x$hjust)[x$shape$id],
    dy = -(x$heights * x$vjust)[x$shape$id]
  )
  left_offset <- left_offset - (x$widths * x$hjust)[x$text$id[x$blocks$start]]
  top_offset <- top_offset - (x$heights * x$vjust)[x$text$id[x$blocks$start]]

  # Precalculate full sizing of grob
  x0 <- -x$hjust * x$widths
  x1 <- (1 - x$hjust) * x$widths
  y0 <- -x$vjust * x$heights
  y1 <- (1 - x$vjust) * x$heights
  rad <- x$angle * 2 * pi / 360
  crad <- cos(rad)
  srad <- sin(rad)
  blx <- x0 * crad - y0 * srad
  bly <- x0 * srad + y0 * crad
  brx <- x1 * crad - y0 * srad
  bry <- x1 * srad + y0 * crad
  trx <- x1 * crad - y1 * srad
  try <- x1 * srad + y1 * crad
  tlx <- x0 * crad - y1 * srad
  tly <- x0 * srad + y1 * crad

  x$bbox <- list(
    x = rep(x$x, 4) + unit(c(blx, brx, trx, tlx), "bigpts"),
    y = rep(x$y, 4) + unit(c(bly, bry, try, tly), "bigpts")
  )
  if (length(x$x) > 1) {
    x$full_width <- max(x$x + unit(pmax(blx, brx, trx, tlx), "bigpts")) -
      min(x$x + unit(pmin(blx, brx, trx, tlx), "bigpts"))
    x$full_height <- max(x$y + unit(pmax(bly, bry, try, tly), "bigpts")) -
      min(x$y + unit(pmin(bly, bry, try, tly), "bigpts"))
  } else {
    # If we are dealing with a single text (we often are), we can simplify this
    x$full_width <- unit(diff(range(blx, brx, trx, tlx)), "bigpts")
    x$full_height <- unit(diff(range(bly, bry, try, tly)), "bigpts")
  }

  # Extract info about decoration (background, border, underline, etc)
  ## Figure out which blocks has backgrounds or borders
  has_deco <- !vapply(
    x$text$background,
    function(x) is.character(x) && is.na(x),
    logical(1)
  ) |
    (!is.na(x$text$border) &
      (x$text$border_width_bottom > 0 |
        x$text$border_width_top > 0 |
        x$text$border_width_left > 0 |
        x$text$border_width_right > 0))

  ## Handle block backgrounds
  block_bg <- has_deco[x$blocks$start]
  block_rects <- list(
    id = x$text$id[x$blocks$start[block_bg]],
    x = left_offset[block_bg] - x$text$padding_left[x$blocks$start[block_bg]],
    y = top_offset[block_bg] + x$text$padding_top[x$blocks$start[block_bg]],
    ## Border box = content + padding: exclude margins on both axes
    ## (heights already excluded top/bottom margins)
    width = widths[block_bg] -
      x$text$margin_left[x$blocks$start[block_bg]] -
      x$text$margin_right[x$blocks$start[block_bg]],
    height = heights[block_bg] -
      x$text$margin_top[x$blocks$start[block_bg]] -
      x$text$margin_bottom[x$blocks$start[block_bg]],
    fill = x$text$background[x$blocks$start[block_bg]],
    col = x$text$border[x$blocks$start[block_bg]],
    r = x$text$border_radius[x$blocks$start[block_bg]],
    left = x$text$border_width_left[x$blocks$start[block_bg]],
    right = x$text$border_width_right[x$blocks$start[block_bg]],
    top = x$text$border_width_top[x$blocks$start[block_bg]],
    bottom = x$text$border_width_bottom[x$blocks$start[block_bg]],
    lty = x$text$border_type[x$blocks$start[block_bg]]
  )

  ## Handle span backgrounds
  span_bg <- which(has_deco & !seq_along(has_deco) %in% x$blocks$start)
  ### Consecutive spans with same bg/border are merged
  span_bg_may_merge <- which(span_bg[-1] - span_bg[-length(span_bg)] == 1)
  span_bg <- as.list(span_bg)
  for (j in rev(span_bg_may_merge)) {
    i <- span_bg[[j]]
    if (has_identical_background(i, i + 1, x$text)) {
      span_bg[[j]] <- c(span_bg[[j]], span_bg[[j + 1]])
      span_bg[j + 1] <- NULL
    }
  }

  ## Glyph cells (cell_x0..cell_y1 from textshaping) give the em box across
  ## the main axis and the advance slot along it, tiling seamlessly across
  ## glyph types and rotations — decorations are anchored to them directly
  span_rects <- lapply(span_bg, function(i) {
    ### Find the glyphs related to the span
    span_ind <- which(x$shape$string_id %in% i)
    if (x$text$text_direction[i[1]] %in% c("ttb", "ttb-ltr")) {
      ### Vertical text: split up by column and make a rect for each. Glyph
      ### order follows reading order, and within a column the cells tile
      ### downward, so a new column starts when the cell top jumps back up.
      ### Grouping by flow rather than by cell_x0 is robust to spacer and
      ### rotated glyphs whose cross-axis cell span can differ by font
      tops <- x$shape$cell_y1[span_ind]
      col_id <- cumsum(c(TRUE, diff(tops) > 1e-6))
      lapply(split(span_ind, col_id), function(j) {
        left <- min(x$shape$cell_x0[j])
        right <- max(x$shape$cell_x1[j])
        top <- max(x$shape$cell_y1[j])
        bottom <- min(x$shape$cell_y0[j])
        c(i[1], left, top, right - left, top - bottom)
      })
    } else {
      ### Split up by line and make a rect for each
      lapply(split(span_ind, x$shape$y_offset[span_ind]), function(j) {
        left <- min(x$shape$x_offset[j])
        top <- max(x$shape$y_offset[j] + x$shape$ascender[j])
        c(
          i[1],
          left,
          top,
          max(x$shape$x_offset[j] + x$shape$advance[j]) - left,
          top - min(x$shape$y_offset[j] + x$shape$descender[j])
        )
      })
    }
  })
  span_rects <- inject(rbind(!!!unlist(span_rects, recursive = FALSE)))

  ### Combine block and span rects
  # Horizontal padding is not added here: add_inline_padding() has already
  # inserted spacer glyphs at the span boundaries, so the glyph extent above
  # includes it (spans starting a block get no spacers and lose horizontal
  # padding in their rect — an accepted edge case). The cross axis is
  # handled here, in the span's own frame: the ascender side takes
  # padding_top, the descender side padding_bottom. For vertical text the
  # ascender side is right for ttb and upright ttb-ltr text, left for +90
  # runs
  vert_span <- x$text$text_direction[span_rects[, 1]] %in% c("ttb", "ttb-ltr")
  ccw <- vert_span &
    x$text$text_direction[span_rects[, 1]] == "ttb-ltr" &
    vapply(
      span_rects[, 1],
      function(i) any(x$shape$rotation[x$shape$string_id == i] > 0),
      logical(1)
    )
  pad_asc <- ifelse(vert_span, x$text$padding_top[span_rects[, 1]], 0)
  pad_desc <- ifelse(vert_span, x$text$padding_bottom[span_rects[, 1]], 0)
  pad_l <- ifelse(ccw, pad_asc, pad_desc)
  pad_r <- ifelse(ccw, pad_desc, pad_asc)
  pad_top <- ifelse(vert_span, 0, x$text$padding_top[span_rects[, 1]])
  pad_bot <- ifelse(vert_span, 0, x$text$padding_bottom[span_rects[, 1]])
  x$rects <- list(
    id = c(block_rects$id, x$text$id[span_rects[, 1]]),
    x = c(block_rects$x, span_rects[, 2] - pad_l),
    y = c(block_rects$y, span_rects[, 3] + pad_top),
    width = c(block_rects$width, span_rects[, 4] + pad_l + pad_r),
    height = c(
      block_rects$height,
      span_rects[, 5] + pad_top + pad_bot
    ),
    fill = c(block_rects$fill, x$text$background[span_rects[, 1]]),
    col = c(block_rects$col, x$text$border[span_rects[, 1]]),
    r = c(block_rects$r, x$text$border_radius[span_rects[, 1]]),
    left = c(block_rects$left, x$text$border_width_left[span_rects[, 1]]),
    right = c(block_rects$right, x$text$border_width_right[span_rects[, 1]]),
    top = c(block_rects$top, x$text$border_width_top[span_rects[, 1]]),
    bottom = c(block_rects$bottom, x$text$border_width_bottom[span_rects[, 1]]),
    lty = c(block_rects$lty, x$text$border_type[span_rects[, 1]])
  )

  ## Extract position of underline and strikethrough
  lines <- lapply(which(x$text$underline | x$text$strikethrough), function(i) {
    span_ind <- which(x$shape$string_id == i)
    vertical <- x$text$text_direction[i] %in% c("ttb", "ttb-ltr")
    ### Like span rects we split by line (column for vertical text,
    ### identified by the column box left edge so all glyph types group
    ### together)
    lapply(
      split(
        span_ind,
        if (vertical) {
          round(x$shape$cell_x0[span_ind], 6)
        } else {
          x$shape$y_offset[span_ind]
        }
      ),
      function(j) {
        ### Line width and position is relative to size. Spacer (null)
        ### glyphs carry their span's resolved font since textshaping
        ### 1.0.5.9000, so any glyph — even in a padding-only span —
        ### provides valid font metrics
        ref <- j[1]
        size <- x$shape$font_size[ref]
        info <- systemfonts::font_info(
          path = x$shape$font_path[ref],
          index = x$shape$font_index[ref],
          size = size,
          res = 600
        )
        lwd <- info$underline_size * 72 / 600
        if (vertical) {
          ### Underlines are drawn on the "under" side of the column (right
          ### for ttb, left for ttb-ltr), strikethrough through its center.
          ### Rotated runs are decorated exactly as if they were unrotated:
          ### the underline sits below the run's baseline (left of it for
          ### -90 runs, right of it for +90 runs) and the strikethrough
          ### crosses the baseline at +0.3em, so decorations switch side
          ### within a column when the glyph rotation changes
          ttb <- x$text$text_direction[i] == "ttb"
          up <- x$shape$rotation[j] == 0
          solid <- !x$shape$spacer[j]
          glyph_left <- x$shape$cell_x0[j]
          glyph_right <- x$shape$cell_x1[j]
          ### Anchor the line ends to the glyph extents along the column:
          ### the ink box (upright, solid) or the advance cell (rotated and
          ### spacer glyphs)
          ink <- up & solid
          y_top <- ifelse(
            ink,
            x$shape$y_offset[j] + x$shape$ascender[j],
            x$shape$cell_y1[j]
          )
          y_bot <- ifelse(
            ink,
            x$shape$y_offset[j] + x$shape$descender[j],
            x$shape$cell_y0[j]
          )
          ### Split the column into contiguous upright/rotated segments;
          ### spacers join the segment of their nearest solid glyph. A
          ### group of pure spacers (a padding-only token) inherits the
          ### rotation of the nearest solid glyph in the block
          rotated_seg <- !up
          if (any(!solid) && any(solid)) {
            first_type <- (!up)[which(solid)[1]]
            for (k in which(!solid)) {
              rotated_seg[k] <- if (k == 1L) first_type else rotated_seg[k - 1L]
            }
          }
          ### Reference glyph for an all-spacer segment's decoration: the
          ### nearest solid glyph of the span the padding belongs to,
          ### identified by its identical background (padding spacers are
          ### duplicates of the span token). Its own rotation is always 0,
          ### so a spacer can't speak for the segment
          seg_ref <- if (!any(solid)) {
            blk <- which(
              x$shape$metric_id == x$shape$metric_id[j[1]] & !x$shape$spacer
            )
            same_bg <- blk[vapply(
              x$shape$string_id[blk],
              function(s) has_identical_background(i, s, x$text),
              logical(1)
            )]
            cand <- if (length(same_bg) == 0) blk else same_bg
            if (length(cand) == 0) {
              ref
            } else {
              cand[which.min(abs(cand - j[1]))]
            }
          } else {
            ref
          }
          if (!any(solid)) {
            rotated_seg[] <- x$shape$rotation[seg_ref] != 0
          }
          seg_id <- cumsum(c(
            TRUE,
            rotated_seg[-1] != rotated_seg[-length(rotated_seg)]
          ))
          seg_lines <- lapply(split(seq_along(j), seg_id), function(sg) {
            sol <- which(solid[sg])
            sinfo <- if (length(sol) == 0) {
              info
            } else {
              sg_ref <- j[sg[sol[1]]]
              systemfonts::font_info(
                path = x$shape$font_path[sg_ref],
                index = x$shape$font_index[sg_ref],
                size = x$shape$font_size[sg_ref],
                res = 600
              )
            }
            slwd <- sinfo$underline_size * 72 / 600
            upos <- sinfo$underline_pos * 72 / 600
            if (rotated_seg[sg[1]]) {
              ### As if unrotated: positions relative to the run's baseline
              rot_g <- j[sg[!up[sg]][1]]
              ### All-spacer segment: use the nearest solid glyph of the
              ### block (classified as rotated above)
              if (is.na(rot_g)) {
                rot_g <- seg_ref
              }
              base <- x$shape$x_offset[rot_g]
              ccw <- x$shape$rotation[rot_g] > 0
              u_x <- if (ccw) base - upos else base + upos
              s_x <- if (ccw) {
                base - 0.3 * x$shape$font_size[rot_g]
              } else {
                base + 0.3 * x$shape$font_size[rot_g]
              }
            } else {
              side <- if (ttb) max(glyph_right[sg]) else min(glyph_left[sg])
              u_x <- side + if (ttb) -upos else upos
              s_x <- mean(range((glyph_left[sg] + glyph_right[sg]) / 2))
            }
            line_x <- c(u_x, s_x)[c(
              x$text$underline[i],
              x$text$strikethrough[i]
            )]
            cbind(i, line_x, max(y_top[sg]), min(y_bot[sg]), slwd)
          })
          l <- do.call(rbind, seg_lines)
        } else {
          mod <- c(info$underline_pos * 72 / 600, 0.3 * size)
          mod <- mod[c(x$text$underline[i], x$text$strikethrough[i])]
          l <- c(
            i,
            min(x$shape$x_offset[j]),
            max(x$shape$x_offset[j] + x$shape$advance[j]),
            x$shape$y_offset[j[1]],
            lwd
          )
          if (length(mod) == 2) {
            l <- rbind(l, l)
            l[, 4] <- l[, 4] + mod
          } else {
            l[4] <- l[4] + mod
          }
        }
        l
      }
    )
  })
  lines <- inject(rbind(!!!unlist(lines, recursive = FALSE)))
  ### Horizontal tuples are (i, x0, x1, y, lwd), vertical ones are
  ### (i, x, ytop, ybot, lwd)
  horizontal_line <- !x$text$text_direction[lines[, 1]] %in% c("ttb", "ttb-ltr")
  x$lines <- list(
    id = x$text$id[lines[, 1]],
    x0 = lines[, 2],
    y0 = ifelse(horizontal_line, lines[, 4], lines[, 3]),
    x1 = ifelse(horizontal_line, lines[, 3], lines[, 2]),
    y1 = lines[, 4],
    lwd = lines[, 5],
    col = x$text$color[lines[, 1]]
  )
  # Emphasis marks (kenten). One small mark glyph per character, placed
  # beside the glyph (right of the column for ttb, left for ttb-ltr, above
  # the line for horizontal text)
  em_tokens <- which(!is.na(x$text$emphasis))
  if (length(em_tokens) != 0) {
    em_glyphs <- lapply(em_tokens, function(i) {
      ind <- which(x$shape$string_id == i & !x$shape$spacer)
      if (length(ind) == 0) {
        return(NULL)
      }
      vertical <- x$text$text_direction[i] %in% c("ttb", "ttb-ltr")
      ### x_offset is the pen origin at the left edge of the em box; the
      ### ink edge is near x_offset + ascender - descender, so anchor the
      ### marks to the em-box edge, not the ascender
      size <- x$text$size[i]
      mark <- textshaping::shape_text(
        x$text$emphasis[i],
        family = x$text$family[i],
        italic = x$text$italic[i],
        weight = x$text$weight[i],
        size = size / 2,
        res = 600,
        vjust = 0
      )
      mark_w <- mark$metrics$width[1]
      mark_h <- mark$metrics$height[1]
      gap <- size / 10
      if (vertical) {
        ### Center the mark on the glyph's cell and place it beside the
        ### em-box edge (right for ttb, left for ttb-ltr)
        center_y <- (x$shape$cell_y0[ind] + x$shape$cell_y1[ind]) / 2
        if (x$text$text_direction[i] == "ttb") {
          center_x <- x$shape$cell_x1[ind] + gap + mark_w / 2
        } else {
          center_x <- x$shape$cell_x0[ind] - gap - mark_w / 2
        }
      } else {
        center_x <- x$shape$x_offset[ind] + x$shape$advance[ind] / 2
        center_y <- x$shape$y_offset[ind] +
          x$shape$ascender[ind] +
          gap +
          mark_h / 2
      }
      n <- length(ind)
      data.frame(
        glyph = seq_len(n),
        index = mark$shape$index[1],
        metric_id = x$shape$metric_id[ind],
        string_id = i,
        x_offset = center_x - mark$shape$x_offset[1] - mark_w / 2,
        y_offset = center_y - mark$shape$y_offset[1] - mark_h / 2,
        font_path = mark$shape$font_path[1],
        font_index = mark$shape$font_index[1],
        font_size = mark$shape$font_size[1],
        advance = 0,
        ascender = mark$shape$ascender[1],
        descender = mark$shape$descender[1],
        rotation = 0,
        spacer = FALSE,
        cell_x0 = center_x - mark_w / 2,
        cell_x1 = center_x + mark_w / 2,
        cell_y0 = center_y - mark_h / 2,
        cell_y1 = center_y + mark_h / 2,
        col = x$shape$col[ind],
        id = x$shape$id[ind],
        outline = x$shape$outline[ind],
        outline_width = x$shape$outline_width[ind],
        outline_type = x$shape$outline_type[ind],
        outline_join = x$shape$outline_join[ind],
        outline_mitre = x$shape$outline_mitre[ind]
      )
    })
    x$shape <- rbind(
      x$shape,
      inject(rbind(!!!em_glyphs[!vapply(em_glyphs, is.null, logical(1))]))
    )
  }
  # Place math glyphs, lines, and boxes based on the null-glyph placeholders
  if (length(maths$index) != 0) {
    math_glyphs <- vector("list", length(maths$index))
    math_lines <- vector("list", length(maths$index))
    math_boxes <- vector("list", length(maths$index))
    for (i in seq_along(maths$index)) {
      placeholder <- which(x$shape$string_id == maths$index[i])[1]
      if (is.na(placeholder)) {
        next
      }

      x0 <- x$shape$x_offset[placeholder]
      ## Inline math sits on the text baseline. Block math is placed like a
      ## block image, at the bottom of the reserved box plus the depth
      y0 <- x$shape$y_offset[placeholder]
      if (!maths$inline[i]) {
        y0 <- y0 +
          x$shape$descender[placeholder] +
          math_metrics$depth[i]
      }
      shaped <- maths$shape[[i]]
      glyphs <- shaped$shape
      math_glyphs[[i]] <- data.frame(
        glyph = glyphs$glyph,
        index = glyphs$index,
        metric_id = x$shape$metric_id[placeholder],
        string_id = maths$index[i],
        x_offset = x0 + glyphs$x_offset,
        y_offset = y0 + glyphs$y_offset,
        font_path = glyphs$font_path,
        font_index = glyphs$font_index,
        font_size = glyphs$font_size,
        advance = glyphs$advance,
        ascender = math_metrics$height[i],
        descender = -math_metrics$depth[i],
        rotation = 0,
        spacer = FALSE,
        cell_x0 = x0 + glyphs$x_offset,
        cell_x1 = x0 + glyphs$x_offset + glyphs$advance,
        cell_y0 = y0 + glyphs$y_offset - math_metrics$depth[i],
        cell_y1 = y0 + glyphs$y_offset + math_metrics$height[i],
        col = glyphs$color,
        id = x$shape$id[placeholder],
        outline = x$shape$outline[placeholder],
        outline_width = x$shape$outline_width[placeholder],
        outline_type = x$shape$outline_type[placeholder],
        outline_join = x$shape$outline_join[placeholder],
        outline_mitre = x$shape$outline_mitre[placeholder]
      )
      if (nrow(shaped$lines) != 0) {
        math_lines[[i]] <- data.frame(
          id = x$shape$id[placeholder],
          x0 = x0 + shaped$lines$x1,
          y0 = y0 + shaped$lines$y1,
          x1 = x0 + shaped$lines$x2,
          y1 = y0 + shaped$lines$y2,
          lwd = shaped$lines$thickness * 96 / 72,
          col = ifelse(
            is.na(shaped$lines$color) | !nzchar(shaped$lines$color),
            glyphs$color[1],
            shaped$lines$color
          )
        )
      }
      if (nrow(shaped$boxes) != 0) {
        math_boxes[[i]] <- data.frame(
          id = x$shape$id[placeholder],
          x = x0 + shaped$boxes$x,
          y = y0 + shaped$boxes$y,
          width = shaped$boxes$width,
          height = shaped$boxes$height,
          fill = shaped$boxes$color
        )
      }
      ## Inline math in vertical text is set horizontally and rotated 90
      ## degrees like Latin runs (mixed = "rotate"): -90 for ttb, +90 for
      ## ttb-ltr (Mongolian convention, reading bottom-to-top)
      if (
        maths$inline[i] &&
          x$text$text_direction[maths$index[i]] %in% c("ttb", "ttb-ltr")
      ) {
        ccw <- x$text$text_direction[maths$index[i]] == "ttb-ltr"
        ## For +90 (ttb-ltr) the math start sits at the bottom of the
        ## reserved slot, like rotated Latin runs whose reading order is
        ## bottom-to-top; for -90 (ttb) it sits at the top. The cross-axis
        ## anchor is the column centreline, offset so the math's vertical
        ## midline lands on it
        math_w <- math_metrics$width[i]
        rot_x <- if (ccw) {
          ## The column centreline is the centre of the placeholder's cell
          col_c <- (x$shape$cell_x0[placeholder] +
            x$shape$cell_x1[placeholder]) /
            2
          x_anchor <- col_c +
            (math_metrics$height[i] - math_metrics$depth[i]) / 2
          function(x, y) x_anchor - (y - y0)
        } else {
          function(x, y) x0 + (y - y0)
        }
        rot_y <- if (ccw) {
          function(x, y) y0 - math_w + (x - x0)
        } else {
          function(x, y) y0 - (x - x0)
        }
        g <- math_glyphs[[i]]
        x_rot <- rot_x(g$x_offset, g$y_offset)
        g$y_offset <- rot_y(g$x_offset, g$y_offset)
        g$x_offset <- x_rot
        ## Rotate the cell boxes along with the glyphs
        cx0 <- rot_x(g$cell_x0, g$cell_y0)
        cy0 <- rot_y(g$cell_x0, g$cell_y0)
        cx1 <- rot_x(g$cell_x1, g$cell_y1)
        cy1 <- rot_y(g$cell_x1, g$cell_y1)
        g$cell_x0 <- pmin(cx0, cx1)
        g$cell_x1 <- pmax(cx0, cx1)
        g$cell_y0 <- pmin(cy0, cy1)
        g$cell_y1 <- pmax(cy0, cy1)
        g$rotation <- if (ccw) 90 else -90
        math_glyphs[[i]] <- g
        if (!is.null(math_lines[[i]])) {
          l <- math_lines[[i]]
          x0_rot <- rot_x(l$x0, l$y0)
          l$y0 <- rot_y(l$x0, l$y0)
          l$x0 <- x0_rot
          x1_rot <- rot_x(l$x1, l$y1)
          l$y1 <- rot_y(l$x1, l$y1)
          l$x1 <- x1_rot
          math_lines[[i]] <- l
        }
        if (!is.null(math_boxes[[i]])) {
          b <- math_boxes[[i]]
          x_rot <- rot_x(b$x, b$y)
          b$y <- if (ccw) {
            rot_y(b$x, b$y)
          } else {
            y0 - (b$x - x0 + b$width)
          }
          b$x <- if (ccw) x_rot - b$height else x_rot
          w <- b$width
          b$width <- b$height
          b$height <- w
          math_boxes[[i]] <- b
        }
      }
    }
    x$shape <- rbind(x$shape, inject(rbind(!!!math_glyphs)))
    x$math_lines <- inject(rbind(!!!math_lines))
    x$math_boxes <- inject(rbind(!!!math_boxes))
  } else {
    x$math_lines <- NULL
    x$math_boxes <- NULL
  }

  # Place images
  image_grobs <- lapply(seq_along(images$grobs), function(i) {
    ## Use the null-glyph to set left-bottom position of image
    glyph <- which(x$shape$string_id == images$index[i])
    grob <- images$grobs[[i]]
    descend <- x$shape$descender[glyph]
    if (x$text$text_direction[images$index[i]] %in% c("ttb", "ttb-ltr")) {
      ## In vertical text the image is centered on the column spine: the
      ## centre of the placeholder's cell across the main axis
      spine <- (x$shape$cell_x0[glyph] + x$shape$cell_x1[glyph]) / 2
      grob$vp$x <- unit(spine - as_bigpts(grob$vp$width) / 2, "bigpts")
      grob$vp$y <- unit(
        x$shape$y_offset[glyph] - as_bigpts(grob$vp$height),
        "bigpts"
      )
    } else {
      grob$vp$x <- unit(x$shape$x_offset[glyph], "bigpts")
      grob$vp$y <- unit(
        x$shape$y_offset[glyph] + descend + images$y_adjust[i],
        "bigpts"
      )
    }
    grob
  })
  ## Remove the null-glyphs from the shape
  x$shape <- x$shape[!x$shape$spacer, ]
  x$images <- list(
    id = x$text$id[images$index],
    grobs = image_grobs
  )
  # Write the bullet shape back so `bullets` reflects the final positioned
  # glyphs, not the stale constructor-time ones
  if (!is.null(x$bullets)) {
    bullet_rows <- x$shape$string_id > n_text_string_ids
    x$bullets$shape$shape <- x$shape[bullet_rows, , drop = FALSE]
    x$bullets$shape$shape$string_id <- x$bullets$shape$shape$string_id -
      n_text_string_ids
  }
  x
}

#' @export
makeContext.marquee_precalculated_grob <- function(x) x

#' @export
heightDetails.marquee_grob <- function(x) {
  x$full_height
}
#' @export
widthDetails.marquee_grob <- function(x) {
  x$full_width
}
#' @export
xDetails.marquee_grob <- function(x, theta) {
  xDetails(structure(x$bbox, class = "points"), theta)
}
#' @export
yDetails.marquee_grob <- function(x, theta) {
  yDetails(structure(x$bbox, class = "points"), theta)
}

#' @export
makeContent.marquee_grob <- function(x) {
  if (inherits(x, what = c("null"))) {
    # We may end here when creating precalculated grobs
    return(x)
  }

  has_glyphs <- isTRUE(grDevices::dev.capabilities("glyphs")$glyphs)

  # Create the font list of unique fonts
  if (has_glyphs && nrow(x$shape) > 0) {
    font_id <- paste0(x$shape$font_path, "&", x$shape$font_index)
    font_match <- match(font_id, unique(font_id))
    unique_font <- !duplicated(font_id)
    fonts <- Map(
      glyphFont,
      x$shape$font_path[unique_font],
      x$shape$font_index[unique_font],
      "",
      0,
      ""
    )
    fonts <- inject(glyphFontList(!!!fonts))
  }

  # We need to make a grob for each markdown text
  ## We do it this way around because some grobs may not contain glyphs
  indices <- vector("list", length(x$x))
  indices[sort(unique(x$shape$id))] <- split(seq_len(nrow(x$shape)), x$shape$id)

  grobs <- lapply(seq_along(x$x), function(grob) {
    ## Construct glyph grob
    i <- indices[[grob]]
    if (length(i) > 0) {
      if (has_glyphs) {
        glyphs <- glyphInfo(
          id = x$shape$index[i],
          x = x$shape$x_offset[i],
          y = x$shape$y_offset[i],
          font = font_match[i],
          size = x$shape$font_size[i],
          fontList = fonts,
          width = x$widths[grob],
          height = -x$heights[grob],
          hAnchor = glyphAnchor(0, "left"),
          vAnchor = glyphAnchor(0, "bottom"),
          col = x$shape$col[i],
          rot = x$shape$rotation[i]
        )
        outline <- outline_glyphs(glyphs, x$shape[i, , drop = FALSE])
        glyphs <- glyphGrob(
          glyphs,
          x = 0,
          y = 0,
          hjust = 0,
          vjust = 0
        )
        if (!is.null(outline)) {
          glyphs <- inject(grobTree(!!!outline, glyphs))
        }
      } else {
        glyphs <- systemfonts::glyph_outline(
          x$shape$index[i],
          x$shape$font_path[i],
          x$shape$font_index[i],
          x$shape$font_size[i]
        )
        need_bitmap <- i[attr(glyphs, "missing")]
        if (nrow(glyphs) == 0) {
          outline <- NULL
          glyphs <- nullGrob()
        } else {
          ## Rotate outlines around the glyph origin (no-op for rotation 0)
          rot <- x$shape$rotation[i][glyphs$glyph] * pi / 180
          if (any(rot != 0)) {
            cos_r <- cos(rot)
            sin_r <- sin(rot)
            rot_x <- glyphs$x * cos_r - glyphs$y * sin_r
            glyphs$y <- glyphs$x * sin_r + glyphs$y * cos_r
            glyphs$x <- rot_x
          }
          outline <- outline_polygon(glyphs, x$shape[i, , drop = FALSE])
          glyphs <- pathGrob(
            x = glyphs$x + x$shape$x_offset[i][glyphs$glyph],
            y = glyphs$y + x$shape$y_offset[i][glyphs$glyph],
            id = glyphs$contour,
            pathId = glyphs$glyph,
            default.units = "bigpts",
            gp = gpar(
              fill = x$shape$col[i][vctrs::vec_unique(glyphs$glyph)],
              col = NA
            )
          )
        }
        raster_glyphs <- systemfonts::glyph_raster(
          x$shape$index[need_bitmap],
          x$shape$font_path[need_bitmap],
          x$shape$font_index[need_bitmap],
          x$shape$font_size[need_bitmap],
          col = x$shape$col[need_bitmap]
        )
        raster_glyphs <- Map(
          function(glyph, x, y, rotation) {
            rotation <- rotation %% 360
            if (rotation == 0 || rotation %% 90 != 0) {
              ## NB: bitmap glyphs are only rotated for right angles
              return(systemfonts::glyph_raster_grob(
                glyph,
                x,
                y,
                interpolate = TRUE
              ))
            }
            rotate_raster_glyph(glyph, x, y, rotation)
          },
          glyph = raster_glyphs,
          x = x$shape$x_offset[need_bitmap],
          y = x$shape$y_offset[need_bitmap],
          rotation = x$shape$rotation[need_bitmap]
        )
        glyphs <- inject(grobTree(!!!outline, glyphs, !!!raster_glyphs))
      }
    } else {
      glyphs <- NULL
    }

    ## Construct span and block rects
    rects <- lapply(which(x$rects$id == grob), function(i) {
      ### If the same stroke is around all sides we can just apply it to the rect
      single_stroke <- !is.na(x$rects$col[i]) &&
        length(unique(c(
          x$rects$left[i],
          x$rects$right[i],
          x$rects$top[i],
          x$rects$bottom[i]
        ))) ==
          1
      grobs <- list()
      rect <- if (x$rects$r[i] == 0) {
        rectGrob(
          x = unit(x$rects$x[i], "bigpts"),
          y = unit(x$rects$y[i], "bigpts"),
          width = unit(x$rects$width[i], "bigpts"),
          height = unit(x$rects$height[i], "bigpts"),
          just = c(0, 1),
          gp = gpar(
            fill = x$rects$fill[[i]],
            col = if (single_stroke) x$rects$col[i] else NA,
            lwd = x$rects$left[i] * 2, #### We double it as we clip the outer part away
            lty = x$rects$lty[i]
          )
        )
      } else {
        roundrectGrob(
          x = unit(x$rects$x[i], "bigpts"),
          y = unit(x$rects$y[i], "bigpts"),
          width = unit(x$rects$width[i], "bigpts"),
          height = unit(x$rects$height[i], "bigpts"),
          r = unit(x$rects$r[i], "bigpts"),
          just = c(0, 1),
          gp = gpar(
            fill = x$rects$fill[[i]],
            col = if (single_stroke) x$rects$col[i] else NA,
            lwd = x$rects$left[i] * 2,
            lty = x$rects$lty[i]
          )
        )
      }
      ### If we need the fill or stroke of the rect, append it (we construct it no matter what as we need it for stroke clipping)
      if (
        single_stroke ||
          !(is.character(x$rects$fill[[i]]) && is.na(x$rects$fill[[i]]))
      ) {
        grobs <- append(grobs, list(rect))
      }
      ### If we don't have a single stroke we construct the sides individually
      if (!single_stroke) {
        if (x$rects$left[i] != 0) {
          grobs <- append(
            grobs,
            list(segmentsGrob(
              x0 = unit(x$rects$x[i], "bigpts"),
              y0 = unit(x$rects$y[i], "bigpts"),
              x1 = unit(x$rects$x[i], "bigpts"),
              y1 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
              gp = gpar(
                col = x$rects$col[i],
                lwd = x$rects$left[i] * 2,
                lineend = "square",
                lty = x$rects$lty[i]
              )
            ))
          )
        }
        if (x$rects$right[i] != 0) {
          grobs <- append(
            grobs,
            list(segmentsGrob(
              x0 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
              y0 = unit(x$rects$y[i], "bigpts"),
              x1 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
              y1 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
              gp = gpar(
                col = x$rects$col[i],
                lwd = x$rects$right[i] * 2,
                lineend = "square",
                lty = x$rects$lty[i]
              )
            ))
          )
        }
        if (x$rects$top[i] != 0) {
          grobs <- append(
            grobs,
            list(segmentsGrob(
              x0 = unit(x$rects$x[i], "bigpts"),
              y0 = unit(x$rects$y[i], "bigpts"),
              x1 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
              y1 = unit(x$rects$y[i], "bigpts"),
              gp = gpar(
                col = x$rects$col[i],
                lwd = x$rects$top[i] * 2,
                lineend = "square",
                lty = x$rects$lty[i]
              )
            ))
          )
        }
        if (x$rects$bottom[i] != 0) {
          grobs <- append(
            grobs,
            list(segmentsGrob(
              x0 = unit(x$rects$x[i], "bigpts"),
              y0 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
              x1 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
              y1 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
              gp = gpar(
                col = x$rects$col[i],
                lwd = x$rects$bottom[i] * 2,
                lineend = "square",
                lty = x$rects$lty[i]
              )
            ))
          )
        }
      }
      ### Combine it all in a single grob clipped to the bounds of the rect
      inject(grobTree(
        !!!grobs,
        vp = viewport(
          clip = if (utils::packageVersion("grid") < package_version("4.1.0")) {
            "on"
          } else {
            rect
          }
        )
      ))
    })
    ## Extract the relevant image grobs
    images <- lapply(
      which(x$images$id == grob),
      function(i) x$images$grobs[[i]]
    )
    ## Construct math boxes (colorbox backgrounds). Drawn below the glyphs
    box_id <- which(x$math_boxes$id == grob)
    math_boxes <- if (length(box_id) == 0) {
      NULL
    } else {
      rectGrob(
        x = unit(x$math_boxes$x[box_id], "bigpts"),
        y = unit(x$math_boxes$y[box_id], "bigpts"),
        width = unit(x$math_boxes$width[box_id], "bigpts"),
        height = unit(x$math_boxes$height[box_id], "bigpts"),
        just = c(0, 0),
        gp = gpar(fill = x$math_boxes$fill[box_id], col = NA)
      )
    }
    ## Construct math lines (fraction bars, radical overlines, etc.)
    math_line_id <- which(x$math_lines$id == grob)
    math_lines <- if (length(math_line_id) == 0) {
      NULL
    } else {
      segmentsGrob(
        x0 = unit(x$math_lines$x0[math_line_id], "bigpts"),
        y0 = unit(x$math_lines$y0[math_line_id], "bigpts"),
        x1 = unit(x$math_lines$x1[math_line_id], "bigpts"),
        y1 = unit(x$math_lines$y1[math_line_id], "bigpts"),
        gp = gpar(
          col = x$math_lines$col[math_line_id],
          lwd = x$math_lines$lwd[math_line_id],
          lineend = "butt"
        )
      )
    }
    ## Construct underline and strikethrough grob. Can be a single segments grob
    line_id <- which(x$lines$id == grob)
    lines <- if (length(line_id) == 0) {
      NULL
    } else {
      lines <- segmentsGrob(
        x0 = unit(x$lines$x0[line_id], "bigpts"),
        y0 = unit(x$lines$y0[line_id], "bigpts"),
        x1 = unit(x$lines$x1[line_id], "bigpts"),
        y1 = unit(x$lines$y1[line_id], "bigpts"),
        gp = gpar(
          col = x$lines$col[line_id],
          lwd = x$lines$lwd[line_id],
          lineend = "square"
        )
      )
    }
    ## Combine to a single grob, clipping at the constraint box if one is set
    content <- inject(
      grobTree(
        !!!rects,
        !!!images,
        math_boxes,
        glyphs,
        math_lines,
        lines
      )
    )
    if (isTRUE(x$clip[grob])) {
      clip_rect <- rectGrob(
        x = unit(-x$hjust[grob] * x$widths[grob], "bigpts"),
        y = unit(-x$vjust[grob] * x$heights[grob], "bigpts"),
        width = unit(x$widths[grob], "bigpts"),
        height = unit(x$heights[grob], "bigpts"),
        just = c(0, 0)
      )
      content <- grobTree(content, vp = viewport(clip = clip_rect))
    }
    grobTree(
      content,
      vp = viewport(
        x$x[grob],
        x$y[grob],
        just = c(0, 0),
        angle = x$angle[grob]
      )
    )
  })

  # Combine all separate texts into one grob
  if (length(x$images$id) == 0 && length(x$rects$id) == 0) {
    ## If there are no rects and images we can draw directly on the main surface
    children <- inject(gList(!!!grobs))
  } else {
    ## If rects/images exists they set their own clipping and we need to use a
    ## group
    children <- gList(groupGrob(inject(grobTree(!!!grobs))))
  }

  setChildren(x, children)
}

#' @export
makeContent.marquee_precalculated <- function(x) x

# Whether the installed textshaping supports math shaping. Resolved once at
# load time since getNamespaceExports() doesn't change during a session
# (barring a textshaping reinstall + reload, which requires an R restart to
# take full effect anyway)
math_support <- new_environment(list(supported = FALSE, shape_math = NULL))
on_load({
  math_support$supported <- "shape_math" %in%
    getNamespaceExports("textshaping")
  if (math_support$supported) {
    math_support$shape_math <- utils::getFromNamespace(
      "shape_math",
      "textshaping"
    )
  }
})

check_math_support <- function(call = caller_env()) {
  if (!math_support$supported) {
    cli::cli_abort(
      c(
        "Rendering math requires a version of textshaping that provides {.fn textshaping::shape_math}",
        i = "Please update to the development version of textshaping"
      ),
      call = call
    )
  }
}

# Shape each expression separately so that a single malformed expression
# doesn't bring down the whole text. Failures are returned as FALSE and the
# caller is expected to reinstate the literal text for these
shape_math_safe <- function(expr, family, size, inline, color) {
  shape_math <- math_support$shape_math
  Map(
    function(e, f, s, i, col) {
      tryCatch(
        shape_math(
          e,
          family = f,
          size = s,
          res = 72,
          inline = i,
          color = col
        ),
        error = function(err) {
          cli::cli_warn(c(
            "Failed to typeset math expression {.val {e}}",
            i = conditionMessage(err)
          ))
          FALSE
        }
      )
    },
    expr,
    family,
    size,
    inline,
    color
  )
}

collect_children <- function(elem, block_starts, block_ends, block_indent) {
  if (elem == block_ends[elem]) {
    children <- list()
  } else {
    children <- block_starts[
      block_starts > elem &
        block_starts <= block_ends[elem] &
        block_indent[block_starts] == block_indent[elem] + 1
    ]
    children <- lapply(
      children,
      collect_children,
      block_starts = block_starts,
      block_ends = block_ends,
      block_indent = block_indent
    )
  }
  list(
    element = elem,
    children = children
  )
}
get_first <- function(tree, background, border) {
  if (background[tree$element] || border[tree$element]) {
    tree$element
  } else if (length(tree$children) != 0) {
    c(tree$element, get_first(tree$children[[1]], background, border))
  } else {
    tree$element
  }
}
get_last <- function(tree, background, border) {
  if (background[tree$element] || border[tree$element]) {
    tree$element
  } else if (length(tree$children) != 0) {
    c(
      tree$element,
      get_last(tree$children[[length(tree$children)]], background, border)
    )
  } else {
    tree$element
  }
}
set_margins <- function(
  tree,
  margins,
  has_background,
  has_start,
  has_end,
  forced_margin = NULL
) {
  starts <- get_first(tree, has_background, has_start)
  margins$start[starts[1]] <- forced_margin %||% max(margins$start[starts])
  margins$start[starts[-1]] <- 0
  ends <- get_last(tree, has_background, has_end)
  margins$end[ends[1]] <- forced_margin %||% max(margins$end[ends])
  margins$end[ends[-1]] <- 0
  for (i in seq_along(tree$children)) {
    child <- tree$children[[i]]
    is_last <- i == length(tree$children)
    margins <- set_margins(
      child,
      margins,
      has_background,
      has_start,
      has_end,
      if (is_last) forced_margin else NULL
    )
  }
  for (i in seq_along(tree$children)[-1]) {
    margins$end[tree$children[[i - 1]]$element] <- max(
      margins$end[tree$children[[i - 1]]$element],
      margins$start[tree$children[[i]]$element]
    )
    margins$start[tree$children[[i]]$element] <- 0
  }
  margins
}

has_identical_background <- function(a, b, style) {
  mapply(
    function(a, b) {
      identical(style$block[a], style$block[b]) &&
        identical(style$background[[a]], style$background[[b]]) &&
        identical(style$border[a], style$border[b]) &&
        (is.na(style$border[a]) ||
          (identical(
            style$border_width_bottom[a],
            style$border_width_bottom[b]
          ) &&
            identical(style$border_width_top[a], style$border_width_top[b]) &&
            identical(style$border_width_left[a], style$border_width_left[b]) &&
            identical(
              style$border_width_right[a],
              style$border_width_right[b]
            )))
    },
    a = a,
    b = b
  )
}

add_inline_padding <- function(parsed) {
  ## Inline padding is along the span's own advance direction: padding_left
  ## at the flow start, padding_right at the flow end
  start_pad <- parsed$padding_left
  end_pad <- parsed$padding_right
  has_background <- which(
    parsed$block == c(0, parsed$block[seq_len(nrow(parsed) - 1)]) &
      (start_pad != 0 | end_pad != 0)
  )
  needs_padding <- parsed$block[has_background] !=
    parsed$block[has_background - 1] |
    !has_identical_background(has_background, has_background - 1, parsed)
  needs_padding <- has_background[needs_padding]

  if (length(needs_padding) == 0) {
    return(parsed)
  }

  padding <- data.frame(
    loc = c(needs_padding, parsed$ends[needs_padding]),
    width = c(
      start_pad[needs_padding],
      end_pad[needs_padding]
    ),
    offset = rep(c(0, 1), each = length(needs_padding))
  )
  padding <- unique(padding[padding$width != 0, ])
  padding <- padding[order(padding$loc), ]
  parsed$ends <- parsed$ends + rowSums(outer(parsed$ends, padding$loc, `>=`))
  n_reps <- vctrs::vec_count(c(seq_len(nrow(parsed)), padding$loc), "key")
  parsed <- parsed[rep(n_reps$key, n_reps$count), ]
  padding$loc <- padding$loc + seq_len(nrow(padding)) - 1 + padding$offset
  parsed$text[padding$loc] <- NA
  parsed$tracking[padding$loc] <- padding$width
  parsed
}

outline_glyphs <- function(glyph_info, shape) {
  if (NROW(glyph_info$glyphs) < 1) {
    return(NULL)
  }
  if (all(is.na(shape$outline))) {
    return(NULL)
  }

  id <- vctrs::vec_group_loc(shape[, c(
    "outline",
    "outline_width",
    "outline_type",
    "outline_join",
    "outline_mitre"
  )])
  id <- id[!is.na(id$key$outline), ]
  outlines <- vector("list", nrow(id))
  for (idx in seq_along(outlines)) {
    outline <- glyph_info
    outline$glyphs <- outline$glyphs[id$loc[[idx]], , drop = FALSE]
    outline_gp <- gpar(
      lwd = id$key$outline_width[idx] * 2,
      lty = id$key$outline_type[idx],
      col = id$key$outline[idx],
      linejoin = id$key$outline_join[idx],
      linemitre = id$key$outline_mitre[idx]
    )
    outline <- glyphGrob(outline, x = 0, y = 0, hjust = 0, vjust = 0)
    outline <- strokeGrob(outline, gp = outline_gp)
    outlines[[idx]] <- outline
  }
  outlines
}

# Shift a shape data frame (as returned by textshaping::shape_text()) along
# with its glyph cell boxes, so the two never go out of sync
## Physical (start, end) sides of the flow axis per direction: right/left
## for ttb, left/right for ttb-ltr, top/bottom otherwise
flow_sides <- function(flow) {
  switch(
    flow,
    ttb = c("right", "left"),
    "ttb-ltr" = c("left", "right"),
    c("top", "bottom")
  )
}

## x position of a vertical block's shaped-box origin within its content
## box. textshaping anchors the box at its flow-start corner: the right edge
## for ttb, the left edge for ttb-ltr
glyph_origin_x <- function(direction, left, width, own_w) {
  ifelse(direction == "ttb", left + width - own_w, left)
}

## x of the flow-start edge of a block's own content within its content box
flow_start_x <- function(direction, left, width, own_w) {
  ifelse(direction == "ttb", left + width - own_w, left + own_w)
}

shift_shape <- function(shape, dx = 0, dy = 0) {
  shape$x_offset <- shape$x_offset + dx
  shape$cell_x0 <- shape$cell_x0 + dx
  shape$cell_x1 <- shape$cell_x1 + dx
  shape$y_offset <- shape$y_offset + dy
  shape$cell_y0 <- shape$cell_y0 + dy
  shape$cell_y1 <- shape$cell_y1 + dy
  shape
}

# Rotate a bitmap glyph (nativeRaster from systemfonts::glyph_raster()) in
# counterclockwise 90° steps, updating the offset/size attributes so it lands
# where the rotated glyph origin demands, and build the rasterGrob
rotate_raster_glyph <- function(glyph, x, y, rotation) {
  offset <- attr(glyph, "offset") # c(top, left) corner relative to baseline origin
  size <- attr(glyph, "size") # c(height, width) in bigpts
  steps <- (rotation / 90) %% 4
  for (step in seq_len(steps)) {
    glyph <- rot90(glyph)
    offset <- c(offset[2] + size[2], -offset[1])
    size <- rev(size)
  }
  rasterGrob(
    glyph,
    x = x + offset[2],
    y = y + offset[1],
    just = c("left", "top"),
    width = size[2],
    height = size[1],
    default.units = "bigpts",
    interpolate = TRUE
  )
}

# Rotate a raster matrix 90° counterclockwise, preserving attributes such as
# class and channels. nativeRaster stores rows bottom-up, so the index
# rotation is mirrored compared to regular rasters
rot90 <- function(x) {
  attrs <- attributes(x)
  x <- if (inherits(x, "nativeRaster")) {
    t(x[nrow(x):1, , drop = FALSE])
  } else {
    t(x[, ncol(x):1, drop = FALSE])
  }
  attrs$dim <- dim(x)
  attrs$dimnames <- NULL
  attributes(x) <- attrs
  x
}

outline_polygon <- function(polygon, shape) {
  if (NROW(polygon) < 1) {
    return(NULL)
  }
  if (all(is.na(shape$outline))) {
    return(NULL)
  }
  polygon <- polygon[!is.na(shape$outline[polygon$glyph]), , drop = FALSE]
  if (NROW(polygon) < 1) {
    return(NULL)
  }
  i <- vctrs::vec_unique(polygon$glyph)
  list(pathGrob(
    x = polygon$x + shape$x_offset[polygon$glyph],
    y = polygon$y + shape$y_offset[polygon$glyph],
    id = polygon$contour,
    pathId = polygon$glyph,
    default.units = "bigpts",
    gp = gpar(
      fill = NA,
      lwd = shape$outline_width[i] * 2,
      lty = shape$outline_type[i],
      col = shape$outline[i],
      linejoin = shape$outline_join[i],
      linemitre = shape$outline_mitre[i]
    )
  ))
}
