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
#' @param x,y The location of the markdown text in the graphics. If numeric it
#' will be converted to units using `default.units`
#' @param width The width of each markdown text. If numeric it will be converted
#' to units using `default.units`. `NULL` is equivalent to the width of the
#' parent container. `NA` uses the width of the text as the full width of the
#' grob and will thus avoid any soft breaking of lines.
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
#' Underlines are placed 0.1em below the baseline of the text. Strikethrough are
#' placed 0.3em above the baseline. The width of the line is set to 0.075em. It
#' inherits the color of the text. No further styling is possible.
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
  x = 0,
  y = 1,
  width = NULL,
  default.units = "npc",
  hjust = "left",
  vjust = "top",
  angle = 0,
  vp = NULL,
  name = NULL
) {
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

  if (!is.unit(x)) x <- unit(x, default.units)
  if (!is.unit(y)) y <- unit(y, default.units)
  width <- width %||% unit(1, "npc")
  if (!is.unit(width)) width <- unit(width, default.units)

  # Parse input
  parsed <- if (!is_parsed(text)) marquee_parse(text, style, ignore_html) else
    text

  # Add spacing for inline padding
  parsed <- add_inline_padding(parsed)

  # Set bottom margin for tight list `li` elements to match the lineheight
  is_tight <- parsed$type == "li" & parsed$tight
  parsed$margin_top[is_tight] <- 0
  parsed$margin_bottom[is_tight] <- parsed$size[is_tight] *
    (parsed$lineheight[is_tight] - 1)

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

  # Determine location of blocks, their indent and location in overall parsed text
  bl <- rle(parsed$block)$lengths
  bs <- cumsum(c(0, bl[-length(bl)])) + 1
  blocks <- list(length = bl, start = bs, indent = parsed$indentation[bs])

  # Perform margin collapsing
  collapsed_margins <- list(
    top = parsed$margin_top,
    bottom = parsed$margin_bottom
  )
  has_background <- vapply(
    parsed$background,
    function(x) !(is.character(x) && is.na(x[1])),
    logical(1)
  )
  has_top <- !is.na(parsed$border) & parsed$border_size_top != 0
  has_bottom <- !is.na(parsed$border) & parsed$border_size_bottom != 0
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
      has_top,
      has_bottom
    )
  }
  # Body margin wins over any collapsing
  if (force_body_margin) {
    roots <- parsed$indentation == 1
    collapsed_margins$top[roots] <- parsed$margin_top[roots]
    collapsed_margins$bottom[roots] <- parsed$margin_bottom[roots]
  }
  parsed$margin_top <- collapsed_margins$top
  parsed$margin_bottom <- collapsed_margins$bottom

  # Handle bullets
  bullets <- place_bullets(
    parsed$type,
    parsed$indentation,
    parsed$block,
    parsed$text == "",
    parsed$ol_index,
    parsed$bullets
  )
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
    direction = parsed$text_direction[bullets$index]
  )
  ## Inherit color and id from the relevant block
  bullets$shape$shape$col <- parsed$color[bullets$index[
    bullets$shape$shape$metric_id
  ]]
  bullets$shape$shape$id <- parsed$id[bullets$index[
    bullets$shape$shape$metric_id
  ]]

  grob <- gTree(
    text = parsed,
    blocks = blocks,
    bullets = bullets,
    images = images,
    x = rep_along(text, x),
    y = rep_along(text, y),
    width = rep_along(text, width),
    hjust = rep_along(text, hjust),
    vjust = rep_along(text, vjust),
    angle = rep_along(text, angle),
    vp = vp,
    name = name,
    cl = "marquee_grob"
  )
  # Check if we can do all calculations upfront
  if (
    all(
      is.na(grob$width) |
        !unitType(absolute.size(grob$width)) %in% c("null", "sum", "min", "max")
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
    convertWidth(x$width, "bigpts", TRUE),
    convertHeight(x$width, "bigpts", TRUE)
  )

  # Calculate width of each block
  widths <- rep(NA, length(x$blocks$start))
  left_offset <- widths
  indent_stack <- rep(NA, max(x$blocks$indent))

  ## Iterate over blocks, calculate offset and width based on parent + margin/padding
  for (i in seq_along(x$blocks$start)) {
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
    ## Start with width
    if (is.null(vp) || unitType(absolute.size(vp$width)) == "null") {
      if (images$inline[i]) {
        ### No absolute size + inline. set width to 1em
        width <- x$text$size[images$index[i]]
        # if raster or svg we cap at height instead of width and let aspect ratio determine width
        if (inherits(images$grobs[[i]], "rastergrob")) {
          asp <- ncol(images$grobs[[i]]$raster) / nrow(images$grobs[[i]]$raster)
          width <- width * asp
        } else if (inherits(images$grobs[[i]], "svg_grob")) {
          asp <- images$grob[[i]]$asp
          width <- width * asp
        }
      } else {
        ### Not inline. Use block width
        width <- widths[x$text$block[images$index[i]]] %|% 0
      }
    } else {
      ### Grob has a width. Use that unless it exceeds the block width
      true_width <- convertWidth(vp$width, "bigpts", TRUE)
      width <- min(widths[x$text$block[images$index[i]]], true_width)
      scale <- width / true_width
    }
    ## Then height
    if (is.null(vp) || unitType(absolute.size(vp$height)) == "null") {
      if (inherits(images$grobs[[i]], "rastergrob")) {
        ### No size and rastergrob. Use aspect ratio of raster
        asp <- ncol(images$grobs[[i]]$raster) / nrow(images$grobs[[i]]$raster)
        height <- width / asp
      } else if (inherits(images$grobs[[i]], "svg_grob")) {
        asp <- images$grob[[i]]$asp
        height <- width / asp
      } else if (images$inline[i]) {
        ### Inline. We set a square size
        height <- width
      } else {
        ### Not inline. We use the aspect ratio from the style
        height <- width / x$text$img_asp[images$index[i]]
      }
    } else {
      ### Grob has width. We use that but scale it based on width downscaling
      height <- convertWidth(vp$height, "bigpts", TRUE) * scale
    }
    ## Set dimensions in size and tracking (this is how it is passed to textshaping)
    size[images$index[i]] <- height
    tracking[images$index[i]] <- width
    ## Create a parent viewport with the correct size
    outer_vp <- viewport(
      x = 0,
      y = 0,
      width = unit(width, "bigpts"),
      height = unit(height, "bigpts"),
      just = c(0, 0),
      clip = "off"
    )
    ## Use parent vp directly if no vp, otherwise wrap the grob
    if (is.null(vp)) {
      images$grobs[[i]]$vp <- outer_vp
    } else {
      images$grobs[[i]] <- grobTree(images$grobs[[i]], vp = outer_vp)
    }
  }

  # Shape text using widths
  shape <- textshaping::shape_text(
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
    vjust = 1,
    max_width = rep(
      convertWidth(unit(widths, "bigpts"), "inches", TRUE),
      x$blocks$length
    ),
    tracking = tracking,
    indent = convertWidth(unit(x$text$indent, "bigpts"), "inches", TRUE),
    hanging = convertWidth(unit(x$text$hanging, "bigpts"), "inches", TRUE),
    space_before = 0,
    space_after = 0,
    direction = x$text$text_direction
  )
  if (nrow(shape$shape) == 0) return(nullGrob())

  # If any widths are not defined we grab the text widths from the shaping and start again
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

  # Inherit color and id from parsed text
  idx <- shape$shape$string_id
  shape$shape$col <- x$text$color[idx]
  shape$shape$id <- x$text$id[idx]
  shape$shape$outline <- x$text$outline[idx]
  shape$shape$outline_size <- x$text$outline_size[idx]
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
      direction = ifelse(shape$metrics$ltr[bullet_blocks], "ltr", "rtl")
    )
    ## Inherit color and id from the relevant block
    idx <- x$bullets$index[bshape$shape$metric_id]
    bshape$shape$col <- x$text$color[idx]
    bshape$shape$id <- x$text$id[idx]
    bshape$shape$outline <- x$text$outline[idx]
    bshape$shape$outline_size <- x$text$outline_size[idx]
  }
  # make rtl bullets left-justified
  bltr <- bshape$metrics$ltr[bshape$shape$metric_id]
  bshape$shape$x_offset[!bltr] <- bshape$shape$x_offset[!bltr] +
    bshape$metrics$width[bshape$shape$metric_id[!bltr]]

  # Add sizebased offset
  bshape$shape$x_offset <- bshape$shape$x_offset +
    ifelse(bltr, -1, 1) * bshape$shape$font_size / 4

  # Position blocks underneath each other, calculate justification info, and position bullets
  for (i in seq_along(x$blocks$start)) {
    ## j is the location in x$text where style is given
    j <- x$blocks$start[i]
    indent <- x$blocks$indent[i]

    ## Position bullet if attached to this block
    bullet_ind <- which(i == bullet_blocks)
    if (length(bullet_ind) != 0) {
      ### Determine if bullet is higher than text at first line
      first <- which(shape$shape$metric_id == i)
      first <- first[
        shape$shape$y_offset[first] == max(shape$shape$y_offset[first])
      ]
      first <- first[
        if (shape$metrics$ltr[i]) which.min(shape$shape$glyph[first]) else
          which.max(shape$shape$glyph[first])
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

    next_indent <- if (i == length(x$blocks$start)) 1 else
      x$blocks$indent[i + 1]
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

  # Update offsets based on calculated width and heights
  shape$shape$x_offset <- shape$shape$x_offset +
    left_offset[shape$shape$metric_id]
  shape$shape$y_offset <- shape$shape$y_offset +
    top_offset[shape$shape$metric_id] -
    y_adjustment[shape$shape$metric_id]

  # Apply baseline shift
  shape$shape$y_offset <- shape$shape$y_offset +
    x$text$baseline[shape$shape$string_id]

  ## Do the same for bullets
  bshape$shape$x_offset <- bshape$shape$x_offset +
    left_offset[bullet_blocks[bshape$shape$metric_id]] +
    x_bullet_adjustment[bshape$shape$metric_id]
  bshape$shape$y_offset <- bshape$shape$y_offset +
    top_offset[bullet_blocks[bshape$shape$metric_id]] -
    y_bullet_adjustment[bshape$shape$metric_id]

  # Store info in object
  x$shape <- shape$shape
  widths <- widths +
    rowSums(x$text[
      x$blocks$start,
      c("padding_left", "padding_right", "margin_left", "margin_right")
    ])
  x$widths <- widths[x$blocks$indent == 1]
  x$heights <- heights[x$blocks$indent == 1]
  ## Adjust y so it is zero-justified
  x$shape$y_offset <- x$shape$y_offset + x$heights[x$shape$id]
  bshape$shape$y_offset <- bshape$shape$y_offset + x$heights[bshape$shape$id]
  top_offset <- top_offset + x$heights[x$text$id[x$blocks$start]]
  ## Combine main text with bullets
  bshape$shape$string_id <- bshape$shape$string_id + max(x$shape$string_id)
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
  x$shape$x_offset <- x$shape$x_offset - (x$widths * x$hjust)[x$shape$id]
  x$shape$y_offset <- x$shape$y_offset - (x$heights * x$vjust)[x$shape$id]
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
      (x$text$border_size_bottom > 0 |
        x$text$border_size_top > 0 |
        x$text$border_size_left > 0 |
        x$text$border_size_right > 0))

  ## Handle block backgrounds
  block_bg <- has_deco[x$blocks$start]
  block_rects <- list(
    id = x$text$id[x$blocks$start[block_bg]],
    x = left_offset[block_bg] - x$text$padding_left[x$blocks$start[block_bg]],
    y = top_offset[block_bg] + x$text$padding_top[x$blocks$start[block_bg]],
    width = widths[block_bg],
    height = heights[block_bg] -
      x$text$margin_top[x$blocks$start[block_bg]] -
      x$text$margin_bottom[x$blocks$start[block_bg]],
    fill = x$text$background[x$blocks$start[block_bg]],
    col = x$text$border[x$blocks$start[block_bg]],
    r = x$text$border_radius[x$blocks$start[block_bg]],
    left = x$text$border_size_left[x$blocks$start[block_bg]],
    right = x$text$border_size_right[x$blocks$start[block_bg]],
    top = x$text$border_size_top[x$blocks$start[block_bg]],
    bottom = x$text$border_size_bottom[x$blocks$start[block_bg]]
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

  span_rects <- lapply(span_bg, function(i) {
    ### Find the glyphs related to the span
    span_ind <- which(x$shape$string_id %in% i)
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
  })
  span_rects <- inject(rbind(!!!unlist(span_rects, recursive = FALSE)))

  ### Combine block and span rects
  x$rects <- list(
    id = c(block_rects$id, x$text$id[span_rects[, 1]]),
    x = c(
      block_rects$x,
      span_rects[, 2] - x$text$padding_left[span_rects[, 1]]
    ),
    y = c(block_rects$y, span_rects[, 3] + x$text$padding_top[span_rects[, 1]]),
    width = c(
      block_rects$width,
      span_rects[, 4] +
        x$text$padding_left[span_rects[, 1]] +
        x$text$padding_right[span_rects[, 1]]
    ),
    height = c(
      block_rects$height,
      span_rects[, 5] +
        x$text$padding_top[span_rects[, 1]] +
        x$text$padding_bottom[span_rects[, 1]]
    ),
    fill = c(block_rects$fill, x$text$background[span_rects[, 1]]),
    col = c(block_rects$col, x$text$border[span_rects[, 1]]),
    r = c(block_rects$r, x$text$border_radius[span_rects[, 1]]),
    left = c(block_rects$left, x$text$border_size_left[span_rects[, 1]]),
    right = c(block_rects$right, x$text$border_size_right[span_rects[, 1]]),
    top = c(block_rects$top, x$text$border_size_top[span_rects[, 1]]),
    bottom = c(block_rects$bottom, x$text$border_size_bottom[span_rects[, 1]])
  )

  ## Extract position of underline and strikethrough
  lines <- lapply(which(x$text$underline | x$text$strikethrough), function(i) {
    span_ind <- which(x$shape$string_id == i)
    ### Like span rects we split by line
    lapply(split(span_ind, x$shape$y_offset[span_ind]), function(j) {
      ### Line width and position is relative to size
      size <- x$shape$font_size[j[1]]
      mod <- c(-0.1, 0.3)[c(x$text$underline[i], x$text$strikethrough[i])] *
        size
      l <- c(
        i,
        min(x$shape$x_offset[j]),
        max(x$shape$x_offset[j] + x$shape$advance[j]),
        x$shape$y_offset[j[1]],
        size * 0.075
      )
      if (length(mod) == 2) {
        l <- rbind(l, l)
        l[, 4] <- l[, 4] + mod
      } else {
        l[4] <- l[4] + mod
      }
      l
    })
  })
  lines <- inject(rbind(!!!unlist(lines, recursive = FALSE)))
  x$lines <- list(
    id = x$text$id[lines[, 1]],
    x0 = lines[, 2],
    x1 = lines[, 3],
    y = lines[, 4],
    lwd = lines[, 5],
    col = x$text$color[lines[, 1]]
  )
  # Place images
  image_grobs <- lapply(seq_along(images$grobs), function(i) {
    ## Use the null-glyph to set left-bottom position of image
    glyph <- which(x$shape$string_id == images$index[i])
    grob <- images$grobs[[i]]
    grob$vp$x <- unit(x$shape$x_offset[glyph], "bigpts")
    descend <- x$shape$descender[glyph]
    grob$vp$y <- unit(
      x$shape$y_offset[glyph] + descend + images$y_adjust[i],
      "bigpts"
    )
    grob
  })
  ## Remove the null-glyphs from the shape
  x$shape <- x$shape[x$shape$font_path != "", ]
  x$images <- list(
    id = x$text$id[images$index],
    grobs = image_grobs
  )
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
          col = x$shape$col[i]
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
        outline <- outline_polygon(glyphs, x$shape[i, , drop = FALSE])
        glyphs <- if (nrow(glyphs) == 0) nullGrob() else
          pathGrob(
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
        raster_glyphs <- Map(
          function(glyph, x, y)
            systemfonts::glyph_raster_grob(glyph, x, y, interpolate = TRUE),
          glyph = raster_glyphs,
          x = x$shape$x_offset[need_bitmap],
          y = x$shape$y_offset[need_bitmap]
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
            lwd = x$rects$left[i] * 2 #### We double it as we clip the outer part away
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
            lwd = x$rects$left[i] * 2
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
                lineend = "square"
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
                lineend = "square"
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
                lineend = "square"
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
                lineend = "square"
              )
            ))
          )
        }
      }
      ### Combine it all in a single grob clipped to the bounds of the rect
      inject(grobTree(
        !!!grobs,
        vp = viewport(
          clip = if (utils::packageVersion("grid") < package_version("4.1.0"))
            "on" else rect
        )
      ))
    })
    ## Extract the relevant image grobs
    images <- lapply(
      which(x$images$id == grob),
      function(i) x$images$grobs[[i]]
    )
    ## Construct underline and strikethrough grob. Can be a single segments grob
    line_id <- which(x$lines$id == grob)
    lines <- if (length(line_id) == 0) {
      NULL
    } else {
      lines <- segmentsGrob(
        x0 = unit(x$lines$x0[line_id], "bigpts"),
        y0 = unit(x$lines$y[line_id], "bigpts"),
        x1 = unit(x$lines$x1[line_id], "bigpts"),
        y1 = unit(x$lines$y[line_id], "bigpts"),
        gp = gpar(
          col = x$lines$col[line_id],
          lwd = x$lines$lwd[line_id],
          lineend = "square"
        )
      )
    }
    ## Combine to a single grob
    inject(
      grobTree(
        !!!rects,
        !!!images,
        glyphs,
        lines,
        vp = viewport(
          x$x[grob],
          x$y[grob],
          just = c(0, 0),
          angle = x$angle[grob]
        )
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
set_margins <- function(tree, margins, has_background, has_top, has_bottom) {
  tops <- get_first(tree, has_background, has_top)
  margins$top[tops[1]] <- max(margins$top[tops])
  margins$top[tops[-1]] <- 0
  bottoms <- get_last(tree, has_background, has_top)
  margins$bottom[bottoms[1]] <- max(margins$bottom[bottoms])
  margins$bottom[bottoms[-1]] <- 0
  for (child in tree$children) {
    margins <- set_margins(child, margins, has_background, has_top, has_bottom)
  }
  for (i in seq_along(tree$children)[-1]) {
    margins$bottom[tree$children[[i - 1]]$element] <- max(
      margins$bottom[tree$children[[i - 1]]$element],
      margins$top[tree$children[[i]]$element]
    )
    margins$top[tree$children[[i]]$element] <- 0
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
            style$border_size_bottom[a],
            style$border_size_bottom[b]
          ) &&
            identical(style$border_size_top[a], style$border_size_top[b]) &&
            identical(style$border_size_left[a], style$border_size_left[b]) &&
            identical(style$border_size_right[a], style$border_size_right[b])))
    },
    a = a,
    b = b
  )
}

add_inline_padding <- function(parsed) {
  has_background <- which(
    parsed$block == c(0, parsed$block[seq_len(nrow(parsed) - 1)]) &
      parsed$padding_right != 0 &
      parsed$padding_left != 0
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
      parsed$padding_left[needs_padding],
      parsed$padding_right[needs_padding]
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
  id <- vctrs::vec_group_loc(shape[, c("outline", "outline_size")])
  id <- id[!is.na(id$key$outline), ]
  outlines <- vector("list", nrow(id))
  for (idx in seq_along(outlines)) {
    outline <- glyph_info
    outline$glyphs <- outline$glyphs[id$loc[[idx]], , drop = FALSE]
    outline_gp <- gpar(
      lwd = id$key$outline_size[idx] * 2,
      col = id$key$outline[idx]
    )
    outline <- glyphGrob(outline, x = 0, y = 0, hjust = 0, vjust = 0)
    outline <- strokeGrob(outline, gp = outline_gp)
    outlines[[idx]] <- outline
  }
  outlines
}

outline_polygon <- function(polygon, shape) {
  if (NROW(polygon) < 1) {
    return(NULL)
  }
  if (all(is.na(shape$outline))) {
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
      lwd = shape$outline_size[i] * 2,
      col = shape$outline[i]
    )
  ))
}
