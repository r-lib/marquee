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
#' @param style A style set such as [classic_style()] that defines how the text
#' should be rendered
#' @param x,y The location of the markdown text in the graphics. If numeric it
#' will be converted to units using `default.units`
#' @param width The width of each markdown text. If numeric it will be converted
#' to units using `default.units`. `NULL` is equivalent to the width of the
#' parent container
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
#' **Bottom margin of last child elements**
#'
#' The bottom margin of the last child is automatically set to 0 to ensure that
#' margins doesn't accumulate as you exits nested elements.
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
#' background but will not modify the placement of glyph (i.e. having a left
#' padding will not move the first glyph further away from it's left neighbor).
#'
#' **Bullet position**
#'
#' Bullets are placed, right-aligned, 0.25em to the left of the first line in
#' the li element.
#'
#' **Border with border radius**
#'
#' If borders are not the same on all sides they are drawn one by one. In this
#' case the border radius is ignored.
#'
#' # Image rendering
#' The image tag can be used to place images. There are support for both png and
#' jpeg images. If the path instead names a grob, ggplot, or patchwork object
#' then this is rendered instead. If the file cannot be read, if it doesn't
#' exist, or if the path names an object that is not a grob, ggplot or patchwork,
#' a placeholder is rendered in it's place (black square with red cross).
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
marquee_grob <- function(text, style, x = 0, y = 1, width = NULL, default.units = "npc",
                         hjust = "left", vjust = "top", angle = 0, vp = NULL,
                         name = NULL) {
  parsed <- if (!is_parsed(text)) marquee_parse(text, style) else text

  # Set bottom margin for tight list `li` elements to match the lineheight
  is_tight <- parsed$type == "li" & parsed$tight
  parsed$margin_top[is_tight] <- 0
  parsed$margin_bottom[is_tight] <- parsed$size[is_tight] * (parsed$lineheight[is_tight] - 1)

  # Handle image tags
  images <- list(
    index = which(parsed$type == "img")
  )
  images$path <- parsed$text[images$index]
  parsed$text[images$index] <- NA
  ## Determine if tag is inline or on it's own
  images$inline <- vapply(images$index, function(i) {
    block <- parsed$block == parsed$block[i]
    sum(block) != 3 || any(nchar(parsed$text[block]) > 0, na.rm = TRUE)
  }, logical(1))
  ## If not inline, the prior p tag inherits the image styling so you can center it or add padding
  parsed[images$index[!images$inline] - 1, names(style[[1]][[1]])] <- parsed[images$index[!images$inline], names(style[[1]][[1]])]
  ## Convert all "paths" to grobs
  env <- caller_env()
  images$grobs <- images_as_grobs(images$path, env)

  # Handle bullets
  bullets <- place_bullets(
    parsed$type,
    parsed$indentation,
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
    align = "right",
    hjust = 1,
    vjust = 1
  )
  ## Inherit color and id from the relevant block
  bullets$shape$shape$col <- parsed$color[bullets$index[bullets$shape$shape$metric_id]]
  bullets$shape$shape$id <- parsed$id[bullets$index[bullets$shape$shape$metric_id]]

  if (!is.unit(x)) x <- unit(x, default.units)
  if (!is.unit(y)) y <- unit(y, default.units)
  width <- width %||% unit(1, "npc")
  if (!is.unit(width)) width <- unit(width, default.units)

  # Basic input checking
  if (is.character(hjust) && !all(hjust %in% c("left", "left-ink", "center", "center-ink", "right-ink", "right"))) {
    cli::cli_abort(c(
      "{.arg hjust} must be a valid justification",
      i = "Use either numerics or one of {.or {.val {c('left', 'left-ink', 'center', 'center-ink', 'right-ink', 'right')}}}"
    ))
  } else if (!is.numeric(hjust) && !is.character(hjust)) {
    cli::cli_abort("{.arg hjust} must either be numeric or a character vector")
  }
  if (is.character(vjust) && !all(vjust %in% c("bottom", "bottom-ink", "last-line", "center", "center-ink", "first-line", "top-ink", "top"))) {
    cli::cli_abort(c(
      "{.arg vjust} must be a valid justification",
      i = "Use either a numerics or one of {.or {.val {c('bottom', 'bottom-ink', 'last-line', 'center', 'center-ink', 'first-line', 'top-ink', 'top')}}}"
    ))
  } else if (!is.numeric(vjust) && !is.character(vjust)) {
    cli::cli_abort("{.arg vjust} must either be numeric or a character vector")
  }

  grob <- gTree(
    text = parsed, bullets = bullets, images = images, x = rep_along(text, x),
    y = rep_along(text, y), width = rep_along(text, width),
    hjust = rep_along(text, hjust), vjust = rep_along(text, vjust),
    angle = rep_along(text, angle), vp = vp, name = name, cl = "marquee"
  )
  # Check if we can do all calculations upfront
  if (all(is.na(grob$width) | unitType(absolute.size(grob$width)) != "null")) {
    grob <- makeContent.marquee(makeContext.marquee(grob))
    class(grob) <- c("marquee_precalculated", class(grob))
  }
  grob
}

#' @export
makeContext.marquee <- function(x) {
  # Everything is calculated in bigpts (1 inch == 72 bigpts)
  width <- convertWidth(x$width, "bigpts", TRUE)

  # Determine location of blocks, their indent and location in overall parsed text
  blocks <- rle(x$text$block)
  block_starts <- cumsum(c(0, blocks$lengths[-length(blocks$lengths)])) + 1
  block_id <- blocks$values
  block_indent <- x$text$indentation[block_starts]

  # Calculate width of each block
  widths <- rep(NA, length(block_id))
  left_offset <- widths
  indent_stack <- rep(NA, max(block_indent))

  ## Iterate over blocks, calculate offset and width based on parent + margin/padding
  for (i in seq_along(block_id)) {
    j <- block_starts[i]
    indent <- block_indent[i]
    if (indent == 1) {
      ### This is the body tag. start fresh
      widths[i] <- width[x$text$id[j]] - x$text$margin_left[j] - x$text$padding_left[j] - x$text$padding_right[j] - x$text$margin_right[j]
      left_offset[i] <- x$text$margin_left[j] + x$text$padding_left[j]
    } else {
      ### Find width and offset of parent and base calc on that
      widths[i] <- widths[indent_stack[indent - 1]] - x$text$margin_left[j] - x$text$padding_left[j] - x$text$padding_right[j] - x$text$margin_right[j]
      left_offset[i] <- left_offset[indent_stack[indent - 1]] + x$text$margin_left[j] + x$text$padding_left[j]
    }
    ### Set the index of the current indent to this
    indent_stack[indent] <- i
  }

  # Figure out width and height of images
  size <- x$text$size
  tracking <- x$text$tracking
  images <- x$images
  for (i in seq_along(images$index)) {
    vp <- images$grobs[[i]]$vp
    scale <- 1
    ## Start with width
    if (is.null(vp) || unitType(absolute.size(vp$width)) == "null") {
      if (images$inline[i]) {
        ### No absolute size + inline. set width to 1.2em
        width <- x$text$size[images$index[i]] * 1.2
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
        width <- widths[x$text$block[images$index[i]]]
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
    max_width = rep(convertWidth(unit(widths, "bigpts"), "inches", TRUE), blocks$lengths),
    tracking = tracking,
    indent = convertWidth(unit(x$text$indent, "bigpts"), "inches", TRUE),
    hanging = convertWidth(unit(x$text$hanging, "bigpts"), "inches", TRUE),
    space_before = 0,
    space_after = 0
  )

  # If any widths are not defined we grab the text widths from the shaping and start again
  if (anyNA(widths)) {
    added_width <- rowSums(x$text[block_starts, c("padding_left", "padding_right", "margin_left", "margin_right")])
    ## We go back from the most indented to the least and compound the widths
    for (i in rev(seq_len(max(block_indent)))) {
      blocks <- which(block_indent == i)
      widths[blocks] <- vapply(blocks, function(j) {
        k <- which(block_starts > block_starts[j] & block_starts <= x$text$ends[block_starts[j]])
        max(shape$metrics$width[j], widths[j], widths[k] + added_width[k], na.rm = TRUE)
      }, numeric(1))
    }
    if (anyNA(widths)) {
      cli::cli_abort("Could not resolve width of text")
    }
    x$width <- unit(widths[block_indent == 1], "bigpts")
    ## Restart evaluation
    return(makeContext.marquee(x))
  }

  # Inherit color and id from parsed text
  shape$shape$col <- x$text$color[shape$shape$string_id]
  shape$shape$id <- x$text$id[shape$shape$string_id]
  bshape <- x$bullets$shape

  # Init height structures
  heights <- rep(NA, length(block_id))
  top_offset <- heights
  cur_offset <- 0

  # Init justification info
  ink_left <- rep(NA, length(x$x))
  ink_right <- ink_left
  ink_top <- ink_left
  ink_bottom <- ink_left
  first_line <- ink_left
  last_line <- ink_left

  # Remove bottom-margin for blocks that are last child
  block_last <- block_is_last(block_indent, x$text$id[block_starts])
  margin_bottom <- x$text$margin_bottom[block_starts]
  margin_bottom[block_last] <- 0

  # Init bullet info
  bullet_blocks <- x$text$block[x$bullets$placement]
  y_adjustment <- rep(0, length(block_id))
  y_bullet_adjustment <- rep(0, length(bullet_blocks))
  x_bullet_adjustment <- rep(0, length(bullet_blocks))

  # Position blocks underneath each other, calculate justification info, and position bullets
  for (i in seq_along(block_id)) {
    ## j is the location in x$text where style is given
    j <- block_starts[i]
    indent <- block_indent[i]

    ## Position bullet if attached to this block
    bullet_ind <- which(i == bullet_blocks)
    if (length(bullet_ind) != 0) {
      ### Determine if bullet is higher than text at first line
      first <- match(i, shape$shape$metric_id)
      added_height <- shape$shape$y_offset[first] - bshape$shape$y_offset[match(bullet_ind, bshape$shape$metric_id)]
      if (added_height > 0) {
        #### If higher, adjust the height to make space and record adjustment for glyphs in block
        y_adjustment[i] <- added_height
        shape$metrics$height[i] <- shape$metrics$height[i] + added_height
      } else {
        #### Otherwise record negative adjustment to put it in line with the text in the list
        y_bullet_adjustment[bullet_ind] <- -1 * added_height
      }
      x_bullet_adjustment[bullet_ind] <- shape$shape$x_offset[first]
    }

    ## calculate y offset and height
    if (indent == 1) {
      ### Body tag, start again
      cur_offset <- 0
    }

    ### Record offset based on top padding and margin
    top_offset[i] <- cur_offset - x$text$margin_top[j] - x$text$padding_top[j]
    ### Init height as sum of top and bottom margin+padding
    heights[i] <- x$text$margin_top[j] + x$text$padding_top[j] + x$text$padding_bottom[j] + margin_bottom[i]
    ### If block has content of it's own, add it to the height
    if (x$text$text[j] != "" || blocks$lengths[i] != 1) {
      heights[i] <- heights[i] + shape$metrics$height[i]
    }

    ### Move down offset by height
    cur_offset <- cur_offset - heights[i]

    ### Increase height of parents
    for (k in seq_len(indent-1)) {
      heights[indent_stack[k]] <- heights[indent_stack[k]] + heights[i]
    }

    next_indent <- if (i == length(block_id)) 1 else block_indent[i + 1]
    if (indent > next_indent) {
      ### If exiting a block, add parent blocks bottom padding + margin to offset
      for (k in next_indent + seq_len(indent - next_indent) - 1) {
        cur_offset <- cur_offset - x$text$padding_bottom[block_starts[indent_stack[k]]] - margin_bottom[indent_stack[k]]
      }
    } else if (indent < next_indent) {
      ### If block has children, move up offset so it doesn't include bottom margin and padding
      cur_offset <- cur_offset + x$text$padding_bottom[j] + margin_bottom[i]
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
      ink_left[text_ind] <- min(ink_left[text_ind], left_offset[i] + shape$metrics$left_bearing[i], na.rm = TRUE)
      ink_right[text_ind] <- max(ink_right[text_ind], left_offset[i] + shape$metrics$width[i] - shape$metrics$right_bearing[i], na.rm = TRUE)
      ink_bottom[text_ind] <- min(ink_bottom[text_ind], top_offset[i] - shape$metrics$height[i] + shape$metrics$bottom_bearing[i], na.rm = TRUE)
      last_line[text_ind] <- min(last_line[text_ind], top_offset[i] + min(y_offsets), na.rm = TRUE)
    }
  }

  # Update offsets based on calculated width and heights
  shape$shape$x_offset <- shape$shape$x_offset + left_offset[shape$shape$metric_id]
  shape$shape$y_offset <- shape$shape$y_offset + top_offset[shape$shape$metric_id] - y_adjustment[shape$shape$metric_id]

  ## Do the same for bullets
  bshape$shape$x_offset <- bshape$shape$x_offset + left_offset[bullet_blocks[bshape$shape$metric_id]] - bshape$shape$font_size/4 + x_bullet_adjustment[bshape$shape$metric_id]
  bshape$shape$y_offset <- bshape$shape$y_offset + top_offset[bullet_blocks[bshape$shape$metric_id]] - y_bullet_adjustment[bshape$shape$metric_id]

  # Store info in object
  x$shape <- shape$shape
  x$widths <- widths[block_indent == 1]
  x$heights <- heights[block_indent == 1]
  ## Adjust y so it is zero-justified
  x$shape$y_offset <- x$shape$y_offset + x$heights[x$shape$id]
  bshape$shape$y_offset <- bshape$shape$y_offset + x$heights[bshape$shape$id]
  top_offset <- top_offset + x$heights[x$text$id[block_starts]]
  ## Combine main text with bullets
  bshape$shape$string_id <- bshape$shape$string_id + max(x$shape$string_id)
  x$shape <- rbind(x$shape, bshape$shape)

  first_line <- first_line + x$heights
  ink_top <- ink_top + x$heights
  ink_bottom <- ink_bottom + x$heights
  last_line <- last_line + x$heights
  if (is.character(x$hjust)) {
    x$hjust <- vapply(seq_along(x$hjust), function(i) {
      just <- x$hjust[i]
      w <- x$widths[i]
      switch(just,
        "left" = 0,
        "left-ink" = ink_left[i] / w,
        "center" = 0.5,
        "center-ink" = mean(c(ink_left[i], ink_right[i])) / w,
        "right-ink" = ink_right[i] / w,
        "right" = 1
      )
    }, numeric(1))
  }
  if (is.character(x$vjust)) {
    x$vjust <- vapply(seq_along(x$vjust), function(i) {
      just <- x$vjust[i]
      h <- x$heights[i]
      switch(just,
        "bottom" = 0,
        "bottom-ink" = ink_bottom[i] / h,
        "last-line" = last_line[i] / h,
        "center" = 0.5,
        "center-ink" = mean(c(ink_bottom[i], ink_top[i])) / h,
        "first-line" = first_line[i] / h,
        "top-ink" = ink_top[i] / h,
        "top" = 1
      )
    }, numeric(1))
  }

  # Perform justification
  x$shape$x_offset <- x$shape$x_offset - (x$widths * x$hjust)[x$shape$id]
  x$shape$y_offset <- x$shape$y_offset - (x$heights * x$vjust)[x$shape$id]
  left_offset <- left_offset - (x$widths * x$hjust)[x$text$id[block_starts]]
  top_offset <- top_offset - (x$heights * x$vjust)[x$text$id[block_starts]]

  # Precalculate full sizing of grob
  x$full_width <- max(x$x + (1 - x$hjust) * unit(x$widths, "bigpts")) - min(x$x - x$hjust * unit(x$widths, "bigpts"))
  x$full_height <- max(x$y + (1 - x$vjust) * unit(x$heights, "bigpts")) - min(x$y - x$vjust * unit(x$heights, "bigpts"))

  # Extract info about decoration (background, border, underline, etc)
  ## Figure out which blocks has backgrounds or borders
  has_deco <- !vapply(x$text$background, function(x) is.character(x) && is.na(x), logical(1)) |
    (!is.na(x$text$border) &
       (x$text$border_size_bottom > 0 |
          x$text$border_size_top > 0 |
          x$text$border_size_left > 0 |
          x$text$border_size_right > 0
        )
     )

  ## Handle block backgrounds
  block_bg <- has_deco[block_starts]
  block_rects <- list(
    id = x$text$id[block_starts[block_bg]],
    x = left_offset[block_bg] - x$text$padding_left[block_starts[block_bg]],
    y = top_offset[block_bg] + x$text$padding_top[block_starts[block_bg]],
    width = widths[block_bg] + x$text$padding_left[block_starts[block_bg]] + x$text$padding_right[block_starts[block_bg]],
    height = heights[block_bg] - x$text$margin_top[block_starts[block_bg]] - margin_bottom[block_bg],
    fill = x$text$background[block_starts[block_bg]],
    col = x$text$border[block_starts[block_bg]],
    r = x$text$border_radius[block_starts[block_bg]],
    left = x$text$border_size_left[block_starts[block_bg]],
    right = x$text$border_size_right[block_starts[block_bg]],
    top = x$text$border_size_top[block_starts[block_bg]],
    bottom = x$text$border_size_bottom[block_starts[block_bg]]
  )

  ## Handle span backgrounds
  span_bg <- which(has_deco & !seq_along(has_deco) %in% block_starts)
  ### Consecutive spans with same bg/border are merged
  span_bg_may_merge <- which(span_bg[-1] - span_bg[-length(span_bg)] == 1)
  span_bg <- as.list(span_bg)
  for (j in rev(span_bg_may_merge)) {
    i <- span_bg[[j]]
    if (identical(x$text$block[i], x$text$block[i+1]) &&
        identical(x$text$background[[i]], x$text$background[[i+1]]) &&
        identical(x$text$border[i], x$text$border[i+1]) &&
        is.na(x$text$border[i]) || (
          identical(x$text$border_size_bottom[i], x$text$border_size_bottom[i+1]) &&
          identical(x$text$border_size_top[i], x$text$border_size_top[i+1]) &&
          identical(x$text$border_size_left[i], x$text$border_size_left[i+1]) &&
          identical(x$text$border_size_right[i], x$text$border_size_right[i+1]
          ))) {
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
      c(i[1], left, top, max(x$shape$x_offset[j] + x$shape$advance[j]) - left, top - min(x$shape$y_offset[j] + x$shape$descender[j]))
    })
  })
  span_rects <- inject(rbind(!!!unlist(span_rects, recursive = FALSE)))

  ### Combine block and span rects
  x$rects <- list(
    id = c(block_rects$id, x$text$id[span_rects[,1]]),
    x = c(block_rects$x, span_rects[,2] - x$text$padding_left[span_rects[,1]]),
    y = c(block_rects$y, span_rects[,3] + x$text$padding_top[span_rects[,1]]),
    width = c(block_rects$width, span_rects[,4] + x$text$padding_left[span_rects[,1]] + x$text$padding_right[span_rects[,1]]),
    height = c(block_rects$height, span_rects[,5] + x$text$padding_top[span_rects[,1]] + x$text$padding_bottom[span_rects[,1]]),
    fill = c(block_rects$fill, x$text$background[span_rects[,1]]),
    col = c(block_rects$col, x$text$border[span_rects[,1]]),
    r = c(block_rects$r, x$text$border_radius[span_rects[,1]]),
    left = c(block_rects$left, x$text$border_size_left[span_rects[,1]]),
    right = c(block_rects$right, x$text$border_size_right[span_rects[,1]]),
    top = c(block_rects$top, x$text$border_size_top[span_rects[,1]]),
    bottom = c(block_rects$bottom, x$text$border_size_bottom[span_rects[,1]])
  )

  ## Extract position of underline and strikethrough
  lines <- lapply(which(x$text$underline | x$text$strikethrough), function(i) {
    span_ind <- which(x$shape$string_id == i)
    ### Like span rects we split by line
    lapply(split(span_ind, x$shape$y_offset[span_ind]), function(j) {
      ### Line width and position is relative to size
      size <- x$shape$font_size[j[1]]
      mod <- c(-0.1, 0.3)[c(x$text$underline[i], x$text$strikethrough[i])] * size
      l <- c(i, min(x$shape$x_offset[j]), max(x$shape$x_offset[j] + x$shape$advance[j]), x$shape$y_offset[j[1]], size * 0.075)
      if (length(mod) == 2) {
        l <- rbind(l, l)
        l[,4] <- l[,4] + mod
      } else {
        l[4] <- l[4] + mod
      }
      l
    })
  })
  lines <- inject(rbind(!!!unlist(lines, recursive = FALSE)))
  x$lines <- list(
    id = x$text$id[lines[,1]],
    x0 = lines[,2],
    x1 = lines[,3],
    y = lines[,4],
    lwd = lines[,5],
    col = x$text$color[lines[,1]]
  )
  # Place images
  image_grobs <- lapply(seq_along(images$grobs), function(i) {
    ## Use the null-glyph to set left-bottom position of image
    glyph <- which(x$shape$string_id == images$index[i])
    grob <- images$grobs[[i]]
    grob$vp$x <- unit(x$shape$x_offset[glyph], "bigpts")
    descend <- 0
    if (images$inline[i]) {
      ## If inline we move baseline down to lowest descender of the line
      descend <- min(x$shape$descender[x$shape$y_offset == x$shape$y_offset[glyph]])
    }
    grob$vp$y <- unit(x$shape$y_offset[glyph] + descend, "bigpts")
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
makeContext.marquee_precalculated <- function(x) x

#' @export
heightDetails.textboxgrob <- function(x) {
  x$full_height
}
#' @export
widthDetails.textboxgrob <- function(x) {
  x$full_width
}

#' @export
makeContent.marquee <- function(x) {
  # Create the font list of unique fonts
  font_id <- paste0(x$shape$font_path, "&", x$shape$font_index)
  font_match <- match(font_id, unique(font_id))
  unique_font <- !duplicated(font_id)
  fonts <- Map(glyphFont, x$shape$font_path[unique_font], x$shape$font_index[unique_font], "", 0, "")
  fonts <- inject(glyphFontList(!!!fonts))

  # We need to make a grob for each markdown text
  indices <- split(seq_len(nrow(x$shape)), x$shape$id)

  grobs <- lapply(seq_along(indices), function(grob) {
    ## Construct glyph grob
    i <- indices[[grob]]
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
    glyphs <- glyphGrob(
      glyphs,
      x = 0,
      y = 0,
      hjust = 0,
      vjust = 0
    )
    ## Construct span and block rects
    rects <- lapply(which(x$rects$id == grob), function(i) {
      ### If the same stroke is around all sides we can just apply it to the rect
      single_stroke <- !is.na(x$rects$col[i]) && length(unique(c(x$rects$left[i], x$rects$right[i], x$rects$top[i], x$rects$bottom[i]))) == 1
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
      if (single_stroke || !(is.character(x$rects$fill[[i]]) && is.na(x$rects$fill[[i]]))) {
        grobs <- append(grobs, list(rect))
      }
      ### If we don't have a single stroke we construct the sides individually
      if (!single_stroke) {
        if (x$rects$left[i] != 0) {
          grobs <- append(grobs, list(segmentsGrob(
            x0 = unit(x$rects$x[i], "bigpts"),
            y0 = unit(x$rects$y[i], "bigpts"),
            x1 = unit(x$rects$x[i], "bigpts"),
            y1 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
            gp = gpar(
              col = x$rects$col[i],
              lwd = x$rects$left[i] * 2,
              lineend = "square"
            )
          )))
        }
        if (x$rects$right[i] != 0) {
          grobs <- append(grobs, list(segmentsGrob(
            x0 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
            y0 = unit(x$rects$y[i], "bigpts"),
            x1 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
            y1 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
            gp = gpar(
              col = x$rects$col[i],
              lwd = x$rects$right[i] * 2,
              lineend = "square"
            )
          )))
        }
        if (x$rects$top[i] != 0) {
          grobs <- append(grobs, list(segmentsGrob(
            x0 = unit(x$rects$x[i], "bigpts"),
            y0 = unit(x$rects$y[i], "bigpts"),
            x1 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
            y1 = unit(x$rects$y[i], "bigpts"),
            gp = gpar(
              col = x$rects$col[i],
              lwd = x$rects$top[i] * 2,
              lineend = "square"
            )
          )))
        }
        if (x$rects$bottom[i] != 0) {
          grobs <- append(grobs, list(segmentsGrob(
            x0 = unit(x$rects$x[i], "bigpts"),
            y0 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
            x1 = unit(x$rects$x[i] + x$rects$width[i], "bigpts"),
            y1 = unit(x$rects$y[i] - x$rects$height[i], "bigpts"),
            gp = gpar(
              col = x$rects$col[i],
              lwd = x$rects$bottom[i] * 2,
              lineend = "square"
            )
          )))
        }
      }
      ### Combine it all in a single grob clipped to the bounds of the rect
      inject(grobTree(!!!grobs, vp = viewport(clip = rect)))
    })
    ## Extract the relevant image grobs
    images <- lapply(which(x$images$id == grob), function(i) x$images$grobs[[i]])
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
      grobTree(!!!rects, !!!images, glyphs, lines, vp = viewport(x$x[grob], x$y[grob], just = c(0, 0), angle = x$angle[grob], clip = "off"))
    )
  })

  setChildren(x, inject(gList(!!!grobs)))
}

#' @export
makeContent.marquee_precalculated <- function(x) x
