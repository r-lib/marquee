# Tests for vertical (tategaki) typesetting. Most tests are structural: they
# assert relationships between glyph coordinates (packing direction, decoration
# sides, flow order) that hold regardless of platform font metrics. Visual
# snapshot tests at the end complement them.

skip_if_not(
  "mixed" %in% names(formals(textshaping::shape_text)),
  "textshaping without vertical text support"
)

# Build a grob and run the full layout/content pipeline. Uses explicit units
# so constraints resolve identically with and without a graphics device.
v_grob <- function(text, style = vertical_style(), ...) {
  grid::makeContent(grid::makeContext(marquee_grob(text, style, ...)))
}

# Row index of a text span in the parsed data
span_id <- function(grob, text) which(grob$text$text == text)

test_that("ttb text sets in top-to-bottom columns packing right-to-left", {
  g <- v_grob(
    "縦書きのテストです。長めに書いて列を増やします。",
    width = NA,
    height = grid::unit(50, "bigpts")
  )

  # Multiple columns
  cols <- sort(unique(g$shape$x_offset))
  expect_gt(length(cols), 1)

  # First column is the rightmost
  first <- span_id(g, "縦書きのテストです。長めに書いて列を増やします。")
  first_col_x <- g$shape$x_offset[g$shape$string_id == first][1]
  expect_equal(first_col_x, max(cols))

  # Within a column x is constant and y decreases
  first_col <- g$shape[g$shape$x_offset == max(cols), ]
  expect_true(all(diff(first_col$y_offset) < 0))

  # Text starts at the top (largest y)
  expect_equal(first_col$y_offset[1], max(g$shape$y_offset))
})

test_that("ttb-ltr mirrors ttb: columns pack left-to-right", {
  g <- v_grob(
    "縦書きのテストです。長めに書いて列を増やします。",
    vertical_style("ttb-ltr"),
    width = NA,
    height = grid::unit(50, "bigpts")
  )

  cols <- sort(unique(g$shape$x_offset))
  expect_gt(length(cols), 1)

  # First column is the leftmost
  first <- span_id(g, "縦書きのテストです。長めに書いて列を増やします。")
  first_col_x <- g$shape$x_offset[g$shape$string_id == first][1]
  expect_equal(first_col_x, min(cols))

  # Still top-to-bottom within a column
  first_col <- g$shape[g$shape$x_offset == min(cols), ]
  expect_true(all(diff(first_col$y_offset) < 0))
})

test_that("block flow is right-to-left in ttb and left-to-right in ttb-ltr", {
  txt <- "一段目です。\n\n二段目です。"
  id1 <- 2 # first paragraph (row 1 is the empty markdown root)

  g <- v_grob(
    txt,
    vertical_style("ttb"),
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  x1 <- range(g$shape$x_offset[g$shape$string_id == id1])
  x2 <- range(g$shape$x_offset[g$shape$string_id == id1 + 1])
  # Second block sits entirely to the left of the first
  expect_lt(x2[2], x1[1])

  gl <- v_grob(
    txt,
    vertical_style("ttb-ltr"),
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  x1l <- range(gl$shape$x_offset[gl$shape$string_id == id1])
  x2l <- range(gl$shape$x_offset[gl$shape$string_id == id1 + 1])
  # Second block sits entirely to the right of the first
  expect_gt(x2l[1], x1l[2])
})

test_that("mixed = 'rotate' rotates horizontal-only runs", {
  g <- v_grob("日本語ABC文字", width = NA, height = grid::unit(200, "bigpts"))
  rotated <- g$shape[g$shape$rotation != 0, ]
  expect_gt(nrow(rotated), 0)
  expect_equal(unique(rotated$rotation), -90)

  # Rotated glyphs stay in the column of the surrounding text (their origin
  # is centered within the column, so allow a font size of slack)
  col_x <- unique(g$shape$x_offset[g$shape$rotation == 0])
  expect_true(all(abs(rotated$x_offset - col_x) < max(g$shape$font_size)))
})

test_that("mixed = 'tate' keeps short digit runs upright", {
  g <- v_grob("昭和12年です", width = NA, height = grid::unit(200, "bigpts"))
  expect_equal(unique(g$shape$rotation), 0)
})

test_that("tate2/tate3 cap digit-run combination", {
  capped <- function(text, m) {
    v_grob(
      paste0("昭和", text, "年"),
      modify_style(vertical_style(), "base", mixed = m),
      width = NA,
      height = grid::unit(200, "bigpts")
    )
  }
  # Runs at or below the cap stay upright, longer runs rotate
  expect_equal(unique(capped("12", "tate2")$shape$rotation), 0)
  expect_true(any(capped("123", "tate2")$shape$rotation == -90))
  expect_equal(unique(capped("123", "tate3")$shape$rotation), 0)
  expect_true(any(capped("1234", "tate3")$shape$rotation == -90))
  # Uncapped tate still combines up to 4 digits
  expect_equal(unique(capped("1234", "tate")$shape$rotation), 0)
})

test_that("kinsoku settings shape without error", {
  for (k in c("strict", "loose", "none")) {
    style <- modify_style(vertical_style(), "base", kinsoku = k)
    expect_no_error(v_grob(
      "句読点、のテスト。改行の確認をします。",
      style,
      width = NA,
      height = grid::unit(60, "bigpts")
    ))
  }
})

test_that("underline is bousen: right of column in ttb, left in ttb-ltr", {
  g <- v_grob(
    "{.u 傍線}です",
    modify_style(vertical_style(), "u", underline = TRUE),
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  col_x <- unique(g$shape$x_offset)
  size <- max(g$shape$font_size)
  expect_equal(unname(g$lines$x0), unname(g$lines$x1)) # vertical segment
  expect_gt(g$lines$x0, col_x)
  expect_lt(g$lines$x0, col_x + 1.5 * size)

  # The line spans the em boxes of the underlined glyphs
  u <- span_id(g, "傍線")
  u_glyphs <- g$shape$string_id == u
  expect_equal(
    unname(g$lines$y0),
    max(g$shape$y_offset[u_glyphs] + g$shape$ascender[u_glyphs])
  )
  expect_equal(
    unname(g$lines$y1),
    min(g$shape$y_offset[u_glyphs] + g$shape$descender[u_glyphs])
  )

  gl <- v_grob(
    "{.u 傍線}です",
    modify_style(vertical_style("ttb-ltr"), "u", underline = TRUE),
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  col_x_l <- unique(gl$shape$x_offset)
  expect_equal(unname(gl$lines$x0), unname(gl$lines$x1))
  expect_lt(gl$lines$x0, col_x_l)
  expect_gt(gl$lines$x0, col_x_l - 1.5 * size)
})

test_that("strikethrough is centered on the column", {
  g <- v_grob(
    "{.s 打消し}です",
    modify_style(vertical_style(), "s", strikethrough = TRUE),
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  col_x <- unique(g$shape$x_offset)
  size <- max(g$shape$font_size)
  expect_equal(unname(g$lines$x0), unname(g$lines$x1))
  expect_gt(g$lines$x0, col_x)
  expect_lt(g$lines$x0, col_x + size)

  # The line spans the em boxes of the struck glyphs
  s <- span_id(g, "打消し")
  s_glyphs <- g$shape$string_id == s
  expect_equal(
    unname(g$lines$y0),
    max(g$shape$y_offset[s_glyphs] + g$shape$ascender[s_glyphs])
  )
  expect_equal(
    unname(g$lines$y1),
    min(g$shape$y_offset[s_glyphs] + g$shape$descender[s_glyphs])
  )
})

test_that("rotated runs are decorated as if unrotated", {
  # Latin runs rotated into vertical text keep their horizontal decoration
  # geometry: background spans the em box measured from the run's baseline,
  # underline sits below the baseline, strikethrough at baseline + 0.3em
  for (dir in c("ttb", "ttb-ltr")) {
    g <- v_grob(
      "縦{.lat abc}書",
      modify_style(
        vertical_style(dir),
        "lat",
        underline = TRUE,
        strikethrough = TRUE,
        background = "grey90"
      ),
      width = NA,
      height = grid::unit(200, "bigpts")
    )
    lat <- span_id(g, "abc")
    run <- g$shape[g$shape$string_id == lat & g$shape$rotation != 0, ]
    expect_gt(nrow(run), 0)
    rot <- sign(run$rotation[1])
    expect_equal(rot, if (dir == "ttb") -1 else 1)
    base <- run$x_offset[1]
    expect_equal(run$x_offset, rep(base, nrow(run)))

    # Background spans [baseline + desc, baseline + asc] mirrored by sign
    lo <- base + min(if (rot < 0) run$descender else -run$ascender)
    hi <- base + max(if (rot < 0) run$ascender else -run$descender)
    expect_equal(unname(g$rects$x), lo)
    expect_equal(unname(g$rects$x + g$rects$width), hi)

    # Lines span exactly the run's advance cells
    cell_hi <- max(
      if (rot < 0) {
        run$y_offset
      } else {
        run$y_offset + run$advance
      }
    )
    cell_lo <- min(
      if (rot < 0) {
        run$y_offset - run$advance
      } else {
        run$y_offset
      }
    )

    info <- systemfonts::font_info(
      path = run$font_path[1],
      index = run$font_index[1],
      size = run$font_size[1],
      res = 600
    )
    upos <- info$underline_pos * 72 / 600
    # -90: underline left of baseline, strike right; +90: mirrored
    u_x <- base - rot * upos
    s_x <- base - rot * 0.3 * run$font_size[1]
    line_x <- sort(unique(g$lines$x0))
    expect_equal(line_x, sort(c(u_x, s_x)))
    expect_equal(sort(unique(g$lines$y0)), cell_hi)
    expect_equal(sort(unique(g$lines$y1)), cell_lo)
  }
})

test_that("padded spans decorate continuously across padding spacers", {
  for (dir in c("ttb", "ttb-ltr")) {
    g <- v_grob(
      "縦{.lat abc}書",
      modify_style(
        vertical_style(dir),
        "lat",
        underline = TRUE,
        background = "grey90",
        padding = trbl(4, 8, 4, 8)
      ),
      width = NA,
      height = grid::unit(200, "bigpts")
    )
    # Padding segments are decorated at the same position as the run
    expect_equal(unique(unname(g$lines$x0)), g$lines$x0[1])
    # Segments tile the padded extent without gaps
    ord <- order(g$lines$y0, decreasing = TRUE)
    expect_equal(g$lines$y0[ord][-1], g$lines$y1[ord][-length(ord)])

    # Horizontal padding is included in the background rect through the
    # spacer glyphs, cross-axis padding through the em-box sides
    lat <- span_id(g, "abc")
    run <- g$shape[g$shape$string_id == lat & g$shape$rotation != 0, ]
    rot <- sign(run$rotation[1])
    base <- run$x_offset[1]
    lo <- base + min(if (rot < 0) run$descender else -run$ascender)
    hi <- base + max(if (rot < 0) run$ascender else -run$descender)
    pad_asc <- 4
    pad_desc <- 4
    expect_equal(
      unname(g$rects$x),
      if (rot < 0) lo - pad_desc else lo - pad_asc
    )
    expect_equal(
      unname(g$rects$x + g$rects$width),
      if (rot < 0) hi + pad_asc else hi + pad_desc
    )
  }
})

test_that("span background rects are split per column", {
  g <- v_grob(
    "{.bg 背景のある長めのテキストです}続き",
    modify_style(vertical_style(), "bg", background = "grey90"),
    width = NA,
    height = grid::unit(60, "bigpts")
  )
  bg <- span_id(g, "背景のある長めのテキストです")
  bg_cols <- unique(g$shape$x_offset[g$shape$string_id == bg])
  expect_gt(length(bg_cols), 1)
  # One rect per column the span touches
  expect_length(g$rects$x, length(bg_cols))
})

test_that("bullets hang at the column head in vertical lists", {
  g <- v_grob(
    "- 項目一\n- 項目二",
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  expect_equal(g$bullets$bullet, c("・", "・"))

  bullet <- g$bullets$shape$shape
  text <- g$shape
  size <- max(text$font_size)

  # Bullet x is centered on a text column
  item_cols <- unique(text$x_offset)
  near_col <- vapply(
    bullet$x_offset,
    function(x) any(abs(x - item_cols) < size),
    logical(1)
  )
  expect_true(all(near_col))

  # Bullet sits above the first glyph of its column
  first_glyph_y <- tapply(text$y_offset, text$string_id, max)
  expect_true(all(bullet$y_offset > min(first_glyph_y) - size))
})

test_that("emphasis marks are placed right of column in ttb, left in ttb-ltr", {
  g <- v_grob("*強調*テスト", width = NA, height = grid::unit(200, "bigpts"))
  em <- g$shape[g$shape$string_id == span_id(g, "強調"), ]
  sizes <- sort(unique(em$font_size))
  expect_length(sizes, 2) # half-size marks + full-size characters
  marks <- em[em$font_size == sizes[1], ]
  chars <- em[em$font_size == sizes[2], ]
  expect_equal(nrow(marks), nrow(chars))
  expect_true(all(marks$x_offset > max(chars$x_offset)))

  gl <- v_grob(
    "*強調*テスト",
    vertical_style("ttb-ltr"),
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  em_l <- gl$shape[gl$shape$string_id == span_id(gl, "強調"), ]
  sizes_l <- sort(unique(em_l$font_size))
  marks_l <- em_l[em_l$font_size == sizes_l[1], ]
  chars_l <- em_l[em_l$font_size == sizes_l[2], ]
  expect_true(all(marks_l$x_offset < min(chars_l$x_offset)))
})

test_that("emphasis marks work in horizontal text (above)", {
  style <- modify_style(classic_style(), "em", emphasis = "●")
  g <- v_grob("a *b* c", style, width = NA, height = NA)
  em <- g$shape[g$shape$string_id == span_id(g, "b"), ]
  expect_equal(nrow(em), 2)
  mark <- em[em$font_size == min(em$font_size), ]
  char <- em[em$font_size == max(em$font_size), ]
  expect_gt(mark$y_offset, char$y_offset)
})

test_that("horizontal blocks nest inside vertical flow", {
  g <- v_grob(
    "縦書きの段落。\n\n    code line\n\n再び縦書き。",
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  cb <- g$shape[g$shape$string_id == which(g$text$type == "cb")[1], ]
  # Horizontal text: glyphs advance in x at constant y
  expect_equal(round(cb$y_offset, 3), rep(round(cb$y_offset[1], 3), nrow(cb)))
  expect_true(all(diff(cb$x_offset) >= 0))
})

test_that("vertical blocks nest inside horizontal flow", {
  style <- classic_style() |>
    modify_style("qb", text_direction = "ttb")
  # Narrow width forces multi-glyph columns in the nested vertical block
  g <- v_grob(
    "Horizontal intro.\n\n> 引用です。\n\nHorizontal outro.",
    style,
    width = grid::unit(60, "bigpts"),
    height = NA
  )
  bq <- g$shape[g$shape$string_id == span_id(g, "引用です。"), ]
  # Multiple columns
  expect_gt(length(unique(bq$x_offset)), 1)
  # Within a column, glyphs advance in -y
  col1 <- bq[bq$x_offset == bq$x_offset[1], ]
  expect_gt(nrow(col1), 1)
  expect_true(all(diff(col1$y_offset) < 0))
})

test_that("auto sizing resolves per direction", {
  # Content-sized block axis: a short text stays narrow
  g <- v_grob("テスト。", width = NA, height = grid::unit(200, "bigpts"))
  expect_lt(as.numeric(g$full_width), 50)

  # Constrained height wraps columns at the constraint
  g2 <- v_grob(
    "縦書きのテストです。長めに書いて列を増やします。",
    width = NA,
    height = grid::unit(50, "bigpts")
  )
  col_spread <- tapply(
    g2$shape$y_offset,
    g2$shape$x_offset,
    function(y) max(y) - min(y)
  )
  expect_true(all(col_spread <= 50 + max(g2$shape$font_size)))
})

test_that("ttb defaults anchor at the flow-start corner", {
  g <- marquee_grob("テスト", vertical_style())
  expect_equal(as.numeric(g$x), 1)
  expect_equal(g$hjust, "right")

  gh <- marquee_grob("test", classic_style())
  expect_equal(as.numeric(gh$x), 0)
  expect_equal(gh$hjust, "left")

  # Mixed root directions in a single vector resolve per text
  gm <- marquee_grob(
    c("テスト", "test"),
    c(vertical_style(), classic_style())
  )
  expect_equal(as.numeric(gm$x), c(1, 0))
  expect_equal(gm$hjust, c("right", "left"))
})

test_that("ol bullets auto-detect and format number systems", {
  # Han numerals auto-detected from CJK content in vertical style
  g <- v_grob(
    "1. 最初\n2. 次\n3. 最後",
    width = NA,
    height = grid::unit(200, "bigpts")
  )
  expect_equal(g$bullets$bullet, c("一、", "二、", "三、"))

  # Explicit templates work in any style
  kanji <- modify_style(classic_style(), "ol", bullets = "一、")
  g2 <- v_grob("1. first\n2. second", kanji, width = NA, height = NA)
  expect_equal(g2$bullets$bullet, c("一、", "二、"))

  circled <- modify_style(classic_style(), "ol", bullets = "①")
  g3 <- v_grob("1. a\n2. b\n3. c", circled, width = NA, height = NA)
  expect_equal(g3$bullets$bullet, c("①", "②", "③"))

  roman <- modify_style(classic_style(), "ol", bullets = "Ⅰ.")
  g4 <- v_grob("1. a\n2. b\n3. c", roman, width = NA, height = NA)
  expect_equal(g4$bullets$bullet, c("I.", "II.", "III."))

  # Default template is arabic for latin content
  g5 <- v_grob("1. first\n2. second", classic_style(), width = NA, height = NA)
  expect_equal(g5$bullets$bullet, c("1.", "2."))
})

test_that("number system helpers convert correctly", {
  to_kanji <- getFromNamespace("to_kanji", "marquee")
  to_roman <- getFromNamespace("to_roman", "marquee")
  to_circled <- getFromNamespace("to_circled", "marquee")
  to_parenthesized <- getFromNamespace("to_parenthesized", "marquee")
  detect <- getFromNamespace("detect_number_system", "marquee")

  expect_equal(to_kanji(1:3), c("一", "二", "三"))
  expect_equal(to_kanji(11), "十一")
  expect_equal(to_roman(c(1, 4, 9)), c("I", "IV", "IX"))
  expect_equal(to_circled(1), "①")
  expect_equal(to_circled(50), "㊿")
  expect_equal(to_parenthesized(1), "⑴")
  expect_equal(to_parenthesized(20), "⒇")

  expect_equal(detect("項目"), "一")
  expect_equal(detect("العربية"), "١")
  expect_equal(detect("รายการ"), "๑")
  expect_equal(detect("सूची"), "१")
  expect_equal(detect("hello"), "1")
})

test_that("vertical showcase renders match snapshots", {
  skip_on_os("windows")
  skip_on_os("linux")

  showcase <- "# 縦書き\n\n縦書きの*本文*です。{.u 傍線}も引けます。\n\n- 項目一\n- 項目二\n\n1. 最初\n2. 次\n\n> 引用文です。\n\n    y <- 1\n"

  for (dir in c("ttb", "ttb-ltr")) {
    file <- tempfile()
    ragg::agg_png(file, width = 800, height = 500)
    grid::grid.draw(marquee_grob(
      showcase,
      vertical_style(dir),
      width = grid::unit(7, "in"),
      height = grid::unit(4.5, "in")
    ))
    dev.off()
    expect_snapshot_file(file, paste0("showcase_", dir, ".png"))
  }
})
