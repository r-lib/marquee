#' Format ordered-list bullets
#'
#' Ordered list bullets are auto-numbered with arabic numerals (`"1."`)
#' during parsing. This function applies richer numbering based on the
#' `bullets` element of the `ol` style, which acts as a template: the first
#' digit-like character in the template identifies the numbering system and
#' is replaced by the number; everything around it is kept as prefix/suffix.
#' `"_"` selects a system based on the script of the list's content (Han
#' numerals for CJK text, arabic otherwise), making `"_."` the effective
#' default. Supported digit characters:
#'
#' * Any Unicode decimal digit (e.g. `"1"`, `"١"`, `"๑"`, `"１"`): positional
#' numbering using that digit's zero point
#' * Han numerals (`"一"`)
#' * Roman numerals (`"Ⅰ"`, `"ⅰ"`, `"I"`, `"i"`)
#' * Circled numbers (`"①"`, up to 50)
#' * Parenthesized numbers (`"⑴"`, up to 20, composed beyond)
#'
#' @noRd
format_ol_bullets <- function(bullets, parsed) {
  for (i in seq_along(bullets$bullet)) {
    ol <- parent_tag(bullets$index[i], "ol", parsed)
    if (is.na(ol)) {
      next
    }
    n <- suppressWarnings(as.integer(sub("[^0-9].*$", "", bullets$bullet[i])))
    if (is.na(n)) {
      next
    }
    template <- ol_template(parsed$bullets[[ol]], ol, parsed)
    content <- paste(parsed$text[ol:parsed$ends[ol]], collapse = "")
    bullets$bullet[i] <- apply_number_template(template, n, content)
  }
  bullets
}

# Pick the template for a list, cycling by ol nesting depth
ol_template <- function(templates, ol, parsed) {
  depth <- 0
  parent <- parent_tag(ol, "ol", parsed)
  while (!is.na(parent)) {
    depth <- depth + 1
    parent <- parent_tag(parent, "ol", parsed)
  }
  templates[depth %% length(templates) + 1]
}

apply_number_template <- function(template, n, content) {
  chars <- strsplit(template, "", fixed = TRUE)[[1]]
  digit <- match(TRUE, vapply(chars, is_number_char, logical(1)))
  if (is.na(digit)) {
    ### No digit character: fully automatic numbering with the template as
    ### suffix (preserves the default "1." style when bullets are dots)
    prefix <- ""
    suffix <- if (template %in% marquee_bullets) {
      "."
    } else {
      template
    }
    system <- "auto"
  } else {
    prefix <- paste0(chars[seq_len(digit - 1)], collapse = "")
    suffix <- paste0(chars[-seq_len(digit)], collapse = "")
    system <- chars[[digit]]
  }
  if (system == "auto" || system == "_") {
    system <- detect_number_system(content)
    if (system != "1" && is.na(digit)) {
      ### Script-appropriate suffix for auto-detected CJK
      suffix <- "、"
    }
  }
  paste0(prefix, format_number(n, system), suffix)
}

is_number_char <- function(char) {
  char == "_" ||
    grepl("^\\p{Nd}$", char, perl = TRUE) ||
    char %in%
      strsplit(
        "〇零一二三四五六七八九ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪⅫⅰⅱⅲⅳⅴⅵⅶⅷⅸⅹⅺⅻIi",
        "",
        fixed = TRUE
      )[[1]] ||
    (char >= "①" & char <= "⑳") ||
    (char >= "㉑" & char <= "㉟") ||
    (char >= "㊱" & char <= "㊿") ||
    (char >= "⑴" & char <= "⒇")
}

detect_number_system <- function(content) {
  if (is_cjk(content)) {
    return("一")
  }
  if (grepl("[؀-ۿݐ-ݿࢠ-ࣿ]", content, perl = TRUE)) {
    return("١")
  }
  if (grepl("[฀-๿]", content, perl = TRUE)) {
    return("๑")
  }
  if (grepl("[ऀ-ॿ]", content, perl = TRUE)) {
    return("१")
  }
  "1"
}

format_number <- function(n, system) {
  if (grepl("^\\p{Nd}$", system, perl = TRUE)) {
    ### Positional digits. The template digit is "1" of its kind, so its zero
    ### is one codepoint less
    zero <- utf8ToInt(system) - 1L
    digits <- utf8ToInt(as.character(n)) - utf8ToInt("0")
    intToUtf8(zero + digits)
  } else if (
    system %in% strsplit("〇零一二三四五六七八九", "", fixed = TRUE)[[1]]
  ) {
    to_kanji(n)
  } else if (system %in% c("Ⅰ", "I", "ⅰ", "i")) {
    if (system %in% c("ⅰ", "i")) tolower(to_roman(n)) else to_roman(n)
  } else if (system == "①") {
    to_circled(n)
  } else if (system == "⑴") {
    to_parenthesized(n)
  } else {
    as.character(n)
  }
}

to_circled <- function(n) {
  if (n <= 20) {
    intToUtf8(utf8ToInt("①") + n - 1)
  } else if (n <= 35) {
    intToUtf8(utf8ToInt("㉑") + n - 21)
  } else if (n <= 50) {
    intToUtf8(utf8ToInt("㊱") + n - 36)
  } else {
    paste0("(", n, ")")
  }
}

to_parenthesized <- function(n) {
  if (n <= 20) {
    intToUtf8(utf8ToInt("⑴") + n - 1)
  } else {
    paste0("(", n, ")")
  }
}

# Whether the text contains Han ideographs or kana
is_cjk <- function(text) {
  grepl(
    "[぀-ヿ㐀-䶿一-鿿豈-﫿]",
    text,
    perl = TRUE
  )
}

to_roman <- function(n) {
  numerals <- c(
    M = 1000,
    CM = 900,
    D = 500,
    CD = 400,
    C = 100,
    XC = 90,
    L = 50,
    XL = 40,
    X = 10,
    IX = 9,
    V = 5,
    IV = 4,
    I = 1
  )
  vapply(
    n,
    function(n) {
      if (is.na(n) || n < 1 || n > 3999) {
        return(as.character(n))
      }
      out <- ""
      for (num in names(numerals)) {
        while (n >= numerals[[num]]) {
          out <- paste0(out, num)
          n <- n - numerals[[num]]
        }
      }
      out
    },
    character(1)
  )
}

# Find the row of the enclosing block tag of the given type
parent_tag <- function(row, type, parsed) {
  indent <- parsed$indentation[row]
  candidates <- which(
    parsed$type == type &
      parsed$indentation == indent - 1 &
      seq_len(nrow(parsed)) < row
  )
  if (length(candidates) == 0) {
    return(NA_integer_)
  }
  candidates[length(candidates)]
}

# Convert integers to kanji numerals. Only 1-999 is supported; anything else
# (including NA and non-positive numbers) falls back to arabic numerals
to_kanji <- function(n) {
  digits <- c("〇", "一", "二", "三", "四", "五", "六", "七", "八", "九")
  vapply(
    n,
    function(n) {
      if (is.na(n) || n < 1 || n > 999) {
        return(as.character(n))
      }
      hundreds <- n %/% 100
      tens <- (n %% 100) %/% 10
      ones <- n %% 10
      paste0(
        if (hundreds == 1) {
          "百"
        } else if (hundreds > 1) {
          paste0(digits[hundreds + 1], "百")
        },
        if (tens == 1) {
          "十"
        } else if (tens > 1) {
          paste0(digits[tens + 1], "十")
        },
        if (ones > 0) digits[ones + 1]
      )
    },
    character(1)
  )
}
