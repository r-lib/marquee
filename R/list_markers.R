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
      suffix <- "\u3001"
    }
  }
  paste0(prefix, format_number(n, system), suffix)
}

is_number_char <- function(char) {
  char == "_" ||
    grepl("^\\p{Nd}$", char, perl = TRUE) ||
    char %in%
      strsplit(
        "\u3007\u96F6\u4E00\u4E8C\u4E09\u56DB\u4E94\u516D\u4E03\u516B\u4E5D\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168\u2169\u216A\u216B\u2170\u2171\u2172\u2173\u2174\u2175\u2176\u2177\u2178\u2179\u217A\u217BIi",
        "",
        fixed = TRUE
      )[[1]] ||
    (char >= "\u2460" & char <= "\u2473") ||
    (char >= "\u3251" & char <= "\u325F") ||
    (char >= "\u32B1" & char <= "\u32BF") ||
    (char >= "\u2474" & char <= "\u2487")
}

detect_number_system <- function(content) {
  if (is_cjk(content)) {
    return("\u4E00")
  }
  if (
    grepl("[\u0600-\u06FF\u0750-\u077F\u08A0-\u08FF]", content, perl = TRUE)
  ) {
    return("\u0661")
  }
  if (grepl("[\u0E00-\u0E7F]", content, perl = TRUE)) {
    return("\u0E51")
  }
  if (grepl("[\u0900-\u097F]", content, perl = TRUE)) {
    return("\u0967")
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
    system %in%
      strsplit(
        "\u3007\u96F6\u4E00\u4E8C\u4E09\u56DB\u4E94\u516D\u4E03\u516B\u4E5D",
        "",
        fixed = TRUE
      )[[1]]
  ) {
    to_kanji(n)
  } else if (system %in% c("\u2160", "I", "\u2170", "i")) {
    if (system %in% c("\u2170", "i")) tolower(to_roman(n)) else to_roman(n)
  } else if (system == "\u2460") {
    to_circled(n)
  } else if (system == "\u2474") {
    to_parenthesized(n)
  } else {
    as.character(n)
  }
}

to_circled <- function(n) {
  if (n <= 20) {
    intToUtf8(utf8ToInt("\u2460") + n - 1)
  } else if (n <= 35) {
    intToUtf8(utf8ToInt("\u3251") + n - 21)
  } else if (n <= 50) {
    intToUtf8(utf8ToInt("\u32B1") + n - 36)
  } else {
    paste0("(", n, ")")
  }
}

to_parenthesized <- function(n) {
  if (n <= 20) {
    intToUtf8(utf8ToInt("\u2474") + n - 1)
  } else {
    paste0("(", n, ")")
  }
}

# Whether the text contains Han ideographs or kana
is_cjk <- function(text) {
  grepl(
    "[\u3040-\u30FF\u3400-\u4DBF\u4E00-\u9FFF\uF900-\uFAFF]",
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
  digits <- c(
    "\u3007",
    "\u4E00",
    "\u4E8C",
    "\u4E09",
    "\u56DB",
    "\u4E94",
    "\u516D",
    "\u4E03",
    "\u516B",
    "\u4E5D"
  )
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
          "\u767E"
        } else if (hundreds > 1) {
          paste0(digits[hundreds + 1], "\u767E")
        },
        if (tens == 1) {
          "\u5341"
        } else if (tens > 1) {
          paste0(digits[tens + 1], "\u5341")
        },
        if (ones > 0) digits[ones + 1]
      )
    },
    character(1)
  )
}
