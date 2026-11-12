# Marquee-aware string interpolation

If you want to create your markdown programmatically you'd probably want
to use some sort of string interpolation such as `glue()`. However, the
custom span syntax of marquee interferes with the standard interpolation
syntax of glue. This function let's you use both together.

## Usage

``` r
marquee_glue(
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
)

marquee_glue_data(
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
)
```

## Arguments

- ...:

  \[`expressions`\]  
  Unnamed arguments are taken to be expression string(s) to format.
  Multiple inputs are concatenated together before formatting. Named
  arguments are taken to be temporary variables available for
  substitution.

  For `glue_data()`, elements in `...` override the values in `.x`.

- .sep:

  \[`character(1)`: ‘""’\]  
  Separator used to separate elements.

- .envir:

  \[`environment`:
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)\]  
  Environment to evaluate each expression in. Expressions are evaluated
  from left to right. If `.x` is an environment, the expressions are
  evaluated in that environment and `.envir` is ignored. If `NULL` is
  passed, it is equivalent to
  [`emptyenv()`](https://rdrr.io/r/base/environment.html).

- .open:

  \[`character(1)`: ‘\\’\]  
  The opening delimiter. Doubling the full delimiter escapes it.

- .close:

  \[`character(1)`: ‘\\’\]  
  The closing delimiter. Doubling the full delimiter escapes it.

- .na:

  \[`character(1)`: ‘NA’\]  
  Value to replace `NA` values with. If `NULL` missing values are
  propagated, that is an `NA` result will cause `NA` output. Otherwise
  the value is replaced by the value of `.na`.

- .null:

  \[`character(1)`: ‘character()’\]  
  Value to replace NULL values with. If
  [`character()`](https://rdrr.io/r/base/character.html) whole output is
  [`character()`](https://rdrr.io/r/base/character.html). If `NULL` all
  NULL values are dropped (as in
  [`paste0()`](https://rdrr.io/r/base/paste.html)). Otherwise the value
  is replaced by the value of `.null`.

- .comment:

  \[`character(1)`: ‘#’\]  
  Value to use as the comment character.

- .literal:

  \[`boolean(1)`: ‘FALSE’\]  
  Whether to treat single or double quotes, backticks, and comments as
  regular characters (vs. as syntactic elements), when parsing the
  expression string. Setting `.literal = TRUE` probably only makes sense
  in combination with a custom `.transformer`, as is the case with
  `glue_col()`. Regard this argument (especially, its name) as
  experimental.

- .transformer:

  \[`function`\]  
  A function taking two arguments, `text` and `envir`, where `text` is
  the unparsed string inside the glue block and `envir` is the execution
  environment. A `.transformer` lets you modify a glue block before,
  during, or after evaluation, allowing you to create your own custom
  `glue()`-like functions. See `vignette("transformers")` for examples.

- .trim:

  \[`logical(1)`: ‘TRUE’\]  
  Whether to trim the input template with
  [`trim()`](https://glue.tidyverse.org/reference/trim.html) or not.

- .x:

  \[`listish`\]  
  An environment, list, or data frame used to lookup values.

## Value

A character vector

## Details

If you choose a different set of delimiters than `"{"` and `"}"` for the
interpolation the functions will call the equivalent glue functions
directly. However, if you keep the defaults, the functions will use a
custom transformer that will make sure to keep the marquee custom span
notation. You can both interpolate the content of the span, as well as
the span class (see examples)

## Examples

``` r
# standard use
red_text <- "this text will be red"
marquee_glue("This will be black and {.red {red_text}}!")
#> This will be black and {.red this text will be red}!

# if the span is not valid it will be treated as standard glue interpolation
try(
  marquee_glue("This will be black and {.red}!")
)
#> Error : Failed to evaluate glue component {.red}
#> Caused by error:
#> ! object '.red' not found

# You can interpolate the tag name as well
col <- "green"
marquee_glue("This will be black and {.{col} this text will be {col}}!")
#> This will be black and {.green this text will be green}!

# Tag name interpolation must follow a `.` or a `#` as these identify the
# bracket pair as a custom span class
col <- ".yellow"
# This is not what you want probably
marquee_glue("This will be black and {{col} this text will be {col}}!")
#> This will be black and {col} this text will be .yellow}!

# Tag interpolation should also interpolate the full tag and be followed by
# a space in order to be valid
part <- "l"
marquee_glue("This will be black and {.ye{part}low this text will be {col}}!")
#> This will be black and {.ye{part}low this text will be .yellow}!
try(
  marquee_glue("This will be black and {.{part}avender this text will be {col}}!")
)
#> Error in .transformer(expr, env) : 
#>   Malformed marquee interpolation block. When interpolating a tag
#> name it must be followed by a space
```
