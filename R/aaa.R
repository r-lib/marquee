bigpts_mod <- c(
  72/2.54, # cm
  72, # inches
  72/25.4, # mm
  72/72.27, # points
  12 * 72/72.27, # picas
  1, # bigpts
  (1238/1157) * (72/72.27), # dida
  12 * (1238/1157) * (72/72.27), # cicero
  (72/72.27) / 65536 # scaledpts
)
bigpts_mod_unit <- c(
  "cm",
  "inches",
  "mm",
  "points",
  "picas",
  "bigpts",
  "dida",
  "cicero",
  "scaledpts"
)

as_bigpts <- function(x, width = TRUE) {
  type <- grid::unitType(x)
  val <- as.numeric(x) * bigpts_mod[match(type, bigpts_mod_unit)]
  if (anyNA(val)) {
    cat("MUST CALL GRID\n")
    nas <- which(is.na(val))
    val[nas] <- if (width) convertWidth(x[nas], "bigpts", TRUE) else convertHeight(x[nas], "bigpts", TRUE)
  }
  val
}
