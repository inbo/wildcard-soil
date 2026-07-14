
#' Classify soil texture from clay, silt, and sand fractions
#'
#' Wrapper around \code{\link[soiltexture]{TT.points.in.classes}} that
#' normalises the particle-size fractions to 100, handles missing values,
#' and returns human-readable texture class labels.
#'
#' Rows with exactly one missing fraction are gap-filled by inference
#' (the missing value is set to \code{100 - sum(other two)}). Rows with
#' two or three missing fractions, or where gap-filling would produce a
#' negative value, are left unclassified (\code{NA}).
#'
#' Points falling exactly on a class boundary (which
#' \code{TT.points.in.classes} would otherwise return as \code{NA}) are
#' assigned to the first matching class in the lookup table of the chosen
#' classification system. Zero fractions are permitted (e.g. sand = 0).
#'
#' @param data A data frame containing at least the columns named in
#'   \code{css.names}. Mass percentages are assumed to follow the
#'   particle size boundaries corresponding with the classification system
#'   (e.g., 0-2-50-2000 µm for USDA)
#' @param css.names Character vector of length 3 giving the column names
#'   for clay, silt, and sand (in that order).
#'   Default: \code{c("clay", "silt", "sand")}.
#' @param class.sys Character string identifying the texture classification
#'   system passed to \code{\link[soiltexture]{TT.points.in.classes}}.
#'   Default: \code{"USDA.TT"}. Full-name labels in \code{texture_class}
#'   are only available for \code{"USDA.TT"}; other systems return the raw
#'   class code.
#' @param text.tol Numeric tolerance (fraction, not percentage) used by
#'   \code{\link[soiltexture]{TT.points.in.classes}} on the sum of the 3
#'   particle size classes. The real sum of the 3 particle size classes in
#'   'tri.data' should be \code{>= 100 (1-text.tol)} OR
#'   \code{<= 100 (1+text.tol)}.
#'
#' @return The input data frame \code{data} with three columns added or
#'   overwritten:
#'   \describe{
#'     \item{\code{clay}, \code{silt}, \code{sand}}{Normalised fractions
#'       (sum to 100)}
#'     \item{\code{texture_class}}{Full texture class label for
#'       \code{"USDA.TT"} (e.g. \code{"Silty clay loam"}), or the raw
#'       class code for other systems; \code{NA} for unclassifiable rows.}
#'   }
#'
#' @importFrom soiltexture TT.normalise.sum TT.points.in.classes
#' @export
classify_texture <- function(data,
                             css.names = c("clay", "silt", "sand"),
                             class.sys = "USDA.TT",
                             text.tol = 0.1) {

  stopifnot(require("soiltexture"))

  tri <- as.data.frame(data[, css.names])

  # Count NAs per row
  na_count <- rowSums(is.na(tri))

  # Gap-fill rows with exactly one NA: infer as 100 - sum(other two)
  one_na <- na_count == 1
  if (any(one_na)) {
    which_na <- apply(tri[one_na, , drop = FALSE], 1,
                    function(row) which(is.na(row)))
    tri[cbind(which(one_na), as.integer(which_na))] <-
      100 - rowSums(tri[one_na, , drop = FALSE], na.rm = TRUE)
  }

  # Classifiable:
  # all cases except: 2-3 NAs, or gap-fill produced negative value
  classifiable <- !(na_count >= 2 |
    (one_na & rowSums(tri, na.rm = TRUE) > 100))

  # Normalise classifiable rows to 100
  tri_norm <- tri
  if (any(classifiable)) {
    tri_norm[classifiable, ] <- round(
      soiltexture::TT.normalise.sum(
        tri.data = tri[classifiable, , drop = FALSE],
        css.names = css.names,
        text.sum = 100
      ), 4) # Round to avoid floating point edge cases
  }

  # Classify
  # Using logical matrix of class membership (in order to handle border cases)
  class_matrix <- soiltexture::TT.points.in.classes(
    tri.data = tri_norm[classifiable, , drop = FALSE],
    class.sys = class.sys,
    css.names = css.names,
    PiC.type = "l",  # logical matrix, not collapsed string
    text.tol = text.tol,
    tri.pos.tst = FALSE # to allow zero fractions
  )

  # Border points: pick first matching class; no match or unclassifiable: NA
  raw_class <- apply(class_matrix, 1, function(row) {
    hits <- names(which(row))
    if (length(hits) == 0) NA_character_ else hits[1]
  })

  # USDA label lookup
  usda_labels <- c(
    Cl     = "Clay",
    SiCl   = "Silty clay",
    SaCl   = "Sandy clay",
    ClLo   = "Clay loam",
    SiClLo = "Silty clay loam",
    SaClLo = "Sandy clay loam",
    Lo     = "Loam",
    SiLo   = "Silty loam",
    SaLo   = "Sandy loam",
    Si     = "Silt",
    LoSa   = "Loamy sand",
    Sa     = "Sand"
  )

  data[, css.names] <- tri_norm
  data$texture_class <- NA
  data$texture_class[classifiable] <- if (class.sys == "USDA.TT") {
    usda_labels[raw_class]
  } else {
    raw_class
  }

  return(data)

}

