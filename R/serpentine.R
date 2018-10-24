#' Perform serpentine sorts on multiple variables.
#'
#' \code{serpentine} sorts data in a serpentine fashion (alternating between
#'   ascending and descending orders). This is helpful in complex sampling
#'   designs with implicit stratification, as it reduces the variation in the
#'   stratified outcome for adjacent sampled units and thus reduces the overall
#'   sampling error. Serpentine sorts are commonly used in NCES surveys.
#'
#' @param data is the data.frame to be sorted
#' @param ... are the variables to serpentine sort, in the given order. That is,
#'   the first variable listed will be sorted in ascending order, the second
#'   variable will alternate between ascending and descending order by the value
#'   of the first variable, and so on.
#'
#' @return A data.frame with equal size as the original data, but sorted
#'   differently.
#'
#' @examples
#' serpentine(data = mtcars, cyl, mpg)
#' serpentine(data = mtcars, cyl, vs, mpg)
#'
#' # If you only want to serpentine sort the mpg variable but keep the rest of
#' # the variables in descending order, then create a new group index variable
#' # with your specified non-serpentined sorted variables.
#' # Following example uses pipelines and dplyr
#' library(magrittr)
#' library(dplyr)
#' mtcars %>%
#'   mutate(group_num = group_indices(., cyl, vs)) %>%
#'   serpentine(group_num, mpg)
#'
#' @import rlang
#' @name serpentine
NULL


# HELPER FUNCTIONS -------------------------------------------------------------

# Single Seprentine Sort
# Function to take a data frame and two variables and return a serpentine
# sort on the second var with the a descending srot on the first var
single_serp <- function(data = NULL, var_desc = NULL, var_serp = NULL) {

  # Split data into lists by value of var_desc
  split_data <- split(data, dplyr::pull(data, !!var_desc))

  # Apply ascending sort to odd list elements
  split_data[c(T, F)] <-
    lapply(
      split_data[c(T, F)],
      function(x) {
        dplyr::arrange(x, !!var_serp)
      }
    )

  # Apply descending sort to even list elements
  split_data[c(F, T)] <-
    lapply(
      split_data[c(F, T)],
      function(x) {
        dplyr::arrange(x, desc(!!var_serp))
      }
    )

  # Recombine in data by appending list elements
  dplyr::bind_rows(split_data)

}


# EXPORTED FUNCTIONS -----------------------------------------------------------

#' @rdname serpentine
#' @export
serpentine <- function(data = NULL, ...) {

  full_vars <- rlang::quos(...)

  # Run serpentine sort on first two variables
  data <- single_serp(data = data, full_vars[[1]], full_vars[[2]])
  data <- dplyr::mutate(data, row_n = dplyr::row_number())
  data <- dplyr::group_by(data, !!!full_vars[1:2])
  data <- dplyr::mutate(data, new_group_num = min(row_n))
  data <- dplyr::ungroup(data)

  # Loop throuh next j values
  if (length(full_vars) >= 3) {
    for (j in 3:length(full_vars)) {
      data <- single_serp(data, rlang::quo(new_group_num), full_vars[[j]])
      data <- dplyr::mutate(data, row_n = dplyr::row_number())
      data <- dplyr::group_by(data, !!!full_vars[1:j])
      data <- dplyr::mutate(data, new_group_num = min(row_n))
      data <- dplyr::ungroup(data)
    }
  }

  # Clean data and export
  dplyr::select(data, -new_group_num, -row_n)

}
