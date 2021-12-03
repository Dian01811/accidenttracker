#' Create a character vector of column values
#'
#' \code{select_options()} takes a column of a dataframe and creates a character vector
#' of all unique values within that column.
#'
#' Used to create a list of options from the unique values within a column.
#'
#' @param df A dataframe
#' @param col The name of a column from the dataframe
#' @param all Default is FALSE, when TRUE will add "All" to the vector
#' @param none Default is FALSE, when TRUE will add "None" to the vector
#'
#' @return A character vector
#' @export

select_options <- function(df, col, all = FALSE, none = FALSE) {
  column <- dplyr::pull(df, var = {{col}})
  if (all == TRUE) {
    x <- c("All", sort(unique(column)))
  } else if (none == TRUE) {
    x <- c("None", sort(unique(column)))
  } else {
    x <- c(sort(unique(column)))
  }
  x
}
