#' Import score data from an Excel spreadsheet.
#'
#' Input spreadsheet is not tidy. See template for format: Columns are "judge"
#' and then entry IDs. Each row is a judge name and associated scores scores.
#' Judge names are uniqued by appending a digit if not already unique.
#' @param input_filename Path to the file to import.
#'
#' @return A dataframe containing tidy data
#' @export
import_scores <- function(input_filename){
  readxl::read_excel(input_filename) |>
    dplyr::mutate(entry = as.character("entry")) |>
    tidyr::pivot_longer(3:last_col(),
                        names_to = "entry",
                        values_to = "score")
}
