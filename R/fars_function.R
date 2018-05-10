#' @name fars_read
#' @title Read and Import File into R
#' @description Returns a tibble data frame after loading the file
#' @details  If your file does not exist or you type the wrong name, the
#'           function will throw an error. If you are using relative path,
#'           the file need to be within relevant directory. Otherwise
#'           there will be an error
#' @param filename The name of your file.
#' @return It returns a data frame
#' @import readr
#' @import dplyr
#' @examples
#' fars_read("Happy.csv")
#' @rdname fars_read
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'@name make_filename
#'@title Create File Name Based on Year
#'@description Returns a string which uses year as suffix
#'@param year The year the accident occured
#'@return The function returns a character string
#'@examples
#'make_filename(2000)
#'@rdname make_filename
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#'@name fars_read_years
#'@title Grabbing the Year and Month From Data
#'@description Returns a data frame containing specific year and month
#'@details If you entered a year that is not valid, there will be a warning.
#'@param years The years you are interested
#'@return A list of data frames for each year
#'@rdname fars_read_years
#'@examples
#'fars_read_years(2013:2014)
#'@import dplyr
#'@export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#'@name fars_summarize_years
#'@title Count the total number of months for the year
#'@description Returns a data frame containing the total number of observations in each month for each year. It counted all observations in the data.
#'@details If you entered a year that is not valid, there will be a warning.
#'@param years The years you are interested
#'@return A data frame containing the number of observations that are in certain month and year
#'@rdname fars_summarize_years
#'@examples
#'fars_summarize_years(2013:2014)
#'@import dplyr
#'@import tidyr
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'@name fars_map_state
#'@title Draw the accident rate map of the state
#'@description The function uses the accident, longtitude and latitude to draw the accident rate map for each state. If you enter an invalid state number or year, there will be an error
#'@param years The state number and year(2013-2015)
#'@return A map object showing the accident numbers on the map
#'@rdname fars_map_state
#'@examples
#'fars_map_state(1, 2013)
#'@import  maps
#'@import  graphics
#'@export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
      xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
