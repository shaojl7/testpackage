#' Read Fatality Analysis Reporting System File
#' 
#' \code{fars_read} reads the Fatality Analysis Reporting System
#' 
#' @param filename \code{string} of characters that contain filename of the csv file that contains the data
#' 
#' @return \code{tibble} \code{data.frame} of the data read from csv file if such filepath exists;
#' it returns an error message returns if specified filename does not exist in the path
#' 
#' @examples 
#' fars_read("daily_SPEC_2014.csv.bz2")
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Make File Name
#' 
#' \code{make_file} creates a standardized filename based on the year specified
#' 
#' @param year \code{numerical} value that converted to the year, used to specify filename
#' 
#' @return \code{make_file} returns \code{string} of characters that contain
#' csv filename based on the year specified
#' 
#' @examples 
#' make_filename(2017)
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read Fatality Analysis Reporting System File based on years specified
#' 
#' \code{fars_read_years} takes in multiple years, and returns the
#' Fatality Analysis Reporting System Data based on the years specified
#' 
#' @param years \code{vector} of \code{numerical} values of specified years
#' 
#' @return \code{fars_read_years} will take in multiple years, and create standardized filenames,
#' and then search for and read the filenames and return \code{tibble} \code{data.frame}. 
#' If such filename with the specified year does not exist, an error message will return
#' 
#' @examples 
#' fars_read_years(2010:2017)
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' 
#' @export
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


#' Summarize Fatality Analysis Reporting System Data by year
#' 
#' \code{fars_summarize_years} reads in Fatality Analysis Reporting 
#'   System data files based on the years specified and summarized the
#'   result based on the year and month. 
#' 
#' @param years \code{vector} of \code{numerical} values of specified years
#' 
#' @return \code{fars_summarize_years} returns summarized result in \code{data.frame} of Fatality Analysis
#' Reporting System Data based on year specified 
#' 
#' @examples 
#' fars_summarize_years(2015:2016)
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' 
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map Fatality Analysis Reporting System Data 
#' 
#' \code{fars_map_state} plots the Fatality Analysis Reporting System data
#' based on year and state specified
#' 
#' @param state.num \code{integer} number that specifies state number
#' @param years  \code{vector} of \code{numerical} value that converted to the year, used to specify filename
#' 
#' @return \code{fars_map_state} plots a map of the accidients based on year
#' and state specified. If there is no accident, a prompt message returns. 
#' If an invalid state number is provided, an error will return. 
#' 
#' 
#' @examples 
#' fars_map_state(2, 2016)
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @export
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