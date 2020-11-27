#' print("fars_read")
#' This is a function that reads a file containing data from the US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' (FARS). A filename must be provided as an argument to the function, which
#' can be done using the \code{filename} argument. If the file does not exists,
#' it generates an error message.
#'
#' @param filename the name of the file which the data are to be read from.
#'
#' @return This function returns a tibble containing the data from the file.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read(filename = "accident_2013.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        tibble::as_tibble(data)
}

#' print("make_filename")
#' This is a function that constructs the filename corresponding to the annual
#' dataset from the US National Highway Traffic Safety Administration's Fatality
#' Analysis Reporting System (FARS), provided a year. The year is passed onto
#' the function as an argument, which can be done using the \code{year} argument.
#'
#' @param year the 4-digit year for which the filename will be constructed.
#'
#' @return This function returns a character string containing the filename
#'    corresponding to the provided year data from the FARS database.
#'
#' @examples
#' make_filename(2015)
#' make_filename(year = 2015)
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' print("fars_read_years")
#' This is a function that reads and returns a list with the data from the US
#' National Highway Traffic Safety Administration's Fatality Analysis Reporting
#' System (FARS), provided a year or vector of years. Each year's data is stored
#' is a different component of the function return as a tibble object. The list
#' of years is passed onto the function as an argument, which can be done using
#' the \code{years} argument. If any of the provided years is not in the correct
#' format, it returns an error message.
#'
#' @param years the year for which the filename will be constructed.
#'
#' @return This function returns a list of tibbles containing each the data
#'    corresponding to a particular year of the FARS database.
#'
#' @examples
#' fars_read_years(2015)
#' fars_read_years(years = c(2013:2015)
#'
#' @importFrom magrittr %>%
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

#' print("fars_summarize_years")
#' This function returns a tibble with the monthly summary of the data from the
#' US National Highway Traffic Safety Administration's Fatality Analysis
#' Reporting System (FARS) for each of the years requested as a vector with an
#' argument passed onto the function, which can be done using the \code{years} argument.
#'
#' @param years the year for which the summary table will be constructed.
#'
#' @return This function returns a tibble containing a monthly summary for each
#'    of the requested years of the FARS database.
#'
#' @examples
#' fars_summarize_years(2015)
#' fars_summarize_years(years = c(2013:2015))
#'
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @importFrom dplyr n
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' print("fars_map_state")
#' This function plots a map visualizing the data corresponding to the requested
#' state and year from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (FARS). The state is provided as a number
#' using the \code{state.num} argument. and the year is provided using the \code{year}
#' argument. If the state number does not exists, it returns an error message.
#' If there are no accidents for the selected state and year, it provides a
#' message.
#'
#' @param state.num the number associated to the state for which data is to be
#'    displayed
#' @param year the year for which the data is to be displayed
#'
#' @return This function plots a map displaying the FARS data corresponding to
#'    a particular year and state.
#'
#' @examples
#' fars_map_state(12,2015)
#' fars_map_state(state.num = 12,year = 2015)
#'
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
