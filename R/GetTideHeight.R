#' Determine the absolute height of the provided datetime for given location.
#'
#' This relies on the xtide library to determine the tide statistics
#' for the specified time and location.
#'
#' @param d a POSIXct value representing the datetime value to be passed to xtide
#' @param location string representing the station location as expected by xtide
#' @return height in meters
#' @export
#' @examples
#' #no examples yet
GetTideHeight <-
  function(d,location) {
    end <- d + 60
    begin <- format(d,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
    end <- format(end,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
    tide_csv <- readr::read_csv(
      paste0(
        system2("tide",
                args = paste("-z -u m -em pSsMm -f c -m m -l \"",location,
                             "\" -b '",begin,"' -e '",end,"'",sep = ""),
                stdout = TRUE,
                stderr = FALSE),"\n"),
      col_names = FALSE, col_types = "cDcd"
    )
    if (nrow(tide_csv) == 0) {
      warning(paste("No tide statistics returned for",
                    location,"at",d))
      height <- NULL
    }
    height <- tide_csv[[1,4]]
    return(height)
  }

