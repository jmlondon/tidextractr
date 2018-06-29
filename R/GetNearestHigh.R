#' Determine the time and height of the nearest (by time) high tide for given location.
#'
#' This relies on the xtide library to determine the high tide statistics
#' for the specified time and location.
#'
#' @param d a POSIXct value representing the datetime value to be passed to xtide
#' @param location string representing the station location as expected by xtide
#' @return a named list of high tide height and time of nearest high tide
#' @export
#' @examples
#' #no examples yet
GetNearestHigh <-
function(d,location) {
	end<-d+(3600*12)
	end<-format(end,format="%Y-%m-%d %H:%M:%S",tz="GMT")
	begin<-d-(3600*12)
	begin<-format(begin,format="%Y-%m-%d %H:%M:%S",tz="GMT")
	tmp.csv <- tempfile()
	system2("tide",
	        args = paste("-z -u m -em pSsMm -f c -l \"",location,
							"\" -b '",begin,"' -e '",end,"'",sep=""),
							stdout = tmp.csv,
							stderr = FALSE)
	tides <- readr::read_csv(tmp.csv,
	                         col_names = FALSE,
	                         col_types = "cDccc")
	unlink(tmp.csv)
	if (nrow(tides) == 0) {
	  warning(paste("No tide statistics returned for",
	                location,"at",d))
	  return(NULL)
	}
	tides<-tides[tides$X5=="High Tide",]
	tides$X6<-paste(tides$X2,tides$X3)
	tides$X6<-as.POSIXct(tides$X6,format="%Y-%m-%d %I:%M %p",tz="GMT")
	tide<-tides[which.min(abs(difftime(d,tides$X6,units="secs"))),]
	tide_list<-list(height=NA,time=NA)
	if(nrow(tide)>0){
		tide_list<-list(height=strsplit(as.character(tide$X4)," ")[[1]][1],
		time=tide$X6)
	}
	return(tide_list)
}

