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
	tides<-read.csv(pipe(
					paste("tide -z -u m -em pSsMm -f c -l \"",location,
							"\" -b '",begin,"' -e '",end,"'",sep="")),
							header=FALSE)
	tides<-tides[tides$V5=="High Tide",]
	tides$V6<-paste(tides$V2,tides$V3)
	tides$V6<-as.POSIXct(tides$V6,format="%Y-%m-%d %I:%M %p",tz="GMT")
	tide<-tides[which.min(abs(difftime(d,tides$V6,units="secs"))),]
	tide_list<-list(height=NA,time=NA)
	if(nrow(tide)>0){
		tide_list<-list(height=strsplit(as.character(tide$V4)," ")[[1]][1],
		time=tide$V6)
	}
	return(tide_list)
}

