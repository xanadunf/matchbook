#' Get List of Available Events
#' @name List Available Events
#' @description List the Events Available on Matchbook.com
#' @param start_date A string (or date/POSIXct) value with format YYYY-mm-dd or YYYY-mm-dd HH:MM:SS format. Defaults to events starting today .
#' @param end_date A string (or date/POSIXct) value with format YYYY-mm-dd or YYYY-mm-dd HH:MM:SS format. Defaults to events starting before two days time.
#' @param sport_ids A vector of integer sport_ids that indicated sports for which event details are required. e.g. c(15,1) gives Soccer and Pro Football (NFL)
#' @param market_states A vector of string containing the market states to return. Defaults to 'open' or 'suspended' market types.
#' @param language A string with the language parameter e.g. 'en'
#' @return If successful, a dataframe with first 500 events and associated information. Only 500 events are permitted at one time. Pagination is possible but not implemented here.
#' @seealso \code{\link{mb_get_sports}}
#' @export 
#' @examples
#' \dontrun{mb_get_events(sport_ids=15)}
#' 

mb_get_events <- function(start_date=Sys.Date(),end_date=Sys.Date()+2,sport_ids=c(15),market_states = c("open","suspended"),language="en")
{
  valid_market_states<- c("suspended","open")
  valid_languages    <- c("en","de","es","fr","hi","id","ja","ru","th","zh","zh_HANS","zh_HANT")
  content            <- NULL
  if(sum(sport_ids%%1)>0){
    print(paste("All sport_ids values must be integers. Please amend and try again."));return(content)
  }
  if(sum(!is.element(market_states,valid_market_states))>0){
    print(paste("All market_states values must be one of",paste(valid_market_states,collapse=","),". Please amend and try again."));return(content)
  }
  if(!is.element(language,valid_languages)){
    print(paste("The language parameter must be one of",paste(valid_languages,collapse=","),". Please amend and try again."));return(content)
  }
  start_date_conv    <- try(as.POSIXct(start_date),silent=TRUE)
  end_date_conv    <- try(as.POSIXct(end_date),silent=TRUE)
  if(inherits(start_date_conv,"try-error")|inherits(end_date_conv,"try-error")){
    print(paste("The start_date and end_date parameters must take YYYY-mm-dd or YYYY-mm-dd HH:MM:SS formats. Please amend and try again."));return(content)    
  } else
  {
    start_date_epoch   <- as.numeric(start_date_conv)
    end_date_epoch     <- as.numeric(end_date_conv)
  }
  param_list         <- list(after=start_date_epoch,before=end_date_epoch,'sport-ids'=paste(sport_ids,collapse=","),'market-states'=paste(market_states,collapse=","),'per-page'='500',language=language)
  get_events_resp    <- GET("https://www.matchbook.com/bpapi/rest/events",query=param_list,content_type_json(),accept_json())  
  status_code        <- get_events_resp$status_code  
  if(status_code==200)
  {
    content <- fromJSON(content(get_events_resp, "text", "application/json"))
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/bpapi/rest/events",sep=""))
  }
  return(content)
}

mb_get_events()