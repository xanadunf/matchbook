#' Get List of Available Events
#' @name mb_get_events
#' @description List the Events Available on Matchbook.com
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param sport_ids A vector of integer sport_ids that indicate sports for which event details are required. e.g. c(15,1) gives Soccer and Pro Football (NFL)
#' @param start_date A string (or date/POSIXct) value with format YYYY-mm-dd or YYYY-mm-dd HH:MM:SS format. 
#' @param end_date A string (or date/POSIXct) value with format YYYY-mm-dd or YYYY-mm-dd HH:MM:SS format. 
#' @param market_states A vector of string containing the market states to return. Defaults to 'open' or 'suspended' market types.
#' @return If successful, a dataframe with first 500 events and associated information. Only 500 events are permitted at one time. Pagination is possible but not implemented in this version.
#' The data frame has the following fields:
#'  \describe{
#'   \item{id}{Event id}
#'   \item{name}{Event name}
#'   \item{start}{The start date of the event}
#'   \item{status}{If betting is still available on this event it will have status='open'}
#'   \item{sport-id}{The sport id of this event} 
#'   \item{category-id}{The category of the event e.g. Premier League is a category within Football/Soccer}
#'   \item{in-running-flag}{Is the market currently in-running}
#'   \item{allow-live-betting}{Is it possible for this market to go in running}
#'   \item{market-ids}{The ids of the markets within this event}
#'   \item{meta-tags}{Tags describing the event}
#' }
#' @seealso \code{\link{mb_get_sports},\link{mb_get_markets}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' mb_get_events(session_data=my_session,sport_ids=15)}
#' 

mb_get_events <- function(session_data,sport_ids=NULL,start_date=NULL,end_date=NULL,market_states = c("open","suspended"))
{
  valid_market_states<- c("suspended","open")
  content            <- list(status_code=0)
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }

  if(sum(!is.element(market_states,valid_market_states))>0){
    print(paste("All market_states values must be one of",paste(valid_market_states,collapse=","),". Please amend and try again."));return(content)
  }
  
  param_list         <- list('exchange-type'='back-lay','market-states'=paste(market_states,collapse=","),'per-page'='500')
  if(!is.null(sport_ids)){
    param_list <- c(param_list,'sport-ids'=paste(sport_ids,collapse=","))
  }
  if(!is.null(start_date)){
    start_date_conv    <- try(as.POSIXct(start_date),silent=TRUE)
    if(inherits(start_date_conv,"try-error")){
      print(paste("The start_date must take YYYY-mm-dd or YYYY-mm-dd HH:MM:SS formats. Please amend and try again."));return(content)    
    } else
    {
      start_date_epoch   <- as.numeric(start_date_conv)
    }
    param_list <- c(param_list,after=start_date_epoch)
  }
  if(!is.null(end_date)){
    end_date_conv    <- try(as.POSIXct(end_date),silent=TRUE)
    if(inherits(end_date_conv,"try-error")){
      print(paste("The end_date parameters must take YYYY-mm-dd or YYYY-mm-dd HH:MM:SS formats. Please amend and try again."));return(content)    
    } else
    {
      end_date_epoch     <- as.numeric(end_date_conv)
    }
    param_list <- c(param_list,before=end_date_epoch)
  }
  
  get_events_resp    <- httr::GET("https://www.matchbook.com/edge/rest/events",query=param_list,httr::content_type_json(),httr::accept_json(),httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf')  )
  status_code        <- get_events_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_events_resp, "text", "application/json"))$events
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/edge/rest/events",sep=""))
    content$status_code <- status_code
  }
  return(content)
}

