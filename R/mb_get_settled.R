#' Get Details of Settled Bets
#' @name mb_get_settled
#' @description Get Details of all Settled bets
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param sport_id A vector of integer sport_ids that indicated sports for which event details are required. e.g. c(15,1) gives Soccer and Pro Football (NFL)
#' @param period A value with one of 'today', '1-day', '2-day', 'yesterday', 'week', 'month', '1-month', '3-month'
#' @param start_date A string (or date/POSIXct) value with format YYYY-mm-dd or YYYY-mm-dd HH:MM:SS format. Defaults to 90 days ago.
#' @param end_date A string (or date/POSIXct) value with format YYYY-mm-dd or YYYY-mm-dd HH:MM:SS format. Defaults to today.
#' @return If successful, a dataframe with first 500 settled bets. Only 500 bets are permitted at one time. Pagination is possible but not implemented in this version. 
#' The data frame has the following fields:
#'  \describe{
#'   \item{stake}{the amount bet}
#'   \item{sport-id}{The sport id on which the bet was placed} 
#'   \item{event-id}{The event id on which the bet was placed} 
#'   \item{market-id}{The market id on which the bet was placed} 
#'   \item{market-type}{The market type on which the bet was placed} 
#'   \item{profit-and-loss}{The amount won or lost on the market}
#'   \item{settled-at}{The date the bet was settled}
#' }
#' @seealso \code{\link{mb_get_bets},\link{mb_get_events}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' mb_get_settled(session_data=my_session)}
#' 

mb_get_settled <- function(session_data,sport_id=NULL,period=NULL,start_date=Sys.Date()-90,end_date=Sys.Date())
{
  valid_periods<- c("today","1-day","2-day","yesterday","week","month","1-month","3-month")
  if(sum(sport_id%%1)>0){
    print(paste("All sport_ids values must be integers. Please amend and try again."));return(content)
  }
  if(sum(!is.element(period,valid_periods))>0){
    print(paste("Period value must be one of",paste(valid_periods,collapse=","),". Please amend and try again."));return(content)
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
  
  content            <- list(status_code=0)
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  
  param_list         <- list('per-page'='500')
  if(sum(!is.null(sport_id))){
    param_list <- c(param_list,'sport-ids'=paste(sport_id,collapse=","))
  }
  
  get_markets_resp    <- httr::GET(paste("https://www.matchbook.com/edge/rest/reports/v1/bets/settled",sep=""),query=param_list,httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf'))
  status_code        <- get_markets_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_markets_resp, "text", "application/json"))$events
  } else if(status_code==401){
    print(paste("Please login as your session may have expired ...",sep=""))
    content <- jsonlite::fromJSON(content(get_markets_resp, "text", "application/json"))
    content$status_code <- status_code
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/edge/rest/reports/v1/bets/settled",sep=""))
    content$status_code <- status_code
  }
  return(content)
}





