#' Get List of Available Markets for a given Event
#' @name mb_get_markets
#' @description List the Markets Available on Matchbook.com for a given Event
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param event_id The event_id integer for which a list of associated markets is required.
#' @param market_states A vector of string containing the market states to return. Defaults to 'open' or 'suspended' market types.
#' @param market_types A vector of strings containing the required market types. Valid market types are either 'multirunner' or 'binary'. Both are returned by default.
#' @param grading_types An optional vector of strings containing the required grading types. Valid grading types are 'one_x_two','asian-handicap','high-score-wins','low-score-wins','point-spread','point-total','single-winner-wins'. All are returned by default.
#' @param include_runners A boolean parameter indicating if the runner names and id should be returned or not. Defaults to FALSE.
#' @param include_prices A boolean parameter indicating if the runner prices should be returned or not. Defaults to FALSE.
#' @return If successful, a dataframe with first 500 markets and associated information. Only 500 markets are permitted at one time. Pagination is possible but not implemented here.
#' The data frame has the following fields:
#'  \describe{
#'   \item{name}{Market name}
#'   \item{start}{The start date of the market}
#'   \item{status}{If betting is still available on this market it will have status='open'}
#'   \item{type}{Market name}
#'   \item{event-id}{Event id}
#'   \item{id}{Market id}
#'   \item{runner-ids}{The ids of runners in this market}
#'   \item{grading-type}{The type of grading}
#'   \item{in-running-flag}{Is the market currently in-running}
#'   \item{allow-live-betting}{Is it possible for this market to go in running}
#'   \item{handicap}{The handicap associated with this market, if any}
#' }
#' If include_runners=TRUE, then additional runner information is returned. Also if include_prices=TRUE then price data for the associated runner is returned nested within the data frame.
#' @seealso \code{\link{mb_get_sports},\link{mb_get_events},\link{mb_get_runners}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","my_password"); 
#' mb_get_markets(event_id=309912)}
#' 

mb_get_markets <- function(session_data,event_id,market_states = c("open","suspended"),market_types=c("multirunner","binary"),grading_types=NULL,include_runners=FALSE,include_prices=FALSE)
{
  valid_market_states<- c("suspended","open")
  valid_market_types <- c("multirunner","binary")
  valid_grading_types<- c('asian-handicap','high-score-wins','low-score-wins','point-spread','point-total','single-winner-wins','one_x_two','handicap', 'total', 'both_to_score', 'correct_score', 'half_time_full_time')
  
  content            <- list(status_code=0)
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(event_id%%1>0){
    print(paste("The event_id must be an integer. Please amend and try again."));return(content)
  }
  if(length(event_id)>1){
    print(paste("The event_id must be a single integer. Please amend and try again."));return(content)
  }
  if(sum(!is.element(market_states,valid_market_states))>0){
    print(paste("All market_states values must be one of",paste(valid_market_states,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.element(market_types,valid_market_types))>0){
    print(paste("All market_types values must be one of",paste(valid_market_types,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.null(grading_types))>0)
  {
    if(sum(!is.element(grading_types,valid_grading_types))>0){
      print(paste("All grading_types values must be one of",paste(valid_grading_types,collapse=","),". Please amend and try again."));return(content)
    }
  }

  param_list         <- list('exchange-type'='back-lay','odds-type'=session_data$odds_type,currency=session_data$currency,'market-states'=paste(market_states,collapse=","),'per-page'='500','types'=paste(grading_types,collapse=","),'market-types'=paste(market_types,collapse=","))
  if(include_runners==TRUE){
    param_list <- c(param_list,'include-runners'='true')
  }
  if(include_prices==TRUE){
    param_list <- c(param_list,'include-prices'='true')
  }

  get_markets_resp    <- httr::GET(paste("https://www.matchbook.com/edge/rest/events/",event_id,"/markets",sep=""),query=param_list,httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf'))
  status_code        <- get_markets_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_markets_resp, "text", "application/json"))$markets
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/edge/rest/events/",event_id,"/markets",sep=""))
    content$status_code <- status_code
  }
  return(content)
}