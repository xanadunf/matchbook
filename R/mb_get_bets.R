#' Get List of Current Bets on Matchbook
#' @name mb_get_bets
#' @description List the first 500 bets that have been made on Matchbook events that have not yet settled.
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param event_ids A vector of event_ids for which a list of current bets is required. This is an optional parameter and the default is to return bets from all events unless market_ids or runner_ids are specified.
#' @param market_ids A vector of market_ids for which a list of current bets is required. This is an optional parameter and the default is to return bets from all markets unless event_ids or runner_ids are specified.
#' @param runner_ids A vector of runner_ids for which a list of current bets is required. This is an optional parameter and the default is to return bets from all runners unless event_ids or market_ids are specified.
#' @param sides A filter to allow selection of either 'back' or 'lay' bets. The default is to return both types.
#' @param status The bet status from one of the possible options ('matched','unmatched','cancelled','expired','open','paused'). By default matched and unmatched bets are returned. Bets with status 'expired' can no longer be matched. 
#' @param interval Time filter (in seconds) to allow selection of bets that were created or updated in the period between the currnet time and the current time minus the specified number of seconds. 
#' @return If successful, a dataframe with first 500 bets and associated information. Only 500 bets are permitted at one time. Pagination is possible but not implemented in this version.
#' The data frame has the following fields:
#'  \describe{
#'   \item{id}{the bet id}
#'   \item{event-id}{the event id on which the bet was placed}
#'   \item{event-name}{the name of the event on which the bet was placed}
#'   \item{event-id}{the event id on which the bet was placed}
#'   \item{market-id}{the market id on which the bet was placed}
#'   \item{market-name}{the name of the market on which the bet was placed}
#'   \item{runner-id}{the runner id on which the bet was placed}
#'   \item{runner-name}{the name of the runner on which the bet was placed}
#'   \item{exchange-type}{the exchange type. This should always be 'back-lay'}
#'   \item{side}{the side the bet was placed on}
#'   \item{odds}{the odds the bet was placed on}
#'   \item{odds-type}{the odds-type of the odds field }
#'   \item{decimal-odds}{the decimal version of the odds}
#'   \item{stake}{the stake placed}
#'   \item{remaining}{this field indicates how much of the original stake placed remains un-matched. If this value is equal to the original stake, the the bet is fully un-matched. If this value is zero, then the bet has been fully matched. Any value in-between indicates a partial match}   
#'   \item{potential-profit}{the potential profit if the matched component of this wager is successful}   
#'   \item{remaining-potential-profit}{the potential profit if the un-matched component of this wager is first matched and then has a successful outcome}   
#'   \item{currency}{The currency the bet stake was placed with}   
#'   \item{created-at}{The date the bet was placed}   
#'   \item{status}{The bet status. Status 'open' indicates an unmatched bet, 'matched' indicates a fully matched bet, 'cancelled' indicates a cancelled bet. For bets with status='open', the 'stake' and 'remaining' fields are key to determining the exact status. If the 'remaining' value is less than 'stake' but greater than zero, then the bet has been partially matched for a 'stake'-'remaining' amount. If the bet is fully un-matched, then the 'stake' and 'remaining' values will be equal.}   
#'   \item{temp-id}{the temporary id of the bet}
#' }
#' @seealso \code{\link{mb_get_sports},\link{mb_get_events},\link{mb_get_markets}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","my_password"); 
#' mb_get_bets(session_data=my_session)}
#' 

mb_get_bets <- function(session_data,event_ids=NULL,market_ids=NULL,runner_ids=NULL,sides = NULL,status=NULL,interval=0)
{
  valid_sides        <- c("back","lay")
  valid_status       <- c("matched","unmatched","cancelled","expired","open","paused")
 
  content            <- list(status_code=0)
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data paramteter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(!is.null(event_ids)){
    stopifnot( all(event_ids == floor(event_ids)) ) 
    #print(paste("The event_ids must be integers. Please amend and try again."));return(content)
  }
  if(!is.null(market_ids)){
    stopifnot( all(market_ids == floor(market_ids)) ) 
    #print(paste("The event_ids must be integers. Please amend and try again."));return(content)
    #if(sum(market_ids%%1>0))  print(paste("The market_ids must be integers. Please amend and try again."));return(content)
  }
  if(!is.null(runner_ids)){
    stopifnot( all(runner_ids == floor(runner_ids)) ) 
    #if(sum(runner_ids%%1>0)) print(paste("The runner_ids must be integers. Please amend and try again."));return(content)
  }
  if(sum(!is.element(sides,valid_sides))>0){
    print(paste("All sides values must be one of",paste(valid_sides,collapse=","),". Please amend and try again."));return(content)
  }

  if(sum(!is.element(status,valid_status))>0){
    print(paste("All status values must be one of",paste(valid_status,collapse=","),". Please amend and try again."));return(content)
  }
  if(interval%%1>0){
    print(paste("The interval must be and integer. Please amend and try again."));return(content)
  }
  
  param_list         <- list('exchange-type'='back-lay','per-page'='500')
  if(!is.null(sides)){
    param_list <- c(param_list,'side'=paste(sides,collapse=","))
  }
  if(!is.null(status)){
    if(sum(!is.element(status,valid_status))>0){
      print(paste("All status values must be one of",paste(valid_status,collapse=","),". Please amend and try again."));return(content)
    }
    param_list <- c(param_list,status=paste(status,collapse=","))
  }
  if(!is.null(event_ids)){
    param_list <- c(param_list,'event-ids'=paste(event_ids,collapse=","))
  }
  if(!is.null(market_ids)){
    param_list <- c(param_list,'market-ids'=paste(market_ids,collapse=","))
  }
  if(!is.null(runner_ids)){
    param_list <- c(param_list,'runner-ids'=paste(runner_ids,collapse=","))
  }
  if(interval>0){
    param_list <- c(param_list,interval=interval)
  }
  get_bets_resp    <- httr::GET(paste("https://www.matchbook.com/edge/rest/offers",sep=""),query=param_list,httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf'))  
  status_code        <- get_bets_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_bets_resp, "text", "application/json"))$offers
  } else if(status_code==401){
    print(paste("Please login as your session may have expired ...",sep=""))
    content <- jsonlite::fromJSON(content(get_bets_resp, "text", "application/json"))
    content$status_code <- status_code
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/edge/rest/offers",sep=""))
    content$status_code <- status_code
  }
  return(content)
}

