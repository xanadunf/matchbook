#' Perform a Bet Cancel Action
#' @name mb_bet_cancel
#' @description This function provides bet cancellation functionality. It is possible to cancel a single bet by entering in a single value for the bet_id parameter. It is also possible to cancel multiple bets at once by passing a vector of the bet_id parameter. It is also possible to cancel all bets from a given market, event or runner by entering the corresponding ids. NOTE: bets with status 'matched' or 'revised' can not be cancelled. 
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param bet_id The bet_id or vector of bet_ids that you want to cancel. 
#' @param event_id The event_id or vector of event_ids that you want to cancel. 
#' @param market_id The market_id or vector of market_ids that you want to cancel. 
#' @param runner_id The runner_id or vector of runner_ids that you want to cancel. 
#' @param cancel_all Boolean variable. Parameter to allow cancellation of all bets on all events/markets/runners. Default is FALSE.
#' @return The status and details of your bet cancellations are returned.
#' The data frame has the following fields:
#'  \describe{
#'   \item{id}{the bet id}
#'   \item{event-id}{the event id on which the original bet was placed}
#'   \item{event-name}{the name of the event on which the original bet was placed}
#'   \item{event-id}{the event id on which the original bet was placed}
#'   \item{market-id}{the market id on which the original bet was placed}
#'   \item{market-name}{the name of the market on which the original bet was placed}
#'   \item{runner-id}{the runner id on which the original bet was placed}
#'   \item{runner-name}{the name of the runner on which the original bet was placed}
#'   \item{temp-id}{the temporary id of the update}
#'   \item{exchange-type}{the exchange type. This should always be 'back-lay'}
#'   \item{side}{the side the bet was placed on}
#'   \item{odds}{the odds the bet was placed on}
#'   \item{odds-type}{the odds-type of the odds field }
#'   \item{decimal-odds}{the decimal version of the odds}
#'   \item{stake}{the stake placed}
#'   \item{potential-profit}{the potential profit if the matched component of this wager is successful}   
#'   \item{remaining-potential-profit}{the potential profit if the un-matched component of this wager is first matched and then has a successful outcome}   
#'   \item{currency}{The currency the bet stake was placed with}   
#'   \item{created-at}{The date the bet was placed}   
#'   \item{status}{The bet status. Status 'open' indicates an unmatched bet, 'matched' indicates a fully matched bet, 'cancelled' indicates a cancelled bet. For bets with status='open', the 'stake' and 'remaining' fields are key to determining the exact status. If the 'remaining' value is less than 'stake' but greater than zero, then the bet has been partially matched for a 'stake'-'remaining' amount. If the bet is fully un-matched, then the 'stake' and 'remaining' values will be equal.}   
#' }
#' If no bets have been cancelled the 'offers' object will be an empty list.
#' @seealso \code{\link{mb_get_bets},\link{mb_bet_place},\link{mb_bet_update}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' mb_bet_cancel(session_data=my_session,odds=2.5,stake=5,runner_id=12345)}
#' 

mb_bet_cancel <- function(session_data,bet_id=NULL,event_id=NULL,market_id=NULL,runner_id=NULL,cancel_all=FALSE)
{ 
  content            <- list(status_code=0)
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(sum(bet_id%%1)>0) {print(paste("The bet_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(event_id%%1)>0) {print(paste("The event_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(market_id%%1)>0) {print(paste("The market_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(runner_id%%1)>0) {print(paste("The runner_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(!is.null(bet_id))==0&sum(!is.null(event_id))==0&sum(!is.null(market_id))==0&sum(!is.null(runner_id))==0&cancel_all==FALSE){
    print(paste("No bets have been specified for cancellation, please try again."));return(content)
  }
  offer_action <- "";event_action <- "";market_action <- "";runner_action <- "";
  if(sum(!is.null(bet_id))>0){
    offer_action <- paste(',offer-ids'=paste(bet_id,collapse=","),sep="")
  }
  if(sum(!is.null(event_id))>0){
    event_action <- paste(',event-ids'=paste(event_id,collapse=","),sep="")
  }
  if(sum(!is.null(market_id))>0){
    market_action <- paste(',market-ids'=paste(market_id,collapse=","),sep="")
  }
  if(sum(!is.null(runner_id))>0){
    runner_action <- paste(',runner-ids'=paste(runner_id,collapse=","),sep="")
  }
  body_data          <- paste("{'exchange-type':'back-lay','currency':'",session_data$currency,"','odds-type':'",session_data$odds_type,"' ",offer_action,event_action,market_action,runner_action,"}",sep="")
  cancel_bet_resp    <- httr::DELETE(paste("https://www.matchbook.com/edge/rest/offers",sep=""),body=body_data,httr::set_cookies('session-token'=session_data$session_token),httr::content_type_json(),httr::accept_json(),httr::add_headers('User-Agent'='rlibnf'))  
  status_code        <- cancel_bet_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(cancel_bet_resp, "text", "application/json"))
    content$status_code <- status_code
  } else if(status_code==401){
    print(paste("Please login as your session may have expired ...",sep=""))
    content <- jsonlite::fromJSON(content(cancel_bet_resp, "text", "application/json"))
    content$status_code <- status_code
  } else{
    print(paste("Warning/Error in communicating with cancel bet at https://www.matchbook.com/edge/rest/offers",sep=""))
    content <- jsonlite::fromJSON(content(cancel_bet_resp, "text", "application/json"))
    content$status_code <- status_code
  }
  return(content)
}



