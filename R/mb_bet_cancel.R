#' Perform a Bet Cancel Action
#' @name mb_bet_cancel
#' @description This function provides bet cancellation functionality. It is possible to cancel a single bet by entering in a single value for the offer_id parameter. It is also possible to cancel multiple bets at once by passing a vector of the offer_id parameter. It is also possible to cancel all bets from a given market, event or runner by entering the corresponding ids.
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param offer_id The offer_id or vector of offer_ids that you want to cancel. 
#' @param event_id The event_id or vector of event_ids that you want to cancel. 
#' @param market_id The market_id or vector of market_ids that you want to cancel. 
#' @param runner_id The runner_id or vector of runner_ids that you want to cancel. 
#' @param cancel_all Boolean variable. Parameter to allow cancellation of all bets on all events/markets/runners. Default is FALSE.
#' @return The status and details of your bet cancellations are returned.
#' @seealso \code{\link{mb_get_bets},\link{mb_bet_place},\link{mb_bet_updatel}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' mb_bet_cancel(session_data=my_session,odds=2.5,stake=5,runner_id=12345)}
#' 

mb_bet_cancel <- function(session_data,offer_id=NULL,event_id=NULL,market_id=NULL,runner_id=NULL,cancel_all=FALSE)
{ #mb_bet_place(session_data=session_details,runner_id=2265802,side='back',stake=2,odds=5)
  ##session_data <- mb_login("xan_niallf","xan_n1allf");offer_id=NULL;event_id=NULL;market_id=NULL;runner_id=NULL;cancel_all=FALSE
  content            <- NULL
  if(is.null(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(sum(offer_id%%1)>0) {print(paste("The offer_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(event_id%%1)>0) {print(paste("The event_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(market_id%%1)>0) {print(paste("The market_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(runner_id%%1)>0) {print(paste("The runner_id values must be in integer format. Please amend and try again."));return(content)}
  if(sum(!is.null(offer_id))==0&sum(!is.null(event_id))&sum(!is.null(market_id))&sum(!is.null(runner_id))&cancel_all==FALSE){
    print(paste("No bets have been specified for cancellation, pelase try again."));return(content)
  }
  offer_action <- "";event_action <- "";market_action <- "";runner_action <- "";
  if(sum(!is.null(offer_id))>0){
    offer_action <- paste(',offer-ids'=paste(offer_id,collapse=","),sep="")
  }
  if(sum(!is.null(event_id))>0){
    event_action <- paste(' event-ids'=paste(event_id,collapse=","),sep="")
  }
  if(sum(!is.null(market_id))>0){
    market_action <- paste(' market-ids'=paste(market_id,collapse=","),sep="")
  }
  if(sum(!is.null(runner_id))>0){
    runner_action <- paste(' runner-ids'=paste(runner_id,collapse=","),sep="")
  }
  body_data          <- paste("{'exchange-type':'back-lay','currency':'",session_data$currency,"','odds-type':'",session_data$odds_type,"' ",offer_action,event_action,market_action,runner_action,"}",sep="")
  cancel_bet_resp    <- DELETE(paste("https://www.matchbook.com/bpapi/rest/offers",sep=""),body=body_data,set_cookies('session-token'=session_data$session_token),content_type_json(),accept_json(),add_headers('User-Agent'='rlibnf'))  
  status_code        <- cancel_bet_resp$status_code  
  if(status_code==200)
  {
    content <- fromJSON(content(cancel_bet_resp, "text", "application/json"))$offers
  } else
  {
    print(paste("Warning/Error in communicating with cancel bet at https://www.matchbook.com/bpapi/rest/offers",sep=""))
  }
  return(content)
}



