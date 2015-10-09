#' Perform a Bet Update Action
#' @name mb_bet_update
#' @description This function provides bet update functionality. Its possible to update a single bet by entering in a single value for each of the offer_id, side, odds and stake parameters. It also possible to update multiple bets at once by passing a vector for each of the offer_id, side, odds and stake parameters. In this instance, its imperative that the order in each of the vectors is preserved i.e. that the runner_id, side, odds and stake for the first update are the first elements of each parameter vector, and that the offer_id, side, odds and stake values for the second update are the second elements of each parameter vector and so on. There is a cap of 25 on the number of updates that can be placed at one time.
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param offer_id The id of the offer that you want to update. 
#' @param side The side that you want place a bet. This has to be one of either 'back' or 'lay'.
#' @param stake The amount that want to stake for this bet. The currency used is the currency you specified when you set up your Matchbook.com account. A real number format is required.
#' @param odds The odds you want to place a bet at. The odds type is based on the information from your session_data, which is the default setting saved for your account.
#' @return The status and details of your bet updates are returned.
#' @seealso \code{\link{mb_get_bets},\link{mb_bet_place},\link{mb_bet_cancel}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' new_odds_value <- 20
#' mb_bet_update(session_data=my_session,offer_id=12345,odds=new_odds_value)}
#' 

mb_bet_update <- function(session_data,offer_id,runner_id,side,stake,odds)
{ #mb_bet_place(session_data=session_details,runner_id=2265802,side='back',stake=2,odds=5)
  ##session_data <- mb_login("xan_niallf","xan_n1allf");side=c('back');stake=c(2);offer_id=c(322817036); odds <- c(6)
  valid_sides        <- c("back","lay")
  content            <- NULL
  if(is.null(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(sum(is.null(offer_id))>0){
    if(sum(offer_id%%1)>0) print(paste("The offer_id values must be in integer format. Please amend and try again."));return(content)
  }
  if(sum(!is.element(side,valid_sides))>0){
    print(paste("The side value must be one of",paste(valid_sides,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.finite(stake)|stake <0)>0){
    print(paste("The stake value must be a positive real number. Please amend and try again."));return(content)
  }
  n_o <- length(offer_id)
  if(n_o!=length(side)|n_o!=length(stake)|n_o!=length(odds)){
    print(paste("The offer_id, stake, odds and side vectors are not of the same length. Please check/amend and try again."));return(content)
  }
  if(n_o>25){
    print(paste("The maximum number of simultaneous bet updates is 25. You are over this limit. Please amend and try again."));return(content)
  }
  
  
  update_list <- data.frame(id=offer_id,side=side,stake=stake,odds=odds,check.names=FALSE)
  
  body_data          <- paste("{'exchange-type':'back-lay','currency':'",session_data$currency,"','odds-type':'",session_data$odds_type,"', 'offers': ",toJSON(update_list,data.frame="rows"),"}",sep="")
  update_bet_resp    <- PUT(paste("https://www.matchbook.com/bpapi/rest/offers",sep=""),body=body_data,set_cookies('session-token'=session_data$session_token),content_type_json(),accept_json(),add_headers('User-Agent'='rlibnf'))  
  status_code        <- update_bet_resp$status_code  
  if(status_code==200)
  {
    content <- fromJSON(content(update_bet_resp, "text", "application/json"))$offers
  } else
  {
    print(paste("Warning/Error in communicating with updating bet at https://www.matchbook.com/bpapi/rest/offers",sep=""))
  }
  return(content)
}



