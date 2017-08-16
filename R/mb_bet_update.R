#' Perform a Bet Update Action
#' @name mb_bet_update
#' @description This function provides bet update functionality. Its possible to update a single bet by entering in a single value for each of the bet_id, side, odds and stake parameters. It also possible to update multiple bets at once by passing a vector for each of the bet_id, side, odds and stake parameters. In this instance, its imperative that the order in each of the vectors is preserved i.e. that the runner_id, side, odds and stake for the first update are the first elements of each parameter vector, and that the bet_id, side, odds and stake values for the second update are the second elements of each parameter vector and so on. There is a cap of 25 on the number of updates that can be placed at one time.
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param bet_id The id of the bet that you want to update. 
#' @param side The side that you want place a bet. This has to be one of either 'back' or 'lay'.
#' @param stake The amount that want to stake for this bet. The currency used is the currency you specified when you set up your Matchbook.com account. A real number format is required.
#' @param odds The odds you want to place a bet at. The odds type is based on the information from your session_data, which is the default setting saved for your account.
#' @return The status and details of your bet updates are returned.
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
#' @seealso \code{\link{mb_get_bets},\link{mb_bet_place},\link{mb_bet_cancel}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' new_odds_value <- 20
#' mb_bet_update(session_data=my_session,bet_id=12345,odds=new_odds_value)}
#' 

mb_bet_update <- function(session_data,bet_id,side,stake,odds)
{ 
  valid_sides        <- c("back","lay")
  content            <- list(status_code=0)
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(sum(is.null(bet_id))>0){
    if(sum(bet_id%%1)>0) print(paste("The bet_id values must be in integer format. Please amend and try again."));return(content)
  }
  if(sum(!is.element(side,valid_sides))>0){
    print(paste("The side value must be one of",paste(valid_sides,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.finite(stake)|stake <0)>0){
    print(paste("The stake value must be a positive real number. Please amend and try again."));return(content)
  }
  n_o <- length(bet_id)
  if(n_o!=length(side)|n_o!=length(stake)|n_o!=length(odds)){
    print(paste("The bet_id, stake, odds and side vectors are not of the same length. Please check/amend and try again."));return(content)
  }
  if(n_o>25){
    print(paste("The maximum number of simultaneous bet updates is 25. You are over this limit. Please amend and try again."));return(content)
  }
  
  
  update_list <- data.frame(id=bet_id,side=side,stake=stake,odds=odds,check.names=FALSE)
  
  body_data          <- paste("{'exchange-type':'back-lay','currency':'",session_data$currency,"','odds-type':'",session_data$odds_type,"', 'offers': ",jsonlite::toJSON(update_list,data.frame="rows"),"}",sep="")
  update_bet_resp    <- httr::PUT(paste("https://www.matchbook.com/edge/rest/offers",sep=""),body=body_data,httr::set_cookies('session-token'=session_data$session_token),httr::content_type_json(),httr::accept_json(),httr::add_headers('User-Agent'='rlibnf'))  
  status_code        <- update_bet_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(update_bet_resp, "text", "application/json"))
    content$status_code <- status_code
  } else if(status_code==401){
    print(paste("Please login as your session may have expired ...",sep=""))
    content <- jsonlite::fromJSON(content(update_bet_resp, "text", "application/json"))
    content$status_code <- status_code
  } else
  {
    print(paste("Warning/Error in communicating with updating bet at https://www.matchbook.com/edge/rest/offers",sep=""))
    content <- jsonlite::fromJSON(content(update_bet_resp, "text", "application/json"))
    content$status_code <- status_code
  }
  return(content)
}



