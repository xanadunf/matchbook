#' Perform a Bet Placement Action
#' @name mb_bet_place
#' @description This function provide the core bet placement functionality. Its possible to place a single bet by entering in a single value for each of the runner_id, side, odds and stake parameters. It also possible to place multiple bets at once by passing a vector for each of the runner_id, side, odds and stake parameters. In this instance, its imperative that the order in each of the vectors is preserved i.e. that the runner_id, side, odds and stake for the first bet are the first elements of each parameter vector, and that the runner_id, side, odds and stake values for the second bet are the second elements of each parameter vector and so on. There is a cap of 20 on the number of bets that can be placed at one time. NOTE: its is very important to note the 'status' field returned after executing this function to ensure that you be has been matched as you expect. See description for more details.
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param runner_id The id of the runner that you want to place a bet on.
#' @param side The side that you want place a bet. This has to be one of either 'back' or 'lay'.
#' @param stake The amount that want to stake for this bet. The currency used is the currency you specified when you set up your Matchbook.com account. A real number format is required.
#' @param odds The odds you want to place a bet at. The odds type is based on the information from your session_data, which is the default setting saved for your account.
#' @return The status and details of your bet placement are returned.
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
#'   \item{side}{the side the bet was placed on}
#'   \item{odds}{the odds the bet was placed on}
#'   \item{decimal-odds}{the decimal version of the odds}
#'   \item{stake}{the stake placed}
#'   \item{remaining}{this field indicates how much of the original stake placed remains unmatched. If this value is equal to the original stake, the the bet is fully un-matched. If this value is zero, then the bet has been fully matched. Any value in-between indicates a partial match}   
#'   \item{potential-profit}{the potential profit if the matched component of this wager is successful}   
#'   \item{remaining-potential-profit}{the potential profit if the un-matched component of this wager is first matched and then has a successful outcome}   
#'   \item{currency}{The currency the bet stake was placed with}   
#'   \item{created-at}{The date the bet was placed}   
#'   \item{status}{The bet status. Status 'open' indicates an unmatched bet, 'matched' indicates a fully matched bet, 'cancelled' indicates a cancelled bet, 'failed' indicates a failed bet placement. For bets with status='open', the 'stake' and 'remaining' fields are key to determining the exact status. If the 'remaining' value is less than 'stake' but greater than zero, then the bet has been partially matched for a 'stake'-'remaining' amount. If the bet is fully un-matched, then the 'stake' and 'remaining' values will be equal.}   
#' }
#' @seealso \code{\link{mb_get_bets}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' mb_bet_place(session_data=my_session,odds=2.5,stake=5,runner_id=12345)}
#' 

mb_bet_place <- function(session_data,runner_id,side,stake,odds)
{ 
  valid_sides        <- c("back","lay")
  content            <- list(status_code=0)
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(sum(is.null(runner_id))>0){
    if(sum(runner_id%%1)>0) print(paste("The runner_id must be an integer. Please amend and try again."));return(content)
  }
  if(sum(!is.element(side,valid_sides))>0){
    print(paste("The side value must be one of",paste(valid_sides,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.finite(stake)|stake <0)>0){
    print(paste("The stake value must be a positive real number. Please amend and try again."));return(content)
  }
  n_r <- length(runner_id)
  if(n_r!=length(side)|n_r!=length(stake)|n_r!=length(odds)){
    print(paste("The runner_id, stake, odds and side vectors are not of the same length. Please check/amend and try again."));return(content)
  }
  if(n_r>20){
    print(paste("The maximum number of simultaneous bet submissions is 20. You are over this limit. Please amend and try again."));return(content)
  }

    
  bet_list <- data.frame('runner-id'=runner_id,side=side,stake=stake,odds=odds,check.names=FALSE)
  
  body_data          <- paste("{'exchange-type':'back-lay','currency' : '",session_data$currency,"','odds-type':'",session_data$odds_type,"', 'offers': ",jsonlite::toJSON(bet_list,data.frame="rows"),"}",sep="")
  place_bet_resp     <- httr::POST(paste("https://www.matchbook.com/edge/rest/offers",sep=""),body=body_data,httr::set_cookies('session-token'=session_data$session_token),httr::content_type_json(),httr::accept_json(),httr::add_headers('User-Agent'='rlibnf'))  
  status_code        <- place_bet_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(place_bet_resp, "text", "application/json"))
  } else if(status_code==401){
    print(paste("Please login as your session may have expired ...",sep=""))
    content <- jsonlite::fromJSON(content(place_bet_resp, "text", "application/json"))
    content$status_code <- status_code
  } else
  {
    print(paste("Warning/Error in communicating with placing bet at https://www.matchbook.com/edge/rest/offers",sep=""))
    content<- jsonlite::fromJSON(content(place_bet_resp, "text", "application/json"))
  }
  return(content)
}



