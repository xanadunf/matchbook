#' Get List of Current Bets on Matchbook
#' @name mb_get_balance
#' @description Get account balance/exposure etc.
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @return If successful, a list with account balance information. 
#' The data frame has the following fields:
#'  \describe{
#'   \item{id}{the account id}
#'   \item{balance}{the account balance in the currency of the account}
#'   \item{exposure}{the account exposure in the currency of the account}
#'   \item{commission-reserve}{the commission-reserve in the currency of the account}
#'   \item{free-funds}{the free-funds in the currency of the account}
#' }
#' @seealso \code{\link{mb_login}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","my_password"); 
#' mb_get_balance(session_data=my_session)}
#' 

mb_get_balance <- function(session_data)
{
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data paramteter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  param_list         <- list()
  get_balance_resp   <- httr::GET(paste("https://www.matchbook.com/edge/rest/account/balance",sep=""),query=param_list,httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf'))  
  status_code        <- get_balance_resp$status_code  
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_balance_resp, "text", "application/json"))
  } else if(status_code==401){
    print(paste("Please login as your session may have expired ...",sep=""))
    content <- jsonlite::fromJSON(content(get_balance_resp, "text", "application/json"))
    content$status_code <- status_code
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/edge/rest/offers",sep=""))
    content$status_code <- status_code
  }
  return(content)
}

