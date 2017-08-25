#' Get List of Available Currencies 
#' @name mb_get_currencies
#' @description List the Currencies currently available on Matchbook.com. 
#' @param session_data A session object returned from a successful \code{\link{mb_login}} attempt. It contains security details and your account preferences.
#' @return If successful, a data frame with the following fields:
#'  \describe{
#'   \item{currency-id}{currency name}
#'   \item{short-name}{short version of currency name}
#'   \item{long-name}{long version of currency name}
#'   }
#' @seealso \code{\link{mb_login}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","versafepassword"); 
#' mb_get_currencies(session_data=my_session)}
#' 

mb_get_currencies <- function(session_data)
{
  content            <- list(status_code=0)
  valid_states       <- c("active","pending")
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided valid data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }  
  param_list         <- list()
  get_currencies_resp    <- httr::GET("https://www.matchbook.com/bpapi/rest/lookups/currencies",query=param_list,httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf'))
  status_code        <- get_currencies_resp$status_code
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_currencies_resp, "text", "application/json"))$currencies
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/bpapi/rest/lookups/currencies",sep=""))
    content$status_code <- status_code
  }
  return(content)
}
