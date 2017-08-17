#' Get List of Available Sports 
#' @name mb_get_sports
#' @description List the Sports currently available on Matchbook.com. 
#' @param session_data A session object returned from a successful \code{\link{mb_login}} attempt. It contains security details and your account preferences.
#' @param nsports The number of sports for which details are required. Default is 1000.
#' @return If successful, a data frame with the following fields:
#'  \describe{
#'   \item{name}{sport name}
#'   \item{type}{the type of the sport}
#'   \item{id}{sport id}
#'   }
#' @seealso \code{\link{mb_get_events}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","versafepassword"); 
#' mb_get_sports(session_data=my_session)}
#' 

mb_get_sports <- function(session_data,nsports=1000)
{
  content            <- list(status_code=0)
  valid_states       <- c("active","pending")
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided valid data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }  
  param_list         <- list('per-page'=nsports)
  body_data          <- paste("",sep="")
  get_sports_resp    <- httr::GET("https://www.matchbook.com/edge/rest/lookups/sports",query=param_list,httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf'))
  status_code        <- get_sports_resp$status_code
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_sports_resp, "text", "application/json"))$sports
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/edge/rest/lookups/sports",sep=""))
    content$status_code <- status_code
  }
  return(content)
}
