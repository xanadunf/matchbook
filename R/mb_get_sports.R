#' Get List of Available Sports 
#' @name mb_get_sports
#' @description List the Sports Available on Matchbook.com
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param status  A string with value one of 'active', 'pending'
#' @return If successful, the list of returned sports data.
#' @seealso \code{\link{mb_get_events}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","my_password"); 
#' mb_get_sports(session_data=my_session)}
#' 

mb_get_sports <- function(session_data,status="active")
{
  content            <- NULL
  valid_states       <- c("active","pending")
  if(is.null(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(!is.element(status,valid_states)){
    print(paste("Invalid status, it must a string with value one of",paste(valid_states,collapse=",")))
    return(content)
  }
  
  body_data          <- paste("{'status': '",status,"'}",sep="")
  get_sports_resp    <- GET("https://www.matchbook.com/bpapi/rest/lookups/sports",body=body_data,set_cookies('session-token'=session_data$session_token),add_headers('User-Agent'='rlibnf'))
  status_code        <- get_sports_resp$status_code
  if(status_code==200)
  {
    content <- fromJSON(content(get_sports_resp, "text", "application/json"))$sports
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/bpapi/rest/lookups/sports",sep=""))
  }
  return(content)
}
