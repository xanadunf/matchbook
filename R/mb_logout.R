#' Terminate a Matchbook.com API session
#' @name mb_logout
#' @description End the Matchbook.com session.
#' @param session_data This is a required parameter containing security and preference information. It must take the exact format of the object returned by the mb_login request. 
#' @return The response integer status_code.
#' @seealso \code{\link{mb_login}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' mb_logout(my_session)}
#' 
mb_logout <- function(session_data)
{
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter."));
  }
  body_data     <- paste("{'username': '",session_data$user_name,"', 'user-id': '",session_data$user_id,"', 'session-token' : '",session_data$session_token,"'}",sep="")
  logout_resp   <- httr::DELETE("https://www.matchbook.com/edge/rest/security/session", body=body_data,httr::content_type_json(),httr::accept_json(),httr::add_headers('User-Agent'='rlibnf'))
  status_code   <- logout_resp$status_code
  content       <- list(status_code=status_code)
  if(status_code==200)
  {
    print(paste("Matchbook session terminated OK.",sep=""))
  } else
  {
    print(paste("Warning/Error in Matchbook session termination.",sep=""))
  }
  return(content)
}

