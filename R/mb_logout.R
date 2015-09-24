#' Logout of the Matchbook.com API session
#' @name Logout and Session termination
#' @description End the Matchbook.com session.
#' @param session_data This is a required paramter contianin security and preference information. Its an object that must take the exact format of an mb_login request. 
#' @return The response insteger status_code.
#' @seealso \code{\link{mb_login}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","verysafepassword"); 
#' mb_logout(my_session)}
#' 
mb_logout <- function(session_data)
{
  status_code <- 0
  if(is.null(session_data)){
    print(paste("You have not provided data about your session in the session_data parameter."));
  }
  body_data     <- paste("{'username': '",session_data$user_name,"', 'user-id': '",session_data$user_id,"', 'session-token' : '",session_data$session_token,"'}",sep="")
  logout_resp    <- DELETE("https://www.matchbook.com/bpapi/rest/security/session", body=body_data,content_type_json(),accept_json(),add_headers('User-Agent'='rlibnf'))
  status_code   <- logout_resp$status_code
  if(status_code==200)
  {
    print(paste("Matchbook session terminated OK.",sep=""))
  } else
  {
    print(paste("Warning/Error in Matchbook session termination.",sep=""))
  }
  return(status_code)
}

