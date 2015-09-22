#' Logout of the Matchbook.com API session
#' @name Logout and Session termination
#' @description End the Matchbook.com session.
#' @param username Your Matchbook.com username. If you are not already registered got to: https://www.matchbook.com/
#' @param user_id Your Matchbook.com user_id obtained from the mb_login call. 
#' @param session_token Your Matchbook.com session_token obtained from the mb_login call. 
#' @return The response insteger status_code.
#' @seealso \code{\link{mb_login}}
#' @export 
#' @examples
#' \dontrun{mb_logout("my_username","my_user_id","my_private_session_token")}
#' 
mb_logout <- function(username,user_id,session_token)
{
  status_code <- 0
  body_data     <- paste("{'username': '",username,"', 'user-id': '",user_id,"', 'session-token' : '",session_token,"'}",sep="")
  logout_resp    <- DELETE("https://www.matchbook.com/bpapi/rest/security/session", body=body_data,content_type_json(),accept_json())
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

