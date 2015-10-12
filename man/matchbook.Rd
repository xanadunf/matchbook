\name{Overview}
\alias{matchbook}
\docType{package}
\title{
  \packageTitle{matchbook}
}
\description{
  \packageDescription{matchbook}
}
\details{
The package provides the basic functionality for betting on \href{www.matchbook.com}{Matchbook.com} via the RESTful API service. More details and examples can be found on this  \href{https://bitbucket.org/xan_nfitzgerald/matchbook/overview}{Bitbucket repository}.
A registered \href{www.matchbook.com}{Matchbook.com} account is required to use this package. Please read the documentation carefully for each individual function before use.

In order to login and log out, use the following functions:
 \itemize{
   \item \code{\link{mb_login}}
   \item \code{\link{mb_logout}}
 }
 In order to get information on events/marketsor runners, use the following functions:
 \itemize{
   \item \code{\link{mb_get_events}}
   \item \code{\link{mb_get_markets}}
   \item \code{\link{mb_get_runners}}
 }
 In order to perform a bet action, use the following functions:
 \itemize{
   \item \code{\link{mb_bet_place}}
   \item \code{\link{mb_bet_cancel}}
   \item \code{\link{mb_bet_update}}
 }
To see details on bets that have not yet settled, use the following function:
 \itemize{
   \item \code{\link{mb_get_bets}}
 }
 To see details on bets that have already settled, use the following function:
 \itemize{
   \item \code{\link{mb_get_settled}}
 }
 Your balance/exposure can be displayed on login and they are also returned as part of the \code{\link{mb_bet_place}},\code{\link{mb_bet_cancel}} and \code{\link{mb_bet_update}} function calls.
 
}
\author{
  \packageAuthor{matchbook}
  
  Maintainer: \packageMaintainer{matchbook}
}

