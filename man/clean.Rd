% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{clean}
\alias{clean}
\title{Cleaning function}
\usage{
clean(x, MNM = 10, MRANK = 500)
}
\arguments{
\item{x}{Data to be cleaned. It must be a data.frame coming from \href{http://www.tennis-data.co.uk/}{http://www.tennis-data.co.uk/}.}

\item{MNM}{\strong{optional} Minimum number of matches played by each player to include in the cleaned dataset. Default to 10. This means that
each player has to play at least 10 matches}

\item{MRANK}{\strong{optional} Maximum rank of the players to consider. Default to 500. This means that all the matches with players
with ranks greater than 500 are dropped}
}
\value{
Data.frame cleaned
}
\description{
Cleans the dataset in order to create a suitable data.frame ready to be used in the \code{\link{welofit}} function.
}
\details{
The cleaning operations are:
\enumerate{
\item Remove all the uncompleted matches;
\item Remove all the NAs from B365 odds;
\item Remove all the NAs from the variable "ranking";
\item Remove all the NAs from the variable "games";
\item Remove all the NAs from the variable "sets";
\item Remove all the matches where the B365 odds are equal;
\item Define players \eqn{i} and \eqn{j} and their outcomes (\eqn{Y_i} and \eqn{Y_j});
\item Remove all the matches of players who played less than MNM matches;
\item Remove all the matches of players with rank greater than MRANK;
\item Sort the matches by date.
}
}
\examples{
\donttest{
data(atp_2019) 
db_clean<-clean(atp_2019)
str(db_clean)
}
}
