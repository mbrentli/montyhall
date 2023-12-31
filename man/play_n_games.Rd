% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem-brentlinger.R
\name{play_n_games}
\alias{play_n_games}
\title{Simulate playing the game}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{The}{function requires a numeric argument specifying how many times ("n)
to simulate the game}
}
\value{
The function will return a dataframe of how many times switching or
staying won or lost the game.
}
\description{
This function will simulate playing the game 'n' times to be able to analyze
the results and develop a strategy for playing the game.
}
\details{
The function will play the entire game and record the results from each simulation,
and return a count of how many times switching or staying won or lost the game.
}
\examples{
play_n_games(n=1000)

}
