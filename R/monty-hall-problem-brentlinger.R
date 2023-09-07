#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of three doors total with two doors
#'   with goats behind them, and one door with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' Player randomly selects a door.
#'
#' @description
#' `select_door()` generates a door selection from game created in `create_game()`
#'
#' @details
#' The door is randomly selected from the game created in `create_game()`
#'
#' @param
#' no arguments are used by the function.
#'
#' @return
#' The function returns a length 1 number vector, indicating the door position of the one selected
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens a goat door.
#'
#' @description
#' The host of the game will open one of the remaining doors that were not
#' selected by the player.  The opened door must be a losing goat door, which
#' excites the game.  The player must next pick whether they want to switch
#' doors or keep their selection.
#'
#' @details
#' The opened door will be a door that is not equal to the player selection,
#' and not equal to a car.
#'
#' @param
#' arguments used by the function are the 'game', which should be the
#' setup of the game created by the 'create_game()' function, or a vector of
#' three prizes, for example game<-c("goat", "goat", "car") and the 'a.pick'
#' which should be the player selection from the 'select_door()' function,
#' or a number of which door was selected
#'
#' @return
#' The function returns a length 1 number vector, indicating the door
#' position of the opened goat door
#'
#' @examples
#' open_goat_door(game, a.pick)
#' x<-c("goat", "goat", "car")
#' y<-3
#' open_goat_door(x, y)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Player chooses to stay or switch.
#'
#' @description
#' After the host has opened a remaining goat door, there are now two doors
#' left: the player's original selection and one more door.  The player now
#' must choose whether they want to keep their original door, or
#' change to the remaining unopened door.
#'
#' @details
#' The player can either stay with their original pick, in which case that
#' becomes their final selection.  Or they can choose to switch doors,
#' in which case their final selection becomes the remaining unopened door
#' (not the original pick and not the opened goat door).
#'
#' @param
#' arguments used by the function are 'stay' in which stay=T means the
#' player is keeping their original pick, or stay=F the player will switch doors.
#' The default is stay=T, 'opened.door' which is the door that the host opened
#' in the 'open_door()' function, and 'a.pick' which is the player's original
#' selection in the 'select_door()' function.
#'
#' @return
#' The function returns a length 1 number vector, indicating the final player
#' selection.
#'
#' @examples
#' change_door(stay=T, opened.door, a.pick)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine whether the player won the game.
#' 
#' @description
#' The function will determine whether the player wins or loses the game 
#' and returns the results of the game. 
#' 
#' @details
#' If the player's final pick is a car, the player wins the game.  If the 
#' player's final pick is a goat, the player loses the game.
#' 
#' @param
#' Arguments used by the function are the player's final pick which was 
#' determined in the 'change_door()' function, and the setup of the game 
#' determined in the 'create_game()' function.
#' 
#' @return
#' The function returns a character string, either "Win" or "Lose" 
#' depending on the outcome of the game.
#' 
#' @examples
#' determine_winner( final.pick, game )
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play the Monty Hall game
#' 
#' @description
#' This function combines all game functions into one function to be able to 
#' play the entire game with one function.
#' 
#' @details
#' By running this function, the entire game is played once.
#'  
#' @param
#' no arguments are used by the function, but it relies on all other functions: 
#' create_game(), select_door(), open_goat_door(), change_door(),determine_winner()
#' 
#' @return
#' The function will return the results or outcome of the instance of the game played.
#' 
#' @examples
#' play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Simulate playing the game
#' 
#' @description
#' This function will simulate playing the game 'n' times to be able to analyze 
#' the results and develop a strategy for playing the game.
#' 
#' @details
#' The function will play the entire game and record the results from each simulation, 
#' and return a count of how many times switching or staying won or lost the game.
#' 
#' @param
#' The function requires a numeric argument specifying how many times ("n) 
#' to simulate the game
#' 
#' @return
#' The function will return a dataframe of how many times switching or 
#' staying won or lost the game.
#' 
#' @examples
#' play_n_games(n=1000)
#' 
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
