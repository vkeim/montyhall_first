#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
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
#' Player selects a door
#' @description
#' 'select_door()' picks which door the player will select
#' without knowing what is behind the door.
#' @details
#' The first step of the game once it is set up is for the
#' player to select a door. Later in the game, the player
#' will have the opportunity to stay with this choice or
#' switch their selection. They will receive more information
#' about the doors before needing to make a decision.
#' @param The function uses the object doors, which is a three character
#' numeric vector. a.pick randomly selects one of the doors from
#' the previous vector. The function return() is used to produce
#' the selected door as one character (1, 2, or 3)
#' @return The function returns 1 character indicating
#' the door selection
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host reveals a door with a goat.
#' @description
#' 'open_goat_door()' reveals one of the doors with
#' a goat behind it.
#' @details
#' Once the player selects a door, then the host
#' will reveal one of the doors that contains a goat.
#' The door will not be the number that has already been
#' selected. This means that if the player selected the
#' car on the first round, then either of the other
#' two doors can be opened. If the player selected
#' a goat door initially, then the other goat door
#' will be revealed.
#' @param This function uses the doors object, which
#' is a three character numeric vector. It also uses
#' an if, then statement to establish that if the player
#' initially selected a goat door, then the other goat door
#' should be opened and not the car door. If the player
#' initially selected the car door, then either of the goat
#' doors can be opened. Both scenarios are built so that
#' the game reveals a door that was not initially selected.
#' @return The function returns one character to show which
#' door (1, 2, or 3) has a goat behind it.
#' @examples
#' open_goat_door()
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
#' Player decides whether to change chosen door.
#' @description
#' 'change_door()' allows the player to either
#' stay with their original choice or choose
#' the other door that was not revealed.
#' @details
#' The player needs to make a choice whether they
#' keep their initial chosen door or if they will
#' switch to the door that the game did not open.
#' Whichever door they select in this part of the
#' game is the door that will be revealed to either
#' be a car that they won, or a goat indicating loss.
#' @param This function uses the same doors vector of
#' previous functions, which is a numeric vector that
#' is three characters long. There are also if, then
#' statements to indicate that if the player stays
#' with their initial choice, then the game should
#' refer back to the 'select_door()' result. If the
#' the player chooses to not stay with their initial
#' choice, then the game will select the door that was not
#' initially picked and also not revealed by the
#' 'open_goat_door()' function.
#' @return the function returns one number between 1 and 3.
#' @examples
#' change_door()
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
#' Game reveals whether player won or lost.
#' @description
#' 'determine_winner()' reveals whether the
#' player's final choice has a car or goat
#' behind the door.
#' @details
#' The function recalls the final pick from the
#' previous function and checks whether the door
#' has a car or goat behind it. If there is a car
#' then the game reveals that the player won. If
#' there is a car, then the game reveals that the
#' player lost.
#' @param This function uses the previously named
#' objects 'final.pick'. The 'final.pick'
#' pulls the result from the 'change_door()' function.
#' @return The function returns a three or four character
#' word: "WIN" or "LOSE".
#' @examples
#' determine_winner()
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
#' Play game in its entirety.
#' @description
#' 'play_game()' will cycle through the entire game.
#' @details
#' This function includes game setup all the way through
#' game results. When this function is run, the user will
#' find out whether staying with the initial pick or switching
#' would result in a win or a loss.
#' @param this function uses several objects and functions.
#' a 'new.game' is created with the create_game() function.
#' Then the 'first.pick' is immediately selected using the
#' select_door() function. Then the game reveals a goat door
#' using the open_goat_door() function and the newly created
#' objects new.game and first.pick. Then the final.pick objects
#' are defined for whether the player stays or switches their pick.
#' The two outcomes are defined whether the final pick is stay or
#' switch using the determine_winner() function. And finally, the
#' game.results object is created using all of the previous information
#' put into a data frame and the results of the game are revealed
#' to the user.
#' @return The output shares the results for each step and reveals
#' whether the player won or lost the round.
#' @examples
#' play_game()
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
#' Loop multiple games to see the probabilty
#' of each outcome.
#' @description
#' 'play_n_games()' loops the full game cycle
#' n times and produces the probability of
#' winning or losing.
#' @details
#' The user can change the
#' number of times the game is looped by
#' entering a number in the function. If
#' nothing is entered in the function, then
#' R will run the loop 100 times.
#' @param additional libraries are loaded for
#' this function. R will loop the gameplay for
#' the amount of times specified by the user.
#' If the user does not input a number in the
#' play_n_games() function, then R will default
#' to looping the game 100 times.
#' @return A table will be provided to show the
#' probabilities of winning and losing.
#' @examples
#' play_n_games()
#' play_n_games(10000)
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
