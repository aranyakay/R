#LC292
#nim game
# 1-3 stones, you first, can you get the last stone(probability)
#exp: 4 -> false

nimGame <- function(X){
  return(ifelse(X %% 4 == 0, F, T) )
}


nimGame(4)

#theory: x*4 are ones that gonna lose
