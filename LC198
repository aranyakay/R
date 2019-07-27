#lc 198
#house robber

rob <- function(house){
  rob <- rep(0,length(house)+2)
  for(i in 1:length(house))rob[i+2] = max(rob[i+1], rob[i]+house[i])
  return(rob)
}

#dynamic programming
#judge by if rob current house
#when yes, get current money and n-2 largest money
#when no, use previous largest money
