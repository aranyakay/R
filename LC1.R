#1. Two Sum

# Given an array of integers, return indices of the two numbers such that they add 
# up to a specific target.
# 
# You may assume that each input would have exactly one solution, and you may not 
# use the same element twice.
# 
# Example:
#   
#   Given nums = [2, 7, 11, 15], target = 9,
# 
# Because nums[0] + nums[1] = 2 + 7 = 9,
# return [0, 1].



givenVec <- c(2,7,11,15)
target <- 9



returnTwo <- function(givenVec, target){
  result <- vector("numeric")
  for(i in 1:length(givenVec)){
    givenVec2 <- givenVec[-i]
    for(j in 1:length(givenVec2)){
      if(sum(givenVec[i],givenVec2[j])==target){
        result = c(i,which(givenVec==givenVec2[j]))
        break
      }
    }
  }
  return(result)
}


returnTwo(givenVec, target)


#hint function which