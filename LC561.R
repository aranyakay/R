# 561. Array Partition I
# 
# Given an array of 2n integers, your task is to group these integers into n pairs 
# of integer, say (a1, b1), (a2, b2), ..., (an, bn) which makes sum of min(ai, bi) 
# for all i from 1 to n as large as possible.
# 
# Example 1:
#   Input: [1,4,3,2]
# 
# Output: 4
# Explanation: n is 2, and the maximum sum of pairs is 4 = min(1, 2) + min(3, 4).
# Note:
#   n is a positive integer, which is in the range of [1, 10000].
# All the integers in the array will be in the range of [-10000, 10000].

Input =  c(1,4,3,2)


sumMinPairs <- function(Input, n=2){
  if(length(Input) %% n != 0){
    total = NA
  }else{
    Input = sort(Input)
    total = 0
    for(i in 1: (length(Input)/n)){
      total = total + min(Input[2*i-1],Input[2*i])
    }
  }
  return(total)
}


sumMinPairs(Input,2)

#hint:as larger as possible
#hint: sort vs order
