# 53. Maximum Subarray
#  
# Given an integer array nums, find the contiguous subarray 
# (containing at least one number) which has 
# the largest sum and return its sum.
# 
# Example:
#   
#   Input: [-2,1,-3,4,-1,2,1,-5,4],
# Output: 6
# Explanation: [4,-1,2,1] has the largest sum = 6.
# Follow up:
#   
#   If you have figured out the O(n) solution, try coding another solution using 
# the divide and conquer approach, which is more subtle.

largestSum <- function(input){
  n = max(input)
  for(i in 1:(length(input)-1)){
    for(j in (i+1):length(input)){
      if(sum(input[i:j])>n){
        n = sum(input[i:j])
        result = c(input[i:j])
      }
    }
  }
  return(result)
}



input = c(-2,1,-3,4,-1,2,1,-5,4)
sum(largestSum(input))

