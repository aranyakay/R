# 70. Climbing Stairs
#  
# You are climbing a stair case. It takes n steps to reach to the top.
# 
# Each time you can either climb 1 or 2 steps. In how many distinct ways can you 
# climb to the top?
#   
#   Note: Given n will be a positive integer.
# 
# Example 1:
#   
#   Input: 2
# Output: 2
# Explanation: There are two ways to climb to the top.
# 1. 1 step + 1 step
# 2. 2 steps
# Example 2:
#   
#   Input: 3
# Output: 3
# Explanation: There are three ways to climb to the top.
# 1. 1 step + 1 step + 1 step
# 2. 1 step + 2 steps
# 3. 2 steps + 1 step



#1->1
#2->2
#3->3
#4->1+ c(4,1) +1
#5 -> 1+ c(5,1) + c(5,2)
#6 -> 1 + c(6,1)+c(6,2)+1

input = 3

climbingStairs <- function(input){
  if(input <= 3) {
    result2 = input
  }else{
    result = input %% 2
    loop = ifelse(result == 1, (input+result)/2-1, (input)/2-1)
    result2 = (result == 0) +1
    for(i in 1:(loop)){
      top = input
      bottom = 1
      if(i > 1){
        for(j in 1:(loop-1)){
          top = top *(top-j)
          bottom = bottom*(j+bottom)
        }
      }
      result2 = result2 + top/bottom
    }
  }
  return(result2)
}

climbingStairs(3)
climbingStairs(4)

climbingStairs(5)

climbingStairs(6)