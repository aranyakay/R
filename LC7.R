# 7. Reverse Integer
# 
# Given a 32-bit signed integer, reverse digits of an integer.
# 
# Example 1:
#   
#   Input: 123
# Output: 321
# Example 2:
#   
#   Input: -123
# Output: -321
# Example 3:
#   
#   Input: 120
# Output: 21
# Note:
#   Assume we are dealing with an environment which could only store integers within 
# the 32-bit signed integer range: [???2^31,  2^(31 ??? 1)]. For the purpose of this problem, 
# assume that your function returns 0 when the reversed integer overflows.

reverseInt <- function(input){
  if(input >= 0){
    start = 1
    }else{
      start=2}
  total = vector()
  for(i in start:nchar(input)){
    total = c(substring(input,i,i),total)
  }
  if(start == 2) {
    total = c("-", total)}
  total0 = as.numeric(paste(total,collapse =""))
  return(total0)
}

reverseInt(123)

reverseInt(-123)

reverseInt(-6587)


#hint
#collapse in paste




