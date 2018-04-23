# 728. Self Dividing Numbers
# 
# A self-dividing number is a number that is divisible by every digit it contains.
# 
# For example, 128 is a self-dividing number because 128 % 1 == 0, 128 % 2 == 0, 
# and 128 % 8 == 0.
# 
# Also, a self-dividing number is not allowed to contain the digit zero.
# 
# Given a lower and upper number bound, output a list of every possible self 
# dividing number, including the bounds if possible.
# 
# Example 1:
#   Input: 
#   left = 1, right = 22
# Output: [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 22]
# Note:
#   
#   The boundaries of each input argument are 1 <= left <= right <= 10000.

left = 1
right = 22

selfDrivingNum <- function(left, right){
  output = vector("numeric")
  for(i in left:right){
    result = T
    for(j in 1:nchar(i)){
      if(substring(i,j,j)=="0"|i %% as.numeric(substring(i,j,j)) != 0) {result = F}
    }
    if(result == T) output = c(output,i)
  }
  return(output)
}

selfDrivingNum(1,22)
selfDrivingNum(9000,10000)


#hint:%%




