#461. Hamming Distance

# The Hamming distance between two integers is the number of positions at which
# the corresponding bits are different.
# 
# Given two integers x and y, calculate the Hamming distance.
# 
# Note:
#   0 ??? x, y < 2^31
# 
# Example:
#   
#   Input: x = 1, y = 4
# 
# Output: 2
# 
# Explanation:
# 1   (0 0 0 1)
# 4   (0 1 0 0)
# ???   ???
# 
# The above arrows point to positions where the corresponding bits are different.

decTobinPos <- function(num){
  num1=num
  for(i in 1:31){
    if(num1/2 >= 1){
      num1 = num - 2^i}else {
        n=i
        break}
  }
  return(n)
}

x = 1
y = 4
decTobinPos(y) - decTobinPos(x) 


#hint inToBits()
#hint convert decimal to binary by 2^n

