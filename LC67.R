# 67. Add Binary
#  
# Given two binary strings, return their sum (also a binary string).
# 
# The input strings are both non-empty and contains only characters 1 or 0.
# 
# Example 1:
#   
#   Input: a = "11", b = "1"
# Output: "100"
# Example 2:
#   
#   Input: a = "1010", b = "1011"
# Output: "10101"


a = c("1010")  
b = c("1011")

addBinary <- function(a,b){
  #add a b
  total = as.character(as.numeric(a)+as.numeric(b))
  #reset binary
  while(grepl("2", total)){
    total = ifelse(substring(total,1,1)=="2", paste("10",substring(total,2,nchar(total)),collapese=""),total)
    total = gsub("12", "20", total)
    total = gsub("02", "10", total)
    total = gsub(" ", "", total)
  }
  return(total)
}



addBinary(a,b)

addBinary("11","1")