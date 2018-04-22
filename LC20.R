# 20. Valid Parentheses
# 
# Given a string containing just the characters '(', ')', '{', '}', '[' and ']', 
# determine if the input string is valid.
# 
# An input string is valid if:
#   
#   Open brackets must be closed by the same type of brackets.
# Open brackets must be closed in the correct order.
# Note that an empty string is also considered valid.
# 
# Example 1:
#   
#   Input: "()"
# Output: true
# Example 2:
#   
#   Input: "()[]{}"
# Output: true
# Example 3:
#   
#   Input: "(]"
# Output: false
# Example 4:
#   
#   Input: "([)]"
# Output: false
# Example 5:
#   
#   Input: "{[]}"
# Output: true

strChar = c('(', ')', '{', '}', '[', ']')
strChar = cbind.data.frame(strChar[c(1,3,5)], strChar[c(2,4,6)], stringsAsFactors = F)
colnames(strChar) = c("front","back")

validCheck <- function(input){
  result=T
  output = matrix(,,3)
  for(i in 1:nrow(strChar)){
    input2 = unlist(strsplit(input,""))
    f = which(input2==strChar$front[i])
    b = which(input2==strChar$back[i])
    if(length(f)!=length(b) | sum(b-f)<0){
      result=F}else{
        if(sum(f,b)!= 0){
          output0 = cbind.data.frame(f,b)
          output0$count = i
          colnames(output) = colnames(output0)
          output = rbind(output,output0)  
        }
      }
  }
  output = output[!is.na(output[,3]),]
  if(nrow(output)>1){
    output = output[order(output$count,output$f),]
    f0 = output$f
    output = output[order(output$count,rev(output$b)),]
    output$f = f0
    for(j in 2:nrow(output)){
      if(sum(output[j,1:2]>output[j-1,2])!=2 & sum(output[j,1:2]<output[j-1,2])!=2) result = F
    }
  }
  return(result)
}

validCheck(input)

validCheck("([)]")

validCheck("()[]{}")

validCheck("(]")

validCheck("([](){[]})")

# hint:
#   section0: break by pairs, create matrix with start and end pairs
#   function section 1: calculte location of pairs as start and end vector 
#                       preset result as T, if num of pairs not match or sequence wrong then turn F 
#                       rbind pair result as a location matrix, with variale label cout pairs
# hint: strsplit(text, "") -> need unlist to create vector
#       which(vec==char) can return location as single digit or vector
#   funtion section 2: clean location matrix, sort the end by pairs, move large end number to bottom of pairs
#                      logic for T: need under row both larger than previous end number, or both smaller 
# hint: matrix[order(matrix$var, rev(matrix$var2)),] -->rev can do reverse order