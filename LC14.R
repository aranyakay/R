# 14. Longest Common Prefix
# 
# Write a function to find the longest common prefix string amongst an array of strings.
# 
# If there is no common prefix, return an empty string "".
# 
# Example 1:
#   
#   Input: ["flower","flow","flight"]
# Output: "fl"
# Example 2:
#   
#   Input: ["dog","racecar","car"]
# Output: ""
# Explanation: There is no common prefix among the input strings.
# Note:
#   
#   All given inputs are in lowercase letters a-z.

longestPrefix <- function(input){
  prefix = ""
  for(i in 1:min(nchar(input))){
    input2 = input
    for(j in 1:length(input)){
      input2[j] = substring(input[j], i, i)
    }
    if(sum(input2==input2[1])==length(input2)){prefix = paste(prefix, input2[1], collapse = "")}
  }
  return(gsub(" ","",prefix))
}

longestPrefix(input)
longestPrefix(c("dog","racecar","car"))

#hint
#remove space in output