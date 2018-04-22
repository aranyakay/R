# 58. Length of Last Word
#  
# Given a string s consists of upper/lower-case alphabets and empty space 
# characters ' ', return the length of last word in the string.
# 
# If the last word does not exist, return 0.
# 
# Note: A word is defined as a character sequence consists of non-space characters only.
# 
# Example:
#   
# Input: "Hello World"
# Output: 5

input = "Hello World"


lastWord <- function(input){
  input2 = unlist(strsplit(input," "))
  result = input2[length(input2)]
  return(nchar(result))
}

lastWord(input)
