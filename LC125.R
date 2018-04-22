# 125. Valid Palindrome
# 
# Given a string, determine if it is a palindrome, considering only alphanumeric 
# characters and ignoring cases.
# 
# Note: For the purpose of this problem, we define empty string as valid palindrome.
# 
# Example 1:
#   
#   Input: "A man, a plan, a canal: Panama"
# Output: true
#
# Example 2:
#   
#   Input: "race a car"
# Output: false

input = c("A man, a plan, a canal: Panama")

validPalindrome <- function(input){
  input = unlist(strsplit(input,""))
  input = input[input %in% c(letters[1:26],LETTERS[1:26])]
  mid1 = (length(input) - length(input)%%2)/2
  mid2 = 1 + (length(input) + length(input)%%2)/2
  result = toupper(paste(input[1:mid1],collapse=""))==toupper(paste(rev(input[mid2:length(input)]),collapse=""))
  return(result)
}



validPalindrome(input)

validPalindrome(c("race a car"))