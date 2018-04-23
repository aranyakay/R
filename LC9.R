#9. Palindrome Number

# Determine whether an integer is a palindrome. An integer is a palindrome when it 
# reads the same backward as forward.
# 
# Example 1:
#   
#   Input: 121
# Output: true
# Example 2:
#   
#   Input: -121
# Output: false
# Explanation: From left to right, it reads -121. From right to left, it becomes 121-. 
# Therefore it is not a palindrome.
# Example 3:
#   
#   Input: 10
# Output: false
# Explanation: Reads 01 from right to left. Therefore it is not a palindrome.
# Follow up:
#   
#   Coud you solve it without converting the integer to a string?


ifPalindrome <- function(input){
  result = F
  if(input >= 0){
    half <- (nchar(input) - (nchar(input) %% 2))/2
    firstHalf <- substring(input,1,half)
    secondHalf <- substring(input, half+(nchar(input) %% 2)+1, nchar(input))
    #reverse
    secondHalf2 <- vector()
    for(i in 1:nchar(secondHalf)){
      secondHalf2 <- c(substring(secondHalf,i,i),secondHalf2)
    }
    secondHalf2 <- paste(secondHalf2, collapse="")
    if(secondHalf2==firstHalf) result=T
    return(result)
  }
}



ifPalindrome(121)

ifPalindrome(123454321)
ifPalindrome(3366)
ifPalindrome(3663)
ifPalindrome(26762)




