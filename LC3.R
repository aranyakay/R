# 3. Longest Substring Without Repeating Characters
# 
# Given a string, find the length of the longest substring without repeating characters.
# 
# Examples:
#   
#   Given "abcabcbb", the answer is "abc", which the length is 3.
# 
# Given "bbbbb", the answer is "b", with the length of 1.
# 
# Given "pwwkew", the answer is "wke", with the length of 3. Note that the answer 
# must be a substring, "pwke" is a subsequence and not a substring.


# loop 1: 1 - > n
# loop 2: loop1 -> n
# if no dup + length > preset
# replacy preset
# return preset


longestSubstring <- function(input){
  result = substring(input,1,1)
  for(i in 1:nchar(input)){
    for(j in i:nchar(input)){
      result0 = substring(input,i,j)
      if(sum(duplicated(unlist(strsplit(result0,""))))==0 & nchar(result0) > nchar(result)) {
        result = result0}
    }
  }
  return(result)
}


longestSubstring(c("abcabcbb"))
longestSubstring(c("bbbbb"))
longestSubstring(c("pwwkew"))


