# 10. Regular Expression Matching
#  
# Given an input string (s) and a pattern (p), implement regular expression matching 
# with support for '.' and '*'.
# 
# '.' Matches any single character.
# '*' Matches zero or more of the preceding element.
# The matching should cover the entire input string (not partial).
# 
# Note:
#   
# s could be empty and contains only lowercase letters a-z.
# p could be empty and contains only lowercase letters a-z, and characters like . or *.
# 
# Example 1:
#   
#   Input:
#   s = "aa"
# p = "a"
# Output: false
# Explanation: "a" does not match the entire string "aa".
# 
# Example 2:
#   
#   Input:
#   s = "aa"
# p = "a*"
# Output: true
# Explanation: '*' means zero or more of the precedeng element, 'a'. Therefore, by 
# repeating 'a' once, it becomes "aa".
# 
# Example 3:
#   
#   Input:
#   s = "ab"
# p = ".*"
# Output: true
# Explanation: ".*" means "zero or more (*) of any character (.)".
# 
# Example 4:
#   
#   Input:
#   s = "aab"
# p = "c*a*b"
# Output: true
# Explanation: c can be repeated 0 times, a can be repeated 1 time. Therefore it 
# matches "aab".
# 
# Example 5:
#   
#   Input:
#   s = "mississippi"
# p = "mis*is*p*."
# Output: false


# preset = T
# loop 1: 1- n(pattern) cut n(string)
# if: if * then loop 2 in preset: from * to end check repeat ones
#                   get n of repeats - > cut(* -> *+n) = preset(*) -> remove > n(string) ones
# if . then cut(string(.))=string(.)
# if cut not match string preset =F


REMatch <- function(string, pattern){
  result = F
  for(i in 1:max(1,(nchar(pattern)-nchar(string)+1))){
    cut = substring(pattern, i, (i+nchar(string)-1))
    #add control for first charac=.
    if(grepl(".*", cut,fixed = T)){  #hint: fixed = T
      result = T
      break
    } 
    if(grepl("*", cut)){
      start = which(unlist(strsplit(cut,""))=="*")
      #add loop for multiple *
      for(m in 1:length(start)){
        #add condition for first char = *
        if(start[m] == 1){
        if(i == 1) break
        string0 = paste(substring(pattern,i-1,1),string,sep="")
        cut0 = paste(substring(pattern,i-1,1),cut,sep="")
        first = T
        start[m] = 2
        }else{
          string0 = string
          cut0 = cut
          first = F
        }
      cut_star = substring(string0, start[m]-1, start[m]-1)
      #this loop needs to stop at next *
      for(j in (start[m]+1):ifelse(!is.na(start[m+1])<nchar(string0), start[m+1], nchar(string0))){
        if(substring(string0, j, j) != cut_star) break
      }
      n = j - start[m]
      cut = paste(substring(cut0,1, start[m]-1),rep(cut_star,n),
                  substring(cut0,start[m]+1,nchar(cut0)),sep="")
      cut = substring(cut,ifelse(first,2,1),nchar(string)+ifelse(first,1,0))
      }
    }
    if(grepl("\\.", cut)){
      start = which(unlist(strsplit(cut,""))==".")
      cut_star = substring(string, start, start)
      cut = paste(substring(cut,1, start-1), cut_star,substring(cut,start+1,nchar(cut)),sep="")
    }
    if(cut == string) result = T
  }
  return(result)
}

s = "aab"
p = "c*a*b"
REMatch(s, p)


s = "mississippi"
p = "mis*is*p*."
REMatch(s, p)

s = "aa"
p = "a*"
REMatch(s, p)

s = "ab"
p = ".*"
REMatch(s, p)