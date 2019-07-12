#Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

#An input string is valid if:

#Open brackets must be closed by the same type of brackets.
#Open brackets must be closed in the correct order.
#Note that an empty string is also considered valid.

#Example 1:
#Input: "()"
#Output: true

#Example 2:
#Input: "()[]{}"
#Output: true

#Example 3:
#Input: "(]"
#Output: false

#Example 4:
#Input: "([)]"
#Output: false

#Example 5:
#Input: "{[]}"
#Output: true

#hint: build a judgement vector

lc20 <- function(s){
  start <- c("(", "[", "{")
  end <- c(")", "]", "}")
  S <- unlist(strsplit(s,""))
  jug <- vector()
  for(i in 1:length(S)){
    if(S[i] %in% start) jug <- c(jug, S[i])
    if(S[i] %in% end){
      if(start[which(end == S[i])] %in% jug & 
         ifelse(length(jug) > 0, start[which(end==S[i])]==tail(jug,1), F)){
        jug <- jug[-max(which(jug==start[which(end == S[i])]))]
      }else{
        jug <- c(jug, S[i])
      }
    }
  }
  return(length(jug)==0)
}

lc20("()")

lc20("()[]{}")

lc20("(]")

lc20("([)]")

lc20("{[]}")

lc20("{{[]}}()()(())()(){()()(){([()][()]()){[[()]]}{()}}}(())")

