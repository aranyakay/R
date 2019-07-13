#LC21
#Merge two sorted linked lists and return it as a new list.
#input:  1->2->4, 1->3->4
#output: 1->1->2->3->4->4

 

mergeTwoLists <- function(l1, l2){
  l <- unlist(c(l1, l2))
  l <- l[order(l)]
  return(as.list(l))
}


l1 <- list(1,2,4)
l2 <- list(1,3,4)

mergeTwoLists(l1, l2)
