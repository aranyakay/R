

i=24
filename <- paste(path600w, fileList[i], sep='')
nrows <- 100000
count <- 100
con <- file(description=filename,open="r")    
## N.B.: skip = 17 from original prob.! Usually not needed (thx @Moody_Mudskipper)
varList = readLines(con, 1 )
varList = iconv(varList, "UTF8", "CP936")
description = unlist(strsplit(varList,"\t"), use.names=F)
data <- read.table(con, nrows=nrows, skip=1, header=F, fill = TRUE,
                   sep="\t", encoding = 'UTF-8')
write.table(data, paste0(path600w, "numOnly/data50_", count, ".txt", sep=""),
            fileEncoding="UTF-8",sep="\t", row.names = FALSE, col.names=F)
count=count+1
repeat {
  if (nrow(data) == 0)
    break
  ## process chunk 'data' here, then...
  write.table(data, paste0(path600w, "numOnly/data50_", count, ".txt", sep=""),
              fileEncoding="UTF-8",sep="\t", row.names = FALSE, col.names=F)
  ## ...read next chunk
  if (nrow(data) != nrows)   # last chunk was final chunk
    break
  data <- tryCatch({
    read.table(con, nrows=nrows, skip=0, header=FALSE, encoding = 'UTF-8',
               sep="\t", fill = TRUE )
  }, error=function(err) {
    ## matching condition message only works when message is not translated
    if (identical(conditionMessage(err), "no lines available in input"))
      data.frame()
    else stop(err)
  })
  count=count+1
}
close(con)    

