
#' Find dollar in lines
#' 
#' Used to find Nonmem parameters in text. This is a background function used by others. 
#'
#' @param lines A Nonmem output or input file as a vector of lines
#' @param name The desired dollar name. eg PROBLEM (PROB) without the $
#' @param keepEmpty Boolean. Should empty lines be included? Defaults to FALSE.
#' @param keepDollarEmpty Boolean. Should a line including the dollar, but with nothing else be kept? Defaults to TRUE.
#' @param drop.comments drop comment lines from output.
#' @author CIEM
#' @keywords internal
#' @return 
#' The value of desired $name with comments and everything.
#'
#' @examples
#' testLines = list("$asdf  ","$asdf asd ", "asdfa","$bla", "$derp","")
#' findDollar(testLines,"bla")
#' findDollar(testLines,"asdf")
#' findDollar(testLines,"asdf", FALSE, FALSE)
#' findDollar(testLines,"derp",TRUE)

##### change log
## 2017-08-16 ppda: bugfix: removed NM comments from output

## 2017-08-16 ppda: bugfix: wasn't catching lines starting with blanks (which is valid in nonmem). Fixed.
#### end change log


####### TODO
### ppda: the function only works reliably on input.txt, not output.txt. I shuggest discarding the possibly Nonmem generated part of the lines (which is the part after the repeated input.txt). So far, I have used something like this to remove it:
## lines <- lines[1:(min(c(length(lines.tab),grep("NM-TRAN MESSAGES|WARNINGS AND ERRORS \\(IF ANY\\) FOR PROBLEM",lines)-1)))]
### it may have to be adjusted not to make trouble if grep returns nothing.

####### end todo


findDollar <- function(lines,name,keepEmpty = FALSE, keepDollarEmpty = TRUE,drop.comments=T){
  #Result array
  result = c()
  regex = paste("^ *\\$",name,sep = "")
  #Find all the lines that start with the $name
  findLines = grep(regex,lines)
  #For each found line, check consecutive lines
  for (line in findLines){
    #First of all add this $name if it's not empty
    #if(nchar(sub(paste("^\\$",name," *",sep = ""),"",lines[line]))> 0)
    #If there is only empty characters after "$name", then check if it should be added to result
    if (keepDollarEmpty || !grepl(paste(regex,"[[:space:]]*$",sep = ""),lines[line]))
      result = c(result,line)
    i = line + 1
    #Check a line at a time after the previous if they start with $something. If they do not, then add it to the result
    while( i <= length(lines) && !grepl("^\\$[A-Za-z]+",lines[i])){
      #Check if it's not empty, before adding it. This removes useless lines
      if(keepEmpty) result = c(result,i)
      else {
        # if (nchar(lines[i]) > 0) result = c(result,i)
        #check that line is not just spaces
        if (!grepl("^[[:space:]]*$",lines[i])) result = c(result,i)
      }
      i = i + 1
    }
  }
    ## dropping comments
    if(drop.comments){
        result <- result[!grepl("^ *;",lines[result])]
    }
  return (unlist(result))
}
