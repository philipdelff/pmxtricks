##' extract sections of Nonmem control streams
##'
##' @param file A file to read from. Normally a .mod or .lst. See lines also.
##' @param lines Text lines to process. This is an alternative to use the file
##'     argument.
##' @param section The name of section to extract. Examples: "INPUT", "PK",
##'     "TABLE", etc.
##' @param return If "text", plain text lines are returned. If "idx", matching
##'     line numbers are returned. "text" is default.
##' @param keepEmpty Keep empty lines in output? Default is FALSE.
##' @param keepName Keep the section name in output (say, "$PROBLEM") Default is
##'     TRUE. It can only be FALSE, if return"idx".
##' @param keepComments Keep comment lines?
##' @param asOne If multiple hits, concatenate into one. This will most often be
##'     relevant with name="TABLE". If FALSE, a list will be returned, each
##'     element representing a table. Default is TRUE. So if you want to process
##'     the tables separately, you probably want FALSE here.
##' @param simplify If asOne=FALSE, do you want the result to be simplified if
##'     only one table is found? Default is TRUE which is desirable for
##'     interactive analysis. For programming, you probably want FALSE.
##' @param cleanSpaces If TRUE, leading and trailing are removed, and multiplied
##'     succeeding white spaces are reduced to single white spaces.
##' @family Nonmem
##' @examples
##' NMgetSection(pmxtricks_filepath("examples/nonmem/run001.lst"),section="DATA")
##'
##' @export


NMgetSection <- function(file=NULL, lines=NULL, text=NULL, section, return="text", keepEmpty=FALSE, keepName=TRUE, keepComments=TRUE, asOne=TRUE, simplify=TRUE, linesep="\n",cleanSpaces=FALSE,debug=F){

    NMextractText(file=file, lines=lines, text=text, section=section,
                  ## this wrapper is especially made for "$" sections
                  char.section="\\$",
                  return=return,
                  keepEmpty=keepEmpty,
                  keepName=keepName,
                  keepComments=keepComments,
                  asOne=asOne,
                  simplify=simplify,
                  cleanSpaces=cleanSpaces,
                  ## we only consider the model definition, not results.
                  type="mod",
                  debug=debug)
    
}

