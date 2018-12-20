
## initiate the browser at next error, and then not anymore. See
### http://adv-r.had.co.nz/Exceptions-Debugging.html
browseOnce <- function() {
  old <- getOption("error")
  function() {
    options(error = old)
    browser()
  }
}
### options(error = browseOnce())
