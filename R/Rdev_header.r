#' Rdev_header: returns a header file as text
#'
#' Purpose: Simplify the process of creating R functions in packages
#' Creation Date:
#' Last Modified: Sat Feb 14 15:10:19 2015
#'
#' @export
#' @examples
#' Rdev_header(expand.grid)

Rdev_header = function(){
return(
"#' function_name:
#'
#' Purpose: 
#'
#' Creation Date:
#' Last Modified:
#'
#' @param paramname character value, optional argument
#' @export
#' @examples
#'"
)
}

add_params = function(paramname,header){
   paramname = c('burgers','fries')
   raw.header = strsplit(Rdev_header(),'\\n')[[1]]
   param.line = which(grepl('param',raw.header))
   param.lines = paste(sapply(paramname, function(x) gsub('param',x, raw.header[param.line])),collapse='\n')
   mod.header = paste(raw.header[1:(param.line - 1)],param.lines,raw.header[(param.line + 1):length(raw.header)],collapse='\n')

   return(mod.header)
}



   
