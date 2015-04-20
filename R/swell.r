#' swell: creates a new package file for a given function
#'
#' Purpose: Simplify the process of creating R functions in packages
#' Creation Date:
#' Last Modified: Wed Feb 18 10:55:25 2015
#'
#' @param fun function, optional argument
#' @param pkg package, optional argument
#' @export
#' @examples
#' swell(eat_so_much,usefulR)

swell = function(fun,pkg){
   fun_text = paste(deparse(fun),collapse='\n')
   fun_name = as.character(substitute(fun))

   header = Rdev_header()

   fun_arg = strsplit(gsub(' ','',gsub('function','',deparse(args(fun))[1])), '[[:punct:]]')[[1]]
   fun_arg = fun_arg[which(fun_arg != '')]

   header = add_params(fun_arg,header)

   header = gsub('function_name',fun_name,header)

   fun_text = paste0(fun_name,' = ',fun_text)

   cat(paste(header,fun_text,sep='\n',collapse='\n'))
}

