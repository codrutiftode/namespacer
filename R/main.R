#' @import dplyr
#' @import rstudioapi
#' @import stringi
library(dplyr)
library(rstudioapi)
library(stringi)

get_active_libs <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  contents <- ctx$contents
  active_libs <- lapply(contents, function(x)
    stri_extract_first(
      x, regex = "(?<=library\\()[a-zA-Z0-9.]+[a-z](?=\\))"))
  active_libs <- active_libs[!is.na(active_libs)]
  return(active_libs)
}

get_lib_funcs <- function(p) {
  funcs <- lsf.str(paste("package:", p, sep=""))
  return(as.list(funcs))
}

get_funcs_in_str <- function(s) {
  if (startsWith(trimws(s), "#")) return(data.frame())
  
  # Allows one space between function name and ()
  matched_pos <- stri_locate_all_regex(s, "(?<!::)[a-zA-Z.][a-zA-Z0-9._]*(?=( |)\\()")
  matches <- as.data.frame(matched_pos[[1]])
  
  to_remove <- c("function", "library", "return", 
                 "c", "lapply", "sapply", "paste", 
                 "is.na", "if", "for")
  
  do_substr <- function(x, y) substr(s, x, y)
  matches <- matches %>%
    dplyr::mutate(func_name = mapply(do_substr, start, end)) %>%
    dplyr::select(-end) %>%
    dplyr::filter(!(func_name %in% to_remove)) %>%
    na.omit()
  
  return(matches)
}

get_file_lines <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  return(ctx$contents)
}

get_lib_for_func <- function(libs, func) {
  for (lib in libs) {
    lib_funcs <- get_lib_funcs(lib)
    first_occ <- which(unlist(lib_funcs) == func)[1]
    if (!is.na(first_occ)) {
      return(lib)
    }
  }
  return(NA)
}

get_lib_for_funcs <- function(libs, funcs) {
  curried <- function(func) get_lib_for_func(libs, func)
  a <- funcs %>%
    dplyr::mutate(lib = mapply(curried, func_name))
}

#' Add namespaces to functions that lack them
#' @examples 
#' addns()
#' @export
addns <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  active_libs <- get_active_libs()
  file_lines <- get_file_lines()
  lapply(seq_along(file_lines), function(i) {
    funcs <- get_funcs_in_str(file_lines[i])
    if (is.null(dim(funcs)) || nrow(funcs) == 0) return()
    funcs <- get_lib_for_funcs(active_libs, funcs)
    funcs <- na.omit(funcs)
    if (is.null(dim(funcs)) || nrow(funcs) == 0) return()
    for (j in 1:nrow(funcs)) {
      row <- funcs[j, ]
      # Perform operations on the row
      rstudioapi::insertText(c(i, row["start"][[1]]), paste(row["lib"], "::", sep=""), ctx$id)
    }
  })
  return(NULL)
}