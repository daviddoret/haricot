require(pkgdown);
rebuild_package <- function(...){

  devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'));
  pkgdown::build_site();

  command <- "Rcmd.exe INSTALL --preclean --no-multiarch --with-keep.source haricot";
  system(command);

  devtools::load_all(".");

  };

