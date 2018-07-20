
install.packages("roxygen2");
install.packages('installr');
install.packages('Rcpp');
install.packages('pkgbuild');
devtools::install_github("r-lib/devtools");

library(devtools);
library(installr);
library(Rcpp);
library(pkgbuild);

installr::install.Rtools();
devtools::find_rtools();
Sys.getenv("PATH")
#Sys.setenv(PATH = paste("C:\\Rtools\\bin", Sys.getenv("PATH"), sep=";"))
#Sys.setenv(PATH = paste("C:\\Rtools\\mingw_64\\bin", Sys.getenv("PATH"), sep=";"));
#Sys.setenv(PATH = paste("C:\\Rtools", Sys.getenv("PATH"), sep=";"))
#Sys.setenv(PATH = "C:\\Program Files\\R\\R-3.4.4\\bin\\x64;C:\\Program Files\\Docker\\Docker\\Resources\\bin;C:\\ProgramData\\Oracle\\Java\\javapath;C:\\WINDOWS\\system32;C:\\WINDOWS;C:\\WINDOWS\\System32\\Wbem;C:\\WINDOWS\\System32\\WindowsPowerShell\\v1.0\\;C:\\WINDOWS\\System32\\OpenSSH\\;C:\\Program Files\\PuTTY\\;C:\\Program Files\\MiKTeX 2.9\\miktex\\bin\\x64\\;C:\\Users\\david\\AppData\\Local\\Microsoft\\WindowsApps;");

#assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools");

find_rtools();

# Setup testing
devtools::use_testthat();


