# Ensure tests run from the Cubit project root when started from the tests directory.
script_dir <- dirname(sys.frame(1)$ofile)
if (is.na(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
root_dir <- normalizePath(file.path(script_dir, ".."))
setwd(root_dir)
