.onAttach <- function(libname, pkgname) {
  packageStartupMessage("rElki - R API for Java Data Mining Framework ELKI")
}

.onLoad <- function(libname, pkgname = 'elki-bundle-0.7.5.jar') {
  # Load ELKI jar and initialize JVM
  rJava::.jpackage(pkgname, lib.loc = libname)
  rJava::.jaddClassPath("inst/java/elki-bundle-0.7.5.jar")
}


.onUnload <- function(libname, pkgname) {
  # TODO
}
