# R dependencies specific to the R package
lib <- ".library"
installed <- installed.packages(lib = lib)
pkg <- c(
  "remotes",
  "MASS"
)
for (i in seq_along(pkg)) {
  if (!(pkg[i] %in% installed)) {
    install.packages(
      pkg[i],
      lib = lib
    )
  }
}
