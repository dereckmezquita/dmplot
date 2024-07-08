# Debug script for dmplot package build

library(devtools)

# Check R version and installed packages
cat("R version:\n")
print(R.version)

cat("\nInstalled packages:\n")
print(installed.packages()[, c("Package", "Version")])

# Check if required system libraries are installed
system_libs <- c("libcurl4-openssl-dev", "libssl-dev", "libxml2-dev", "libgit2-dev")
for (lib in system_libs) {
  status <- system(paste("dpkg -s", lib, ">/dev/null 2>&1"), intern = TRUE)
  cat(sprintf("%s installed: %s\n", lib, ifelse(status == 0, "Yes", "No")))
}

# Attempt to load and build the package
cat("\nAttempting to load and build the package:\n")
tryCatch({
  load_all()
  document()
  build()
}, error = function(e) {
  cat("Error occurred during package build:\n")
  print(e)
})

# Check for common issues
cat("\nChecking for common issues:\n")

if (!file.exists("DESCRIPTION")) {
  cat("DESCRIPTION file is missing.\n")
} else {
  cat("DESCRIPTION file exists.\n")
}

if (!dir.exists("R")) {
  cat("R directory is missing.\n")
} else {
  cat("R directory exists.\n")
}

if (!dir.exists("src")) {
  cat("src directory is missing.\n")
} else {
  cat("src directory exists.\n")
  cpp_files <- list.files("src", pattern = "\\.cpp$")
  if (length(cpp_files) == 0) {
    cat("No .cpp files found in src directory.\n")
  } else {
    cat("Found", length(cpp_files), ".cpp files in src directory.\n")
  }
}

# Additional checks can be added here

cat("\nDebug script completed.\n")