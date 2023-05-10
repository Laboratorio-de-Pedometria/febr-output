# Load and publish datasets to the FEBR Dataverse instance

# Install and load required packages
if (!require("dataverse")) {
  install.packages("dataverse")
}

# Set environment variable DATAVERSE_SERVER
Sys.setenv("DATAVERSE_SERVER" = "solo.mapbiomas.org")

dataverse::get_dataverse(
  dataverse = "febr",
  server = "solo.mapbiomas.org")
