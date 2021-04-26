# List all packages needed here
{
  packages <- c("ggplot2", "gganimate", "tidyverse")
  
  # Find all uninsalled packages
  installed_packages <- packages %in% rownames(installed.packages())
  # If any packages not installed, iterate over vector and install
  if (any(installed_packages == FALSE)){
    install.packages(packages[!installed_packages])
  }
  
  # Load all packages with LAPPLY
  lapply(packages, library, character.only = TRUE)
}

# Function: datamovie
# I: frame_data --> list of data to create frames to
# I: plot_fun --> a ggplot-esque function to apply your data to
# I: ... --> list of other data sources required for plot_fun
# I: xmin. xmax, ymin, ymax --> known 
# O: for now, a list of where each value is a frame
# O: goal: write SVG/GIF to output
datamovie <- function(frame_data, plot_fun, ..., xmin=0, xmax=NULL, ymin=0, ymax=NULL){
  # Set default limits so hopefully work as expected
  if(!is.integer(xmin)) xmin = 0
  if(!is.integer(xmax)) xmax = 100 # Overwrite based on data
  if(!is.integer(ymin)) ymin = 0
  if(!is.integer(ymax)) ymax = 100 # Overwrite based on data
  
  # Apply individual data frames to point
  frames <- lapply(frame_data, plot_fun(...)) %>%     # Get frames of plotting by getting all graphs
    lapply(FUN=function(x) x + xlim(xmin, xmax)) %>%  # Apply xlim so same size (different by default)
    lapply(FUN=function(x) x + ylim(ymin, ymax))      # Apply ylim so same size (different by default)
  
  return(frames)
}

# ---------------------------------- Sanity testing

# Load in seatbelt data
unique(mpg$cyl)

mpg_cyl <- split(mpg, mpg$cyl)
plotfun <- function(d, xval, yval){
  ggplot(d, aes(x=xval, y=yval, color=manufacturer)) + geom_jitter() + ggtitle(paste0("Cylinders=",d$cyl))
}
plotfun(mpg_cyl$'6', mpg_cyl$'6'$cty, mpg_cyl$'6'$hwy)


mpg_test <- lapply(mpg_cyl, plotfun, xval=mpg_cyl$cty, yval=mpg_cyl$hwy)
mpg_test <- lapply(mpg_test, function(x) x + ylim(0,25))
