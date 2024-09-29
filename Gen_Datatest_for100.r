 # Loading necessary libraries- given + magick + imager
library(ggplot2)
library(ctmm)
library(sf)
library(sp)
library(cowplot)
library(magick)
library(imager)

# this is a function to set the data formate using ctmm- given
set_data_format = function(sim){
  sim = ctmm:::pseudonymize(sim)
  sim = data.frame(timestamp = sim$timestamp, sim$longitude, sim$latitude)
  sim <- st_as_sf(sim, coords = c(2:3))
  st_crs(sim) = 4326
  sim = st_transform(sim, crs=3857)
  return(sim)
}

# Function to set boundary conditions for model 2- given
set_boundary_condition = function(sim, sigma){  
  
  reflect = function(pos, vec){
    n = -pos / norm(pos, type = "2")                
    vecnew = vec - 2 * (sum(vec * n)) * n        
    return(vecnew)
  }

  coords = st_coordinates(sim)
  displacements = coords[-1,] - coords[-nrow(coords),]
  rn = c()
  rn = rbind(rn, coords[1,])
  for(i in 1:(length(displacements[, 1]))){
    dr = displacements[i,]
    d0 = norm(rn[i,] + dr, type = "2")  
    if(d0 >= sqrt(sigma)){
      dr = reflect(rn[i,], dr)            
    }          
    d0 = norm(rn[i,] + dr, type = "2")                          
    if(d0 >= sqrt(sigma)){
      dr = c(0, 0)            
    }    
    rn = rbind(rn, rn[i,] + dr)              
  }
  rn = data.frame(rn[, 1], rn[, 2])
  rn = st_as_sf(rn, coords = c(1:2), crs=3857)    
  st_geometry(sim) = rn$geometry
  return(sim)
}

# Function to create and save plot- can set dimentions later on for better clarity
create_and_save_plot <- function(sim, file_name) {
  p1 <- ggplot(sim) +
    geom_sf() +      
    coord_sf(datum = st_crs(sim)) +
    xlim(-100, 100) +
    ylim(-100, 100) #+
    # ggtitle(plot_title)
 p <- p1 + theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  ggsave(file_name, plot = p, device = "png", width = 5, height = 5) #saved in open file on my system- can set file name. 
}

# Function to generate movement model simulations- given
generate_simulations <- function(i) {
  T = 100
  dx = 0.1
  sigma0 = 1000
  tau = 10
  D0 = sigma0 / tau

 #numbered to differentiate in the pngs
  model1 = ctmm(tau=Inf, sigma=D0, mu=c(0, 0))
  sim1 = simulate(model1, t=seq(1, T, by=dx))
  sim1 = set_data_format(sim1)
  sim1 = set_boundary_condition(sim1, 2 * sigma0)
  # To save it as pngs
  create_and_save_plot(sim1, paste0("model1_", i, ".png"))

  model2 = ctmm(tau=tau, sigma=sigma0, mu=c(0, 0))
  sim2 = simulate(model2, t=seq(1, T, by=dx))
  sim2 = set_data_format(sim2)
  # To save it as pngs
  create_and_save_plot(sim2, paste0("model2_", i, ".png"))

  model3 = ctmm(tau=c(tau, tau), sigma=sigma0, mu=c(0, 0))
  sim3 = simulate(model3, t=seq(1, T * tau, by=dx * tau))
  sim3 = set_data_format(sim3)
  # To save it as pngs
  create_and_save_plot(sim3, paste0("model3_", i, ".png"))

  model4 = ctmm(tau=c(Inf, tau), sigma=D0, mu=c(0, 0))
  sim4 = simulate(model4, t=seq(1, T * tau, by=dx * tau))
  sim4 = set_data_format(sim4)
  sim4 = set_boundary_condition(sim4, 2 * sigma0)
  # To save it as pngs 
  create_and_save_plot(sim4, paste0("model4_", i, ".png"))
}

# Generate 100 images for each of the 4 models (4N times)
for (i in 1:250) {
  generate_simulations(i)
}



#==============================================================================================
#step 2
# Function to convert an image to a binary matrix
convert_to_binary_matrix <- function(image_file) {
  # Read the image
  img <- image_read(image_file)
 
  # Resize the image for simplicity (e.g., 128x128 pixels)
    img_resized <- image_resize(img, "128x128!")

    # Convert the image to an imager object
    img_cimg <- magick2cimg(img_resized)

    # Convert the image to grayscale
    img_gray <- grayscale(img_cimg)

    # Normalize the pixel values to 0 and 1
    binary_matrix <- as.matrix(img_gray) < 0.5 # Thresholding at 0.5 to convert to binary

  return(binary_matrix)
}

image_files <- list.files(pattern= "model[1-4]_\\d+\\.png")
binary_matrices <- lapply(image_files, convert_to_binary_matrix)

for (i in seq_along(binary_matrices)) {
    write.csv(binary_matrices[[i]], file = paste0("binary_matrix_", i, ".csv"), row.names = FALSE)

}



###############################################


m = t(as.matrix(read.csv("Gen_Sim_BM_CSV/binary_matrix_1.csv")))
image(t(m[nrow(m):1,] ), axes=FALSE, zlim=c(-4,4), col=rainbow(21))

##################################################

# For labeling and removing header for the csv confussion matrix and files
library(imager)
library(magick)

# Function to convert an image to a binary matrix
convert_to_binary_matrix <- function(image_file) {
  # Read the image
  img <- image_read(image_file)
 
  # Resize the image for simplicity (e.g., 128x128 pixels)
  img_resized <- image_resize(img, "128x128!")
  
  # Convert the image to an imager object
  img_cimg <- magick2cimg(img_resized)
  
  # Convert the image to grayscale
  img_gray <- grayscale(img_cimg)
  
  # Normalize the pixel values to 0 and 1
  binary_matrix <- as.matrix(img_gray) < 0.5 # Thresholding at 0.5 to convert to binary
  
  return(binary_matrix * 1) # Convert logical to integer (0 and 1)
}

# List of image files (modify the pattern as necessary to match your files)
image_files <- list.files(pattern = "model[1-4]_\\d+\\.png")

# Convert images to binary matrices
binary_matrices <- lapply(image_files, convert_to_binary_matrix)

# Create the folder if it does not exist
output_folder <- "binary_matrices"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Save the binary matrices to CSV files with specified naming conventions
for (i in seq_along(binary_matrices)) {
  file_suffix <- if (i <= 100) {
    "BM"
  } else if (i <= 200) {
    "OU"
  } else if (i <= 300) {
    "OUF"
  } else {
    "IOF"
  }
  
  file_name <- paste0(output_folder, "/binary_matrix_", i, "_", file_suffix, ".csv")
  
  # Write the matrix to a CSV file without row names and column names
  write.table(binary_matrices[[i]], file = file_name, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
}
