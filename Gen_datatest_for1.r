library(ggplot2)
library(ctmm)
library(sf)
library(sp)
library(cowplot)

set_data_format = function(sim){
    sim = ctmm:::pseudonymize(sim)
    sim = data.frame(timestamp = sim$timestamp,sim$longitude,sim$latitude)
    sim <- st_as_sf(sim, coords = c(2:3))
    st_crs(sim) = 4326
    sim = st_transform(sim, crs=3857)
    return(sim)
}

set_boundary_condition = function(sim,sigma){  

    reflect = function(pos,vec){
        n = -pos/norm(pos,type = "2")                
        vecnew = vec - 2*(sum(vec*n))*n        
        return(vecnew)
    }
    coords = st_coordinates(sim)
    displacements = coords[-1,] - coords[-nrow(coords),]
    rn = c()
    rn = rbind(rn,coords[1,])
    for(i in 1:(length(displacements[,1]))){
        dr = displacements[i,]
        d0 = norm(rn[i,]+dr, type="2")  
        if(d0>=sqrt(sigma)){
            dr = reflect(rn[i,],dr)            
        }           
        d0 = norm(rn[i,]+dr, type="2")                          
        if(d0>=sqrt(sigma)){
            dr = c(0,0)            
        }     
        rn = rbind(rn,rn[i,]+dr)               
    }
    rn = data.frame(rn[,1],rn[,2])
    rn = st_as_sf(rn, coords = c(1:2),crs=3857)    
    st_geometry(sim) = rn$geometry
    return(sim)
}


#ARTIFICIAL DATA EXAMPLE
########## presets
T=100
dx=0.1
sigma0 = 1000
tau = 10
D0 = sigma0/tau
########################
#set movement model 1
model=ctmm(tau=Inf,sigma=D0,mu=c(0,0))
sim = simulate(model,t=seq(1, T, by=dx))
sim1 = set_data_format(sim)
sim1 = set_boundary_condition(sim1,2*sigma0)

#PLOT
low_bound=-100
high_bound=100

p1 <- ggplot(sim1) +
    geom_sf() +      
    coord_sf(datum = st_crs(sim1)) +
    xlim(low_bound,high_bound) +
    ylim(low_bound,high_bound) # +  ggtitle("Movement Model #1 (BM)")


p <- p1 + theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())

 ggsave("model1_1_test.png", plot = p, device = "png", width = 5, height = 5) #saved in open file on my system- can set file name.