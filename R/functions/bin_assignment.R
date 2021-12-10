##################################
###ASSIGN BINS VIA PROBABILITY###
#################################

assign_bins_prob <- function(max, min, bins){
  gen <- seq(from = min, to = max, length.out = 10000)
  m <- which.max(hist(gen, breaks = bins, plot = FALSE)$counts)
  mid_ma <- hist(gen, breaks = bins, plot = FALSE)$mids[m] #extract mid points of bins
  prob <- max(hist(gen, breaks = bins, plot = FALSE)$counts)/10000
  output <- cbind.data.frame(mid_ma, prob)
  return(output)
}

max <- 11 #maximum age range of fossil
min <- 0 #minimum age range of fossil
bins <- c(0, 10, 20, 30, 40, 50) #bin boundaries
assign_bins_prob(max = max, min = min, bins = bins) #run function

########################################
###ASSIGN BINS FOR ALL POSSIBLE CASES###
########################################

assign_bins_all <- function(data, max, min, bins){
  mid_ages <- (bins[2:length(bins)] + bins[1:length(bins)-1])/2 #calculate mid ages of bins
  if(max == min){max <- max + 0.001; min <- min - 0.001} #if max and minimum are the same adjust for sequence calculation
  tpts <- seq(from = (min+0.001), to = (max-0.001), by = 0.001)
  counts <- table(cut(tpts, breaks=bins))
  names(counts) <- mid_ages
  counts <- counts[which(counts > 0)]
  data <- do.call("rbind", replicate(length(counts), data, simplify = FALSE))
  data$mid_ma <- as.numeric(names(counts))
  return(data)
}

#PARED <- read.csv("./data/occurrences/PARED_06_10_2021.csv")
#data <- PARED[2,]
#max <- data$max_ma
#min <- data$min_ma
#bins <- c(90, 92, 95, 98, 105, 110, 125, 140, 150, 160, 165, 170)

#assign_bins_all(data = data, max = max, min = min, bins = bins)

###############################
###ASSIGN AND MATCH BIN AGES###
###############################

assign_bin_ages <- function(max, min, bin_max, bin_min){
  ages <- cbind.data.frame(max, min)
  ages <- unique(ages) #keep only unique pairs
  ages <- ages[order(ages$max),]
  
  bins <- cbind.data.frame(bin_max, bin_min)
  bins <- unique(bins)
  bins <- bins[order(bins$bin_max),]
  
  ages$bin_max <- NA #assign empty cols
  ages$bin_min <- NA #assign empty cols
  
  for(i in 1:nrow(bins)){ #for loop across bins
    ages$bin_min[which(ages$min == bins$bin_min[i])] <- bins$bin_min[i] #assign minimum ages that match exactly any minimum age of stage
    ages$bin_max[which(ages$max == bins$bin_max[i])] <- bins$bin_max[i] #assign maximum ages that match exactly any maximum age of stage
    ages$bin_min[which(ages$max <= bins$bin_max[i] & ages$min >= bins$bin_min[i])] <- bins$bin_min[i] #assign ages that fall within a stage
    ages$bin_max[which(ages$max <= bins$bin_max[i] & ages$min >= bins$bin_min[i])] <- bins$bin_max[i] #assign ages that fall within a stage
  }
  
  constrained <- subset(ages, !is.na(bin_max) & !is.na(bin_min))
  constrained$constrained <- 1
  unconstrained <- subset(ages, is.na(bin_max) | is.na(bin_min))
  unconstrained$constrained <- 0
  
  for(i in 1:nrow(unconstrained)){
    max <- which.min(abs(bins$bin_max-unconstrained$max[i])) #calculate the 'closest' maximum age based on the available bins
    min <- which.min(abs(bins$bin_min-unconstrained$min[i])) #calculate the 'closest' minimum age based on the available bins
    
    unconstrained$bin_max[i] <- bin_max[max] #assign
    unconstrained$bin_min[i] <- bin_min[min] #assign
  }
  
  ages <- rbind.data.frame(constrained, unconstrained)
  
  return(ages)
}

#PARED <- read.csv("./data/occurrences/PARED_06_10_2021.csv") #load data
#bins <- read.csv("./data/stage_bins.csv") #load stage bins
#max <- PARED$max_ma
#min <- PARED$min_ma
#bin_max <- bins$max_ma
#bin_min <- bins$min_ma
#
#age_match <- assign_bin_ages(max = PARED$max_ma, min = PARED$min_ma, bin_max = bins$max_ma, bin_min = bins$min_ma)
#PARED <- inner_join(x = PARED, y = age_match, by = c("max_ma" = "max", "min_ma" = "min")) #join stage names based on assigned mid age

