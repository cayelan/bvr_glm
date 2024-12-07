# ZOOP SENSITIVITY ANALYSIS

#note not comparing to obs bc not so many obs for a good fit

# Function to check if file path ends with ".csv"
ends_with_csv <- function(file_path) {
  tolower(substr(file_path, nchar(file_path) - 3, nchar(file_path))) == ".csv"
}

removeQuotes <- function(x) {
  trimws(gsub("'", '', x))
}

run_sensitivity <- function(var, max_r, x0, lb, ub, pars, obs, nml_file){
  
  calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
  
  all_ee <- matrix(0, nrow=max_r, ncol=length(x0))
  
  if (nrow(calib) %% 2 > 0){
    p <- nrow(calib) *2
  } else {
    p <- nrow(calib) *2
  }
  #p = nrow(calib) *2 # should be even
  del = p/(2*(p-1))
  
  for (r in 1:max_r){
    ee <- random_sampling(x0,p,del,lb,ub)
    ee[is.na(ee)] <- ee[which.max(abs(ee))] #replace NA's with max value
    all_ee[r,] <- ee
  }
  
  ee_norm <- (abs(all_ee) - min(abs(all_ee)))/(max(abs(all_ee)) - min(abs(all_ee)))
  
  morris_res <- data.frame('pars'=c(pars), 'mean' = apply(abs(all_ee),2,mean), 'std' = apply(all_ee,2,sd))
  colnames(all_ee) <- pars
  
  write.csv(all_ee, paste0('results/SA_ee_results_',var,'.csv'), quote = F, row.names = F)
  
  morris_norm <- data.frame('pars'=c(pars), 'mean' = apply(abs(ee_norm),2,mean), 'std' = apply(ee_norm,2,sd))
  p6 <- ggplot(morris_norm, aes(pars,mean))+ #EDIT THIS- Originally I had it as (morris_res, aes(pars,mean))
    geom_bar(stat="identity", fill = 'blue')+
    geom_hline(yintercept = 0.1, colour = 'red', linetype = 'dashed')+
    ylab('Normalized Mean')+
    xlab('Parameters')+
    ggtitle('Normalized Sensitivity')+
    theme_bw()
  
  ggsave(file=paste0('results/SA_plot_',var,'.png'), p6, dpi = 300,width = 384,height = 216, units = 'mm') #saves g
  
  
  morris_cluster = morris_res[,c('mean','std')]
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:(nrow(morris_cluster)-1)
  mean_ss <- k.values*0
  for (k in k.values){
    km.res <- kmeans(morris_cluster, centers = k, nstart = 25)
    ss <- cluster::silhouette(km.res$cluster, dist(morris_cluster))
    mean_ss[k-1] <- mean(ss[, 3])
  }
  
  k_means <- kmeans(morris_cluster,k.values[index(max(mean_ss))])
  morris_res_clust <- data.frame(morris_res, "cluster" = k_means$cluster)
  
  p7 <- ggplot(morris_res_clust, aes(mean, std, col= factor(cluster), shape = pars))+
    geom_point()+
    scale_shape_manual(values = seq(0,20,1))+
    ylab('std EE')+
    xlab('mean EE')+
    ggtitle('Sensitivity')+
    theme_bw()
  ggsave(file=paste0('results/SA_plot_',var,'-clust.png'), p7, dpi = 300,width = 150,height = 150, units = 'mm') #saves g
  
  cal_pars = calib
  
  cal_pars = calib[c(which(morris_norm$mean >= 0.1)),]
  
  write.csv(cal_pars, paste0('calibration_file_',var,'.csv'), row.names = F, quote = F)
  
  return()
}


random_sampling <- function(x0,p,del,lb,ub){
  k <- length(x0)
  m <- k+1
  
  B <- matrix(0, m, k)
  B[lower.tri(B)] <- 1
  
  D_star <- diag(plus_minus_one(k), k, k)
  J_mk <- matrix( 1, m, k)
  
  #(1/2)*((2*B-J_mk)%*%D_star+J_mk)  
  
  x_star <- sample(random_value(p,del),k)
  
  P_star <- matrix(0,k,k)
  sample_P_star <- sample(k)
  for (j in 1:ncol(P_star)){
    P_star[sample_P_star[j],j] <- 1
  }
  
  J_mk_x_star <- matrix(rep(x_star, each = m),m,k)
  
  # J_mk %*% x_star
  
  B_star <- (J_mk_x_star+ (del/2) * ((2 * B - J_mk) %*% D_star + J_mk)) %*% P_star
  
  p = c(B_star[1,])#,lb,ub)
  
  #glmFUNsa(p)
  
  result_matrix <- matrix(0,nrow(B_star),1)
  for (j in 1:nrow(result_matrix)){
    result_matrix[j,]<- glmFUNsa(c(B_star[j,]))
  }
  
  ee <- matrix(0,ncol=k,nrow=1)
  for (i in 1:ncol(ee)){
    ind <- which(diff(B_star[,i])!=0)
    if (diff(B_star[,1])[ind] > 0){
      ee[i] <- (result_matrix[ind+1] - result_matrix[ind])/del}
    else {
      ee[i] <- (result_matrix[ind] - result_matrix[ind+1])/del
    }
  }
  return(ee)
}
  
glmFUNsa <- function(p){
  #Catch non-numeric arguments
  if(!is.numeric(p)){
    p = values.optim
  }
  
  p <- wrapper_scales(p, lb, ub)
  
  if(ends_with_csv(nml_file)) {
    eg_nml <- as.list(read.csv(paste0("aed/", nml_file)) |> 
      rename(zoop_name = X.zoop_name., #renaming zoop cols HERE
             ZOO_rotifer = X.rotifer.,
             ZOO_cladoceran = X.cladoceran.,
             ZOO_copepod = X.copepod.) |> 
      select(c(zoop_name,var)) |> 
      mutate(zoop_name = removeQuotes(zoop_name),
             ZOO_cladoceran = removeQuotes(ZOO_cladoceran)))
      
  } else {
  eg_nml <- read_nml(nml_file = nml_file)
  }
  
  for(i in 1:length(pars[!duplicated(pars)])){
    if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
      eg_nml <- set_nml_csv(eg_nml, pars[!duplicated(pars)][i], 
                        p[which(pars[!duplicated(pars)][i] == pars)])
    } else {
      eg_nml <- set_nml_csv(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
    }
  }
  
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  
  run_glm(os) #changed from Unix 
  
  if(ends_with_csv(nml_file)){
    suppressWarnings(mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)) #Supressed warnings  
    
    
  } else {
  suppressWarnings(mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)) #Supressed warnings
}
  
  fit = sum((mod[,3] - obs[,3])^2,na.rm = T)
  
  print(paste('SAE', round(fit,1)))
  return(fit)
} 

#glm_nml <- nml_file

set_nml_csv <- function(glm_nml, arg_name, arg_val, arg_list = NULL) {
  if (missing(arg_name) & missing(arg_val)) {
    return(setnmlList(glm_nml, arg_list))
  }
  if (!is.character(arg_name)) {
    stop("arg_name should be a character")
  }
  if (!is.null(arg_list) & arg_name %in% names(arg_list)) {
    warning(c("duplicate names given to arg_name and arg_list.", 
              " arg_name and arg_val values will overwrite duplicate arg_list values."))
    glm_nml <- setnmlList(glm_nml, arg_list)
  }
  currVal <- get_nml_csv_value(glm_nml, arg_name, warn = FALSE)
  typeError <- paste0("input ", arg_name, " must be of same data type as current value")
  if (is.logical(currVal) & !is.logical(arg_val)) {
    stop(c(typeError, " (logical)"))
  }
  else if (is.character(currVal) & !is.character(arg_val)) {
    stop(c(typeError, " (character)"))
  }
  else if (is.numeric(currVal) & !is.numeric(arg_val)) {
    stop(c(typeError, " (numeric)"))
  }
  blck <- get_block(glm_nml, arg_name)
  arg_name <- get_arg_name(arg_name)
  if (length(arg_val) > 1 & is.character(arg_val)) {
    arg_val <- paste0(arg_val, collapse = ",")
  }

# Check if the argument value is a file path and ends with ".csv"
if (ends_with_csv(nml_file)) {
  # If yes, read the CSV file and assign its contents
  glm_nml <- as.list(read.csv(paste0("aed/",nml_file)))
  glm_nml[["ZOO_cladoceran"]][[blck]] <- arg_val
} else {
  # Otherwise, just assign the argument value directly
  glm_nml[[blck]][[arg_name]] <- arg_val
}

return(glm_nml)
}

setnmlList <- function (glm_nml, arg_list) 
{
  if (!is.list(arg_list)) {
    stop("arg_list must be a list")
  }
  if (any(nchar(names(arg_list)) == 0) | length(names(arg_list)) == 
      0) {
    stop("arg_list must be a named list")
  }
  arg_names <- names(arg_list)
  for (i in seq_len(length(arg_names))) {
    glm_nml <- set_nml_csv(glm_nml, arg_name = arg_names[i], 
                       arg_val = arg_list[[i]])
  }
  return(glm_nml)
}



get_nml_csv_value <- function (glm_nml = NA, arg_name, nml_file = "template", ...) 
{
  #if (!all(is.na(glm_nml)) & nml_file != "template") {
  #  stop("Must specify either an nml object via 'glm_nml' or \n         an nml file path via 'nml_file'")
  #}
  
  if(ends_with_csv(nml_file)) {
    if (all(is.na(nml_file))) {
      glm_nml <- as.list(read.csv(paste0("aed/",nml_file)) |> 
        rename(zoop_name = X.zoop_name., #renaming zoop cols HERE
               ZOO_rotifer = X.rotifer.,
               ZOO_cladoceran = X.cladoceran.,
               ZOO_copepod = X.copepod.) |> 
        select(c(zoop_name,var)) |> 
        mutate(zoop_name = removeQuotes(zoop_name),
               ZOO_cladoceran = removeQuotes(ZOO_cladoceran)))
    }
  } else {
  if (all(is.na(nml_file))) {
    glm_nml <- read_nml(nml_file)
  }
  }
  blck <- get_block(glm_nml, arg_name)
  arg_name <- get_arg_name(arg_name)
  
  # Check if the file path ends with ".csv"
  if (ends_with_csv(nml_file)) {
    # If yes, read the CSV file and return its contents
    return(glm_nml[["ZOO_cladoceran"]][[blck]])
  } else {
    # Otherwise, return the argument value directly
    return(glm_nml[[blck]][[arg_name]])
  }
}

get_block <- function (glm_nml, arg_name, warn = TRUE) 
{
  arg_split = strsplit(arg_name, "::")[[1]]
  if (length(arg_split) > 1) {
    blck = arg_split[1]
    arg_name = get_arg_name(arg_name)
  }
  else {
    blck <- findBlck(glm_nml, arg_name)
  }
  if (length(blck) > 1) {
    if (warn) 
      warning(arg_name, " found in ", paste(names(glm_nml[blck]), 
                                            collapse = " & "), ", returning the first. Try ", 
              names(glm_nml[blck])[1], "::", arg_name, " for explicit match")
    blck = blck[1]
  }
  return(blck)
}

findBlck <- function (nml, argName) 
{
  if (!is.character(argName)) {
    stop(c("parameter name must be a string"))
  }
  fau <- " "
  fault.string <- rep(fau, 1000)
  blckI <- c()
  
  if(ends_with_csv(nml_file)) {
    blockNames <- glm_nml$zoop_name 
    
    for (i in seq_len(length(blockNames))) {
      if (any(argName %in% glm_nml$zoop_name[[i]])) {
        blckI <- c(blckI, i)
      }
      else {
        one.i <- which(fault.string == fau)[1]
        fault.string[one.i:(one.i + length(glm_nml$zoop_name[[i]]) - 
                              1)] = glm_nml$zoop_name[[i]]
      }
    }
  } else {
    blockNames <- names(nml)
  for (i in seq_len(length(blockNames))) {
    if (any(argName %in% names(nml[[i]]))) {
      blckI <- c(blckI, i)
    }
    else {
      one.i <- which(fault.string == fau)[1]
      fault.string[one.i:(one.i + length(names(nml[[i]])) - 
                            1)] = names(nml[[i]])
    }
  }
  
}
  
  fault.string <- fault.string[!fault.string == fau]
  if (is.null(blckI)) {
    stop(c("parameter name ", argName, " not found in nml. Possible names:", 
           paste(fault.string, collapse = ", ")))
  }
  return(blckI)
}


get_arg_name <- function (arg_name) 
{
  arg_split = strsplit(arg_name, "::")[[1]]
  if (length(arg_split) > 1) {
    blck = arg_split[1]
    arg_name = arg_split[2]
  }
  return(arg_name)
}























file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
var = 'ZOO_cladoceran'
calib <- matrix(c('par', 'lb', 'ub', 'x0', 
                  'Rgrz_zoo', 0.2, 1.5, 0.5,
                  'theta_grz_zoo', 1.01, 1.1, 1.02,
                  'Rresp_zoo', 0.08, 0.3, 0.2,
                  'Rmort_zoo', 0.01, 0.1, 0.01,
                  'theta_resp_zoo', 1.01, 1.1, 1.04,
                  'Cmin_grz_zoo', 1, 5, 2
), nrow = 7,ncol = 4, byrow = TRUE) #EDIT THE NROW TO REFLECT # OF ROWS IN ANALYSIS
write.table(calib, file = paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), row.names = FALSE, 
            col.names = FALSE, sep = ',',
            quote = FALSE)
max_r = 3
calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
x0 <- calib$x0
lb <- calib$lb
ub <- calib$ub
pars <- calib$par
obs <- read.csv('field_data/field_zoops.csv') |> 
  mutate(Depth = NA) |> 
  select(DateTime, Depth, var)
nml_file = 'aed_zoop_pars_3groups_28Apr2024.csv'
os = 'Compiled'
run_sensitivity(var, max_r, x0, lb, ub, pars, obs, nml_file)
