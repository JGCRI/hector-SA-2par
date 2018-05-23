# Purpose: Calculate the Dn statistics for each individual observational data set 

# 0. Set Up ------------------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}


# Load required libs
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# Define the intermediate output sub directory
sub_dir <- 'rcp26'

# Set up directoires 
BASE           <- getwd() 
INT_OUTPUT_DIR <- file.path(BASE, 'int-out', sub_dir)

# 1. Define Functions ------------------------------------------------------------------------
# Dn_func: is a function that will calculate the Dn distance stat for an observation and model 
# comparison. The input data table requires the run_name, observational and model data, and a 
# s2n column that contains a sample size variability. 

Dn_func <- function(data){
  
  # Check for required columns
  req_columns <- c('run_name', 'obs', 'model', 's2n')
  if( any(!req_columns %in% names(data)) ){stop("Missing 1 or more required columns.")}
  
  # Calculate Dn 
  n  <- nrow(data)
  Dn <- (1 / n) * sum( ( (data[['obs']] - data[['model']]) ^2 ) / data[['s2n']])
  
  # Return Dn as a tibble
  run_name <- unique(data$run_name)
  tibble(run_name = run_name, Dn = Dn)
  
}
  

# Dc_func: is a function that calculates Dc or the Dn cut off value for 
# significance based on the gamma distribution. The input data frame required 
# a run_name, observational data, Dn, and variability columns. The alpha input 
# is used to determine Dc based on the PDF probability.

Dc_func <- function(data, alpha = 0.01){
  
  # Check for required columns
  req_columns <- c('run_name', 'obs', 'Dn', 's2n')
  if( any(! req_columns %in% names(data)) ){stop("Missing 1 or more required columns.")}
  
  
  # Find n and a based on the number of observations and 
  # model points being compared.
  n  <- nrow(data)
  a  <- n / 2
  
  # Parse out Dn from the input data frame.
  Dn <- unique(data[['Dn']])
  
  # Determing the model temporal varience for each variable
  data %>%  
    group_by(run_name, variable) %>% 
    summarise(model_var = var(model)) %>% 
    ungroup -> 
    model_var_df 
  
  data %>% 
    full_join(model_var_df, by = c('run_name', 'variable')) -> 
    data
    
  
  # Find b, right now we are assuming that there is no 
  # variability in Hector output but there is temporal 
  # variability in the observational data.
  si2        <- data[['s2n']]
  sigma_diff <- data[['temporal_vari']] #data[['model_var']] + si2 # + var(data[['model']]) - 2 * cov(data[['obs']], data[['model']])
#  sigma_diff2 <- var(data[['model']])
#  sigma_diff3 <- var(data[['model']]) + var(data[['obs']]) - 2 * cov(data[['obs']], data[['model']])
  
  b          <- (1/n^2) * sum(sigma_diff * 2 / si2)
 # b2         <- (1/n^2) * sum(sigma_diff2 / si2)
 # b3         <- (1/n^2) * sum(sigma_diff3 / si2)
  
  # Find the Dn cutoff value based on the 
  # gamma distribution.
 # x  <- seq(0, Dn^2, length.out = 1000)
#  bin <- diff(x)[1]
  
  Dc <- qgamma((1 - alpha), shape = a, scale = b, lower.tail = TRUE)
  
  # map(x, function(D){ 
  #   G <- ((D ^ (a - 1)) * exp(- D / b)) / (gamma(a) * b ^ a)
  #   tibble(G = G)
  # }) %>%  
  #   bind_rows %>% 
  #   mutate(G = bin * G) ->
  #   G
  # 
  # close_alpha <- abs((1 - alpha) - cumsum(G$G))
  # index <- which.min(close_alpha)
  My_Dc <- NA

  
  # Format output into a tibble
  run_name <- unique(data[['run_name']])
  tibble(run_name = run_name, Dn = Dn, Dc =  Dc, a = a, b = b)
  
}


# 2. Import Dn input tables ----------------------------------------------------------------
# Import the various Dn files that were created in part G.

path <- list.files(INT_OUTPUT_DIR, "G.mean_Tgav_Dn_input_table.csv", full.names = T)
Tgav_Dn_input <- read.csv(path, stringsAsFactors = FALSE)

path <- list.files(INT_OUTPUT_DIR, "G.NOAA_CO2_Dn_input_table.csv", full.names = T)
CO2_Dn_input <- read.csv(path, stringsAsFactors = FALSE)

path <- list.files(INT_OUTPUT_DIR, "G.CDIAC_LandFlux_Dn_input_table.csv", full.names = T)
LandFlux_Dn_input <- read.csv(path, stringsAsFactors = FALSE)


# # What is bigger the varience or the standard deviation?? 
# Tgav_sd  <- sd(Tgav_Dn_input$obs)
# Tgav_var <- var(Tgav_Dn_input$obs)
# 
# Tgav_sd^2  == Tgav_var

# Okay so varience is sd * sd... which is really annoying when sd is a fraction... because 
# squaring it makes it smaller! So I think that I would like to try the Dn metrics 
# using the sd. In theory I don't think is really matters so long as we are consistent 
# with the values we use... 


# 3. Calculate Dn ----------------------------------------------------------------
# Find the Dn for every single Hector run by mapping the Dn_func to every
# run_name data frame.

Tgav_Dn_input %>%  
  split(.$run_name) %>% 
  # Find the Dn for every single run_name by mapping the Dn_func to every
  # run_name data frame.
  map(., function(data = .){ Dn_func(data) }) %>% 
  bind_rows -> 
  Tgav_Dn

CO2_Dn_input %>% 
  split(.$run_name) %>% 
  map(., function(data = .){ Dn_func(data) }) %>% 
  bind_rows -> 
  CO2_Dn

LandFlux_Dn_input %>% 
  split(.$run_name) %>% 
  map(., function(data = .){ Dn_func(data) }) %>% 
  bind_rows -> 
  LandFlux_Dn

# Calculate Dn for both CO2 and LandFlux
CO2_Dn_input %>% 
  bind_rows(LandFlux_Dn_input) %>% 
  split(.$run_name) %>% 
  map_dfr(., function(data = .){ Dn_func(data) }) -> 
  LandFlux_CO2_Dn
  

# # Sanity check look at the Dn results 
# summary(CO2_Dn$Dn)
# summary(Tgav_Dn$Dn)
# summary(LandFlux_Dn$Dn)
# 
# 
# hist(Tgav_Dn$Dn, breaks = 1000)      # This is sort of what we would expect the gamma distribtuion to look like! 
# hist(CO2_Dn$Dn, breaks = 1000)      # This is not really a great gamma distribtuion but I guess the shape does vary
# hist(LandFlux_Dn$Dn, breaks = 1000) # This is a strange looking gamma distribtion as well, it looks exponential. 
# 


# 4. Calculate Dc ----------------------------------------------------------------

# First we are going to have to create the Dc inptus tables by adding the 
# Dn column to to Dn input tables. 

Tgav_Dc_input     <- full_join(Tgav_Dn_input, Tgav_Dn, by = "run_name")
CO2_Dc_input      <- full_join(CO2_Dn_input, CO2_Dn, by = "run_name")
LandFlux_Dc_input <- full_join(LandFlux_Dn_input, LandFlux_Dn, by = "run_name")


# Find the Dc for every single Hector run by mapping the Dc_func to every
# run_name data frame.

Tgav_Dc_input %>% 
  mutate(variable = 'Tgav') %>% 
  split(.$run_name) %>%  
  map(., function(data = .){ Dc_func(data) }) %>% 
  bind_rows -> 
  Tgav_Dn_Dc

CO2_Dc_input %>% 
  mutate(variable = 'CO2') %>% 
  split(.$run_name) %>%  
  map(., function(data = .){ Dc_func(data) }) %>% 
  bind_rows -> 
  CO2_Dn_Dc

LandFlux_Dc_input %>% 
  mutate(variable = 'LandFlux') %>% 
  split(.$run_name) %>% 
  map(., function(data = .){ Dc_func(data) }) %>% 
  bind_rows -> 
  LandFlux_Dn_Dc


bind_rows(LandFlux_Dn_input %>%  
            full_join(LandFlux_CO2_Dn, by = 'run_name') %>% 
            mutate(variable = 'LandFlux'), 
          CO2_Dn_input %>% 
            full_join(LandFlux_CO2_Dn, by = 'run_name') %>% 
            mutate(variable = 'CO2')) %>% 
  split(.$run_name) %>% 
  map_dfr(.,  function(data = .){ Dc_func(data) }) -> 
  LandFlux_CO2_Dn_Dc

  


# 5. Hector Dn vs Obs ----------------------------------------------------------------

# Tgav
ordered     <- arrange(Tgav_Dn, Dn)
top_10_tgav <- ordered$run_name[1:10]
sifnig_runs <- filter(Tgav_Dn_Dc, Dn < Dc)

ggplot() + 
  geom_line(data = Tgav_Dc_input, aes(year, model, color = "All Hector Runs", group = run_name)) + 
  geom_line(data = filter(Tgav_Dc_input, run_name %in% sifnig_runs$run_name), aes(year, model, color = "Dn Matches Obs", group = run_name)) + 
  geom_line(data = filter(Tgav_Dc_input, run_name %in% top_10_tgav), aes(year, model, color = "Top 10 Dn Scores", group = run_name)) + 
  geom_ribbon(data = Tgav_Dc_input, aes(year, ymin = obs - s2n, ymax = obs + s2n, fill = "Obs", color = "Obs"), alpha = 0.5) + 
  labs(title = "Tgav Obs vs Hector", 
       caption = paste0("Number of Dn matching runs ", length(sifnig_runs$run_name))) + 
  scale_color_manual(values = c("grey", "purple","red", "black")) + 
  scale_fill_manual(values = "red") + 
  theme_bw() -> 
  fig
ggsave(filename = file.path('methods_check', 'Dn', 'Tgav_Hector_obs.png'))


# CO2
ordered     <- arrange(CO2_Dn, Dn)
top_10_CO2 <- ordered$run_name[1:10]
sifnig_runs <- filter(CO2_Dn_Dc, Dn < Dc)

ggplot() + 
  geom_line(data = CO2_Dc_input, aes(year, model, color = "All Hector Runs", group = run_name)) + 
  geom_line(data = filter(CO2_Dc_input, run_name %in% sifnig_runs$run_name), aes(year, model, color = "Dn Matches Obs", group = run_name)) + 
  geom_line(data = filter(CO2_Dc_input, run_name %in% top_10_CO2), aes(year, model, color = "Top 10 Dn Scores", group = run_name)) + 
  geom_ribbon(data = CO2_Dc_input, aes(year, ymin = obs - s2n, ymax = obs + s2n, fill = "Obs", color = "Obs"), alpha = 0.5) + 
  labs(title = "CO2 Obs vs Hector", 
       caption = paste0("Number of Dn matching runs ", length(sifnig_runs$run_name))) + 
  scale_color_manual(values = c("grey", "purple","red", "black")) + 
  scale_fill_manual(values = "red") + 
  theme_bw() -> 
  fig
ggsave(filename = file.path('methods_check', 'Dn', 'CO2_Hector_obs.png'))


# LandFlux
ordered     <- arrange(LandFlux_Dn, Dn)
top_10_LandFlux <- ordered$run_name[1:10]
sifnig_runs <- filter(LandFlux_Dn_Dc, Dn < Dc)

ggplot() + 
  geom_line(data = LandFlux_Dc_input, aes(year, model, color = "All Hector Runs", group = run_name)) + 
 # geom_line(data = filter(LandFlux_Dc_input, run_name %in% sifnig_runs$run_name), aes(year, model, color = "Dn Matches Obs", group = run_name)) + 
  geom_line(data = filter(LandFlux_Dc_input, run_name %in% top_10_LandFlux), aes(year, model, color = "Top 10 Dn Scores", group = run_name)) + 
  geom_ribbon(data = LandFlux_Dc_input, aes(year, ymin = obs - s2n, ymax = obs + s2n, fill = "Obs", color = "Obs"), alpha = 0.5) + 
  labs(title = "LandFlux Obs vs Hector", 
       caption = paste0("Number of Dn matching runs : 0")) + 
  scale_color_manual(values = c("grey", "red", "black")) + 
  scale_fill_manual(values = "red") + 
  theme_bw() -> 
  fig
ggsave(filename = file.path('methods_check', 'Dn', 'LandFlux_Hector_obs.png'))



# 6. Hector Dn vs Obs ----------------------------------------------------------------

# What is going on... I think that there are some issues with how the gamma is being 
# distribtued... .


x <- seq(0, 100, length.out = 100)
a <- unique(Tgav, v_Dn_Dc$a)
b <- unique(Tgav_Dn_Dc$b)[10]


G <- dgamma(x, shape = a, scale = b)
G_data <- tibble(G = G, x = x)

b     <- unique(Tgav_Dn_Dc$b2)[1]
G      <- dgamma(x, shape = a, scale = b2 )
G_data2 <- tibble(G = G, x = x)

b       <- unique(Tgav_Dn_Dc$b3)[3]
G       <- dgamma(x, shape = a, scale = b)
G_data3 <- tibble(G = G, x = x)

plot(x = G_data$x, y = G_data$G, type = 'l', col = 'red',
     ylim = c(0, 0.5), xlim = c(0, 60),
     main = 'How is the gamma distribution impacted \nby sigma diff choice?', 
     xlab = 'Dn', 
     ylab = 'p')
lines(x = G_data2$x, y = G_data2$G, col = 'blue')
lines(x = G_data3$x, y = G_data3$G, col = 'black')
legend('topright', c('model temporal & obs', 'only model variation', 'model and obs varience and co varience'), lty = 1, col = c('red', 'blue', 'black'))

save.image(file = file.path('methods_check', 'Dn', 'Gammas.png'))




# 7. Top ranked Hector Runs -------------------------------------------------------------------------------

arranged_LandFlux_CO2 <- arrange(LandFlux_CO2_Dn, Dn) 
top_10_LandFlux_CO2   <- arranged_LandFlux_CO2$run_name[1:10]


bind_rows(filter(Tgav_Dn_input, run_name %in% top_10_tgav) %>% 
            mutate(rank = 'top 10 Tgav') , 
          
          filter(Tgav_Dn_input, run_name %in% top_10_CO2) %>% 
            mutate(rank = 'top 10 CO2'), 
          
          filter(Tgav_Dn_input, run_name %in% top_10_LandFlux) %>% 
            mutate(rank = 'top 10 LandFlux') , 
          
          filter(Tgav_Dn_input, run_name %in% top_10_LandFlux_CO2) %>% 
            mutate(rank = 'top 10 LandFlux CO2')
          ) -> 
  top_10_df



ggplot(top_10_df) + 
  geom_line(aes(year, model, color = rank, group = run_name)) + 
  facet_wrap('rank', nrow = 1) + 
  theme_bw() -> 
  fig

ggsave(filename = file.path('methods_check', 'Dn', 'top_10_ranking.png'))



# 8. Run Count Figure ------------------------------------------------------------------------------------
bind_rows(mutate(Tgav_Dn_Dc, match = if_else(Dn <= Dc, 1, 0), obs = 'Tgav'), 
          mutate(CO2_Dn_Dc, match = if_else(Dn <= Dc, 1, 0), obs = 'CO2'), 
          mutate(LandFlux_Dn_Dc, match = if_else(Dn <= Dc, 1, 0), obs = 'LandFlux'), 
          mutate(LandFlux_CO2_Dn_Dc, match = if_else(Dn <= Dc, 1, 0), obs = 'LandFlux & CO2')) %>% 
  group_by(obs) %>% 
  summarise(run_count = sum(match)) %>% 
  ungroup -> 
  run_count_df


ggplot(run_count_df) + 
  geom_col(aes(obs, run_count, color = obs, fill = obs)) + 
  geom_text(aes(obs, run_count, label = run_count), position = position_dodge(0.9), 
            vjust=-.2) + 
  labs(title = 'Matching model run and obs data run count', 
       x = 'obs data', 
       y = 'number of matching runs based on Dn stats') -> 
  fig

ggsave(filename = file.path('methods_check', 'Dn', 'run_count.png'))


