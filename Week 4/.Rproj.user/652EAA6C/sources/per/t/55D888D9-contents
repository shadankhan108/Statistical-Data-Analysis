# Ques 1 Define the string with your name, unit name, and task name
my_string <- "Name: Shadan Khan, Unit: Statistical Data Analysis, Task: Calculas Exploratory Week 4"

# Print the string
print(my_string)

#Ques 2

data<-read.csv('weather (2).csv')

#printing head

head(data)

#Ques 3  Print variables names/types, select wind_speed and 1 other variable for the next tasks.

str(data)

selected_data <- data[c("temp", "wind_speed")]

head(selected_data)

# Load necessary libraries
library(ggplot2)
library(fitdistrplus)


#Ques 4 Provide QQ plots and histograms for the selected 2 variables

generate_qq_plots <- function(data, variable_name) {
  # Extract the specified variable data
  variable_data <- data[[variable_name]]
  # Remove NA values
  variable_data <- variable_data[!is.na(variable_data)]
  # Create a data frame with the variable data
  df <- data.frame(variable_data = variable_data)
  # Calculate parameters for the distributions
  mean_var <- mean(variable_data, na.rm = TRUE)
  sd_var <- sd(variable_data, na.rm = TRUE)
  
  # QQ Plot against a Normal Distribution
  qqplot_normal <- ggplot(df, aes(sample = variable_data)) +
    stat_qq(distribution = qnorm, dparams = list(mean = mean_var, sd = sd_var)) +
    stat_qq_line(distribution = qnorm, dparams = list(mean = mean_var, sd = sd_var)) +
    ggtitle(paste("QQ Plot of", variable_name, "vs. Normal Distribution"))
  print(qqplot_normal)
  
  # QQ Plot against an Exponential Distribution
  rate_exp <- 1 / mean_var
  qqplot_exp <- ggplot(df, aes(sample = variable_data)) +
    stat_qq(distribution = qexp, dparams = list(rate = rate_exp)) +
    stat_qq_line(distribution = qexp, dparams = list(rate = rate_exp)) +
    ggtitle(paste("QQ Plot of", variable_name, "vs. Exponential Distribution"))
  print(qqplot_exp)
  
  # QQ Plot against a Log-Normal Distribution
  qqplot_lognorm <- ggplot(df, aes(sample = variable_data)) +
    stat_qq(distribution = qlnorm, dparams = list(meanlog = log(mean_var), sdlog = log(sd_var))) +
    stat_qq_line(distribution = qlnorm, dparams = list(meanlog = log(mean_var), sdlog = log(sd_var))) +
    ggtitle(paste("QQ Plot of", variable_name, "vs. Log-Normal Distribution"))
  print(qqplot_lognorm)
  
  # Estimate shape and scale parameters for Weibull distribution
  fit_weibull <- tryCatch({
    fitdistrplus::fitdist(variable_data[variable_data > 0], "weibull")
  }, error = function(e) {
    warning("Weibull fitting failed: ", e$message)
    return(NULL)
  })
  
  if (!is.null(fit_weibull)) {
    shape <- fit_weibull$estimate["shape"]
    scale <- fit_weibull$estimate["scale"]
    # QQ Plot against a Weibull Distribution
    qqplot_weibull <- ggplot(df, aes(sample = variable_data)) +
      stat_qq(distribution = qweibull, dparams = list(shape = shape, scale = scale)) +
      stat_qq_line(distribution = qweibull, dparams = list(shape = shape, scale = scale)) +
      ggtitle(paste("QQ Plot of", variable_name, "vs. Weibull Distribution"))
    print(qqplot_weibull)
  } else {
    print("Weibull distribution fitting could not be performed due to data issues.")
  }
  
  # QQ Plot against a Uniform Distribution
  qqplot_uniform <- ggplot(df, aes(sample = variable_data)) +
    stat_qq(distribution = qunif, dparams = list(min = min(variable_data, na.rm = TRUE), max = max(variable_data, na.rm = TRUE))) +
    stat_qq_line(distribution = qunif, dparams = list(min = min(variable_data, na.rm = TRUE), max = max(variable_data, na.rm = TRUE))) +
    ggtitle(paste("QQ Plot of", variable_name, "vs. Uniform Distribution"))
  print(qqplot_uniform)
}

generate_qq_plots(data,"temp")

#Generate the QQ plot without removing the outliers
generate_qq_plots(data,"wind_speed")  

# We now remove the outliers and plot the QQ plot
wind_speed <- data$wind_speed
wind_speed <- wind_speed[!is.na(wind_speed) & !is.nan(wind_speed)]
# Remove outliers using the IQR method
Q1 <- quantile(wind_speed, 0.25, na.rm = TRUE)
Q3 <- quantile(wind_speed, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
wind_speed_filtered<- wind_speed[wind_speed >= lower_bound & wind_speed <= upper_bound]
# Create a data frame for wind_speed_filtered
wind_speed_filtered_df <- data.frame(wind_speed_filtered = wind_speed_filtered)
# Call the generate_qq_plots function
generate_qq_plots(wind_speed_filtered_df, "wind_speed_filtered")

#We now plot the histograms 
print(hist(wind_speed_filtered))

#we not plot the temperature histogram.
temp <- data$temp
print(hist(temp))

#Ques 5 For one of the variables, generate simulated data using an appropriate distribution and
#compare the Cullen and Frey graph with that for the original distribution. Comment on
#any notable differences.
#Note: You will need to determine the required parameters appropriate for your
#distribution (and how to estimate them from the original data). A list of these can be
#found by searching for ‘distributions’ in the R Studio help tab. Remember that, e.g.,
#rgamma(…), runif(…) will be the corresponding function that generates the random
#data.  

library(fitdistrplus)

# We extract temperature data and remove NAs
temp <- data$temp
temp <- temp[!is.na(temp)]

# Step 1: We will analyze the Original Data
# Fit a Normal distribution to the original temperature data
fit_temp <- fitdist(temp, "norm")

# We will put Cullen and Frey graph for the original data
descdist(temp, boot = 1000)

# Step 2: We will simulate Data Based on the Fitted Distribution
# Estimate parameters from the fitted Normal distribution
mean_temp <- fit_temp$estimate["mean"]
sd_temp <- fit_temp$estimate["sd"]  

# We will simulate data using the Normal distribution
simulated_temp <- rnorm(length(temp), mean = mean_temp, sd = sd_temp)

# We will simulate data using the Normal distribution
simulated_temp <- rnorm(length(temp), mean = mean_temp, sd = sd_temp)

#Ques 6 For each variable, use the fitdist() function in the "fitdistrplus” package to obtain an
#appropriate MLE fit of your data and display the estimates as well as the empirical vs
#theoretical comparison plots.

library(fitdistrplus)
library(ggplot2)

# Here the function to fit distributions, to find the best fit, and create a plot of the best fit
find_best_distribution <- function(data, variable_name) {
  # Extract the data for the specified variable, removing NA/NaN values
  variable_data <- data[[variable_name]]
  variable_data <- variable_data[!is.na(variable_data) & !is.nan(variable_data)]
  
  # Add a constant to ensure all values are positive for distributions that require it
  constant_add <- 100
  variable_data_positive <- variable_data + constant_add
  # Fit different distributions to the data
  fit_weibull <- fitdist(variable_data_positive, "weibull")
  fit_normal <- fitdist(variable_data, "norm")
  fit_lognormal <- fitdist(variable_data_positive, "lnorm")
  fit_exponential <- fitdist(variable_data_positive, "exp")
  fit_uniform <- fitdist(variable_data, "unif")
  
  # Extract negative log-likelihoods
  nll_weibull <- -fit_weibull$loglik
  nll_normal <- -fit_normal$loglik
  nll_lognormal <- -fit_lognormal$loglik
  nll_exponential <- -fit_exponential$loglik
  nll_uniform <- -fit_uniform$loglik
  # Create a data frame to summarize and compare
  nll_summary <- data.frame(
    Distribution = c("Weibull", "Normal", "Lognormal", "Exponential", "Uniform"),
    NLL = c(nll_weibull, nll_normal, nll_lognormal, nll_exponential, nll_uniform)
  )
  # Identify the distribution with the minimum NLL (best fit)
  best_fit <- nll_summary[which.min(nll_summary$NLL), ]
  # Print the summary and the best fit
  print(nll_summary)
  print(paste("Best fit distribution:", best_fit$Distribution))
  # Plot the best-fitting distribution
  if (best_fit$Distribution == "Weibull") {
    plot(fit_weibull)
  } else if (best_fit$Distribution == "Normal") {
    plot(fit_normal)
  } else if (best_fit$Distribution == "Lognormal") {
    plot(fit_lognormal)
  } else if (best_fit$Distribution == "Exponential") {
    plot(fit_exponential)
  } else if (best_fit$Distribution == "Uniform") {
    plot(fit_uniform)
  }
  return(list(
    nll_summary = nll_summary,
    best_fit_distribution = best_fit$Distribution
  ))
}

find_best_distribution(data, "temp")




