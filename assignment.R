# === SETUP ===

# import packages
library(tidyverse)
library(ggplot2)
library(readr)

# === INPUT ===

# load data
data <- read_csv('A1_DMS_store_data_2026.csv')

# === TRANSFORMATION ===

# create CTR variable
data <- data%>%
	mutate(
		ctr = clicks/impressions
	)

# create CR variable
data <- data%>%
	mutate(
		cr = conversions/clicks
	)

# create CTR histogram

# determine binwidth

# calculate q1
q1 <- quantile(data$ctr, .25, na.rm = TRUE)

# calculate q2
q3 <- quantile(data$ctr, .75, na.rm = TRUE)

# calculate iqr
ctr_iqr <- q3-q1

# calculate binwidth with freedman formula
ctr_freedman_diaconis <- (2*ctr_iqr)/(nrow(data[data$impressions!=0, ])^(1/3))

data_ctr <- data%>%
	filter(impressions != 0)%>%
	# no impressions means possible click through, data is not useful
	filter(ctr <= 1) # ctr bigger than 1 means flawed data, good to filter out
	
ctr_histogram <- ggplot(data = data_ctr, aes(x = ctr)) +
	geom_histogram(
		binwidth = ctr_freedman_diaconis,
		fill = "steelblue",
		color = "white"
	) +
	labs(
		title = "Distribution of Click-Through Rate",
		x = "Click-Through Rate",
		y = "Frequency"
	) +
	scale_x_continuous(
		breaks = seq(0, 1, by = 0.05),
		labels = scales::percent
	) +
	theme_classic() +
	theme(
		axis.text.x = element_text(angle = 45, hjust = 1)
	)

ctr_histogram

# Create CR histogram

# filter data for CR analysis
data_cr <- data %>% 
	filter( clicks != 0)%>% # conversion is conditional on there being clicks
	filter(!is.na(cr))%>% # just to be safe
	filter(cr <=1) # just to be very safe

# calculate q1 and q3
q1_cr <- quantile(data_cr$cr, .25, na.rm = TRUE)
q3_cr <- quantile(data_cr$cr, .75, na.rm = TRUE)
iqr_cr <- q3_cr-q1_cr

# calculate binwidth with freedman formula
cr_freedman_diaconis <- (2*iqr_cr)/(nrow(data_cr)^(1/3))

# Create histogram
cr_histogram <- ggplot(data = data_cr, aes(x = cr)) +
	geom_histogram(
		binwidth = cr_freedman_diaconis*10,
		fill = "steelblue",
		color = "white"
	) +
	labs(
		title = "Distribution of Conversion Rate",
		x = "Conversion Rate",
		y = "Frequency"
	) +
	scale_x_continuous(
		breaks = seq(0, .3, by = 0.02),
		labels = scales::percent
	) +
	theme_classic() +
	theme(
		axis.text.x = element_text(angle = 45, hjust = 1)
	)+ coord_cartesian(xlim = c(0, 0.30))

cr_histogram

# average CTR and CR calculations
ctr_raw <- mean(data_ctr$ctr)
cr_raw <- mean(data_cr$cr)

# weighed averages
ctr_weighed <- sum(data_ctr$clicks)/sum(data_ctr$impressions)
cr_weighed <- sum(data_cr$conversions)/sum(data_cr$clicks)

# averages excluding campaigns with very low impressions for ctr and clicks for cr
ctr_cutoff_imp <- quantile(data_ctr$impressions, 0.10, na.rm = TRUE)
data_ctr_trimmed <- data_ctr[data_ctr$impressions > ctr_cutoff_imp, ]
ctr_mean_trimmed <- mean(data_ctr_trimmed$ctr, na.rm = TRUE)

cr_cutoff_clicks <- quantile(data_cr$clicks, 0.10, na.rm = TRUE)
data_cr_trimmed <- data_cr[data_cr$clicks > cr_cutoff_clicks, ]
cr_mean_trimmed <- mean(data_cr_trimmed$cr, na.rm = TRUE)
