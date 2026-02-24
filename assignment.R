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
		fill = "indianred",
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

ggsave(
	"ctr_plot.png",
	width = 8,
	height = 6,
	dpi = 300
)

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
		binwidth = cr_freedman_diaconis*5,
		fill = "indianred",
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
ggsave(
	"cr_plot.png",
	width = 8,
	height = 6,
	dpi = 300
)

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

# Compute correlation between ad rank and ctr and cr
ctr_correlation_adpos <- cor.test(data_ctr$ctr, data_ctr$adPosition, method = 'spearman')
print(ctr_correlation_adpos)

cr_correlation_adpos <- cor.test(data_cr$cr, data_cr$adPosition, method = 'spearman')
print(cr_correlation_adpos)

# Question 1.3
profit_margin <- 0.035

data <- data%>%mutate(
	profits = revenue*profit_margin
)%>%mutate(
	costs = clicks * bidprice
)%>%mutate(
	roi_net = (profits-costs)/costs
)

# average ROI per day/campaign combination
roi_net_avg <- mean(data$roi_net[is.finite(data$roi_net)])
# alternative formula
mean(data$roi_net[data$costs > 0], na.rm = TRUE)

# First aggregate per campaign
roi_grouped <- data %>%
	mutate(
		profits = revenue * 0.035,
		costs   = ifelse(clicks == 0, 0, clicks * bidprice)
	) %>%
	group_by(adID) %>%
	summarise(
		total_profits = sum(profits, na.rm = TRUE),
		total_costs   = sum(costs,   na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(
		roi_campaign = ifelse(
			total_costs > 0,
			(total_profits - total_costs) / total_costs,
			NA_real_
		)
	)

mean_roi_net_per_campaign <- mean(roi_grouped$roi_campaign, na.rm = TRUE)

# Question 2.1

# check values of $numberofwords
table(data_cr$numberofwords)

# data needs to be filtered and only include 2 and 3 keywords
table_cr_21 <- data %>%
	filter(numberofwords %in% c(2, 3), clicks > 0) %>%
	mutate(
		retailer_label = ifelse(retailer == 1, "With Retailer", "Without Retailer"),
		length_label   = paste0("Keyword Length ", numberofwords)
	) %>%
	group_by(length_label, retailer_label) %>%
	summarise(
		avg_cr = mean(cr, na.rm = TRUE),
		n      = n(),
		.groups = "drop"
	) %>%
	tidyr::pivot_wider(names_from = retailer_label, values_from = c(avg_cr, n))

# Question 2.2

data_ctr_23 <- data %>%
	filter(numberofwords %in% c(2, 3),
		   impressions > 0)

table_ctr_22 <- data_ctr_23 %>%
	group_by(numberofwords, retailer, brandname) %>%
	summarise(
		avg_ctr = mean(ctr, na.rm = TRUE),
		.groups = "drop"
	)

table_ctr_final <- table_ctr_22 %>%
	mutate(
		retailer_label = ifelse(retailer == 1, "With Retailer", "Without Retailer"),
		brand_label    = ifelse(brandname == 1, "With Brand", "Without Brand"),
		length_label   = paste0("Keyword Length ", numberofwords)
	) %>%
	unite(col_group, retailer_label, brand_label, sep = " | ") %>%
	select(length_label, col_group, avg_ctr) %>%
	pivot_wider(
		names_from  = col_group,
		values_from = avg_ctr
	)

table_n <- data_ctr_23 %>%
	group_by(numberofwords, retailer, brandname) %>%
	summarise(
		n = n(),
		.groups = "drop"
	) %>%
	mutate(
		retailer_label = ifelse(retailer == 1, "With Retailer", "Without Retailer"),
		brand_label    = ifelse(brandname == 1, "With Brand", "Without Brand"),
		length_label   = paste0("Keyword Length ", numberofwords)
	) %>%
	unite(col_group, retailer_label, brand_label, sep = " | ") %>%
	select(length_label, col_group, n) %>%
	pivot_wider(
		names_from  = col_group,
		values_from = n
	)

# Question 2.3
profit_margin <- 0.035

data_roi <- data %>%
	mutate(
		profits = revenue * profit_margin,
		costs   = ifelse(clicks == 0, 0, clicks * bidprice),
		roi     = (profits - costs) / costs
	)

data_roi_valid <- data_roi %>%
	filter(
		numberofwords %in% c(2, 3),
		costs > 0,
		is.finite(roi)
	)

table_roi <- data_roi_valid %>%
	group_by(numberofwords, retailer, brandname) %>%
	summarise(
		avg_roi = mean(roi, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(
		retailer_label = ifelse(retailer == 1, "With Retailer", "Without Retailer"),
		brand_label    = ifelse(brandname == 1, "With Brand", "Without Brand"),
		length_label   = paste0("Keyword Length ", numberofwords)
	) %>%
	tidyr::unite(col_group, retailer_label, brand_label, sep = " | ") %>%
	select(length_label, col_group, avg_roi) %>%
	tidyr::pivot_wider(
		names_from  = col_group,
		values_from = avg_roi
	)
