## This analysis of the decathlon data requires preprocessing
## done elsewhere to extract the first-season season best marks
## for each athlete for each event as well as all-time PR for
## each athlete in each year.
##
## Is that right? Or, should we take their best year for their
## overall score and get the best mark from that?
##
## Either way, this analysis will implement one or multiple
## generalized linear model(s) for analyzing said data.
##
## Note: SP and DT switch places when getting coefficient values
## or confidence intervals from the model, in spite of 

# Load the data.
# https://stackoverflow.com/questions/13265153/how-do-i-import-a-csv-file-in-r
library(statmod)
library(MASS)
library(DescTools)
library(rstatix)
library(car)

# Still need to add references for these.
library(broom)
library(knitr)
library(ggplot2)
library(data.table)
library(multcomp)
library(boot)

# This dataframe actually doesn't contain the bests -- it's only first and delta.
dec_data = read.csv("data_first_best_updated_dist_speed.csv")
dec_data <- dec_data[, -1]  # Remove redundant index column.

# Load the updated data.
# In this version of the script, these are actually the same.
# dec_data_updated <- read.csv("data_first_best_updated_dist_speed.csv")
# dec_data_updated <- dec_data_updated[, -1]

# This contains FSB, best, and delta marks, along with ATPB.
dec_data_all <- read.csv("data_first_best_delta_dist_speed.csv")
dec_data_all <- dec_data_all[, -1]

# Make sure both agree.
# print(all(dec_data == dec_data_updated))
# 
# print(all(dec_data == dec_data_all))

# Build formula.
# https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
build_formula <- function(outcome, predictors) {
	# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/paste.html
	form = paste(outcome, "~", paste(predictors, collapse=" + "))
	return(form)
}

# Define variable names to relate in the equations.
outcome = "All_Time_PB"
predictors <- c("Men_100_first", "Men_110h_first", "Men_400_first",
				"Men_LJ_first", "Men_HJ_first", "Men_PV_first",
				"Men_SP_first", "Men_DT_first", "Men_JT_first",
				"Men_1500_first",
				"Men_100_delta", "Men_110h_delta", "Men_400_delta",
				"Men_LJ_delta", "Men_HJ_delta", "Men_PV_delta",
				"Men_SP_delta", "Men_DT_delta", "Men_JT_delta",
				"Men_1500_delta")
predictors_first <- c("Men_100_first", "Men_110h_first", "Men_400_first",
					  "Men_LJ_first", "Men_HJ_first", "Men_PV_first",
					  "Men_SP_first", "Men_DT_first", "Men_JT_first",
					  "Men_1500_first")
# predictors_best <- c("Men_100_best", "Men_110h_best", "Men_400_best",
# 					 "Men_LJ_best", "Men_HJ_best", "Men_PV_best",
# 					 "Men_SP_best", "Men_DT_best", "Men_JT_best",
# 					 "Men_1500_best")
predictors_delta <- c("Men_100_delta", "Men_110h_delta", "Men_400_delta",
					  "Men_LJ_delta", "Men_HJ_delta", "Men_PV_delta",
					  "Men_SP_delta", "Men_DT_delta", "Men_JT_delta",
					  "Men_1500_delta")
# predictors_fb <- c(predictors_first, predictors_best)

# These dataframes shouldn't come out equal because the new dataframe contains the track
# events in units of speed as opposed to distance.
dec_data_all_comparison_subset <- dec_data_all[names(dec_data)]
all.equal(dec_data, dec_data_all_comparison_subset)
identical(dec_data, dec_data_all_comparison_subset)

# These come out equal now.
# Old comment: When we do this, they don't. Why?
dec_data_all_comparison_subset <- dec_data_all[c("Competitor", predictors_first, predictors_delta, outcome)]
all.equal(dec_data, dec_data_all_comparison_subset)
identical(dec_data, dec_data_all_comparison_subset)

# This seems strange.
# Update: seems like there are 0 rows where there isn't a match.
dec_data_all_comparison_subset[which(dec_data != dec_data_all_comparison_subset), ]

# Make sure that the portion of the decathlon data already present in
# other dataframes agrees with the largest dataframe.
for (column in c("Competitor", predictors_first, predictors_delta, outcome)) {
	print(paste(column, ": ", all(dec_data[[column]] == dec_data_all[[column]])))
}

# Let's look at the distributions of each value in each event.
par(mfrow=c(2, 5))
for (predictor in predictors_first) {
	hist(dec_data_all[[predictor]], xlab=predictor)
}

# We no longer define `predictors_best`.
# par(mfrow=c(2, 5))
# for (predictor in predictors_best) {
# 	hist(dec_data_all[[predictor]], xlab=predictor)
# }

par(mfrow=c(2, 5))
for (predictor in predictors_delta) {
	hist(dec_data_all[[predictor]], xlab=predictor)
}

# Not used.
boxcox_transform <- function(feat_name, dataset) {
	# First, check to see if any values are below 0 and
	# correct if needed.
	below_0 <- any(dataset[[feat_name]] < 0)
	if (below_0) {
		# If we have values below 0, our additive
		# modifier will consist of the lowest value
		# in the set made positive plus 0.01.
		mod = round(abs(min(dataset[[feat_name]])), 2) + 0.01
	} else {
		mod = 0
	}

	#vals_working = vals + mod
	#print(vals_working)

	# Construct formula and adjust environment appropriately so
	# it can find everything.
	# Perform transformation and extract optimal lambda.
	bc_results <- boxcox(lm((dataset[[feat_name]] + mod) ~ 1, data=dataset),
						 lambda=seq(-10, 20, 1))
	bc_lambda <- round(bc_results$x[which.max(bc_results$y)], 1)

	# Produce the transformed variable.
	vals_new = ((dataset[[feat_name]] + mod) ^ bc_lambda - 1) / bc_lambda

	return(list("vals_new"=vals_new, "mod"=mod))
}

# Let's test out boxcox transformation for the deltas.
boxcox_test_feat_min = abs(min(dec_data$Men_1500_delta)) + 0.01
boxcox_test <- boxcox(lm((dec_data$Men_1500_delta + boxcox_test_feat_min) ~ 1),
					  lambda=seq(-10, 20, 1))
boxcox_test_lambda <- round(boxcox_test$x[which.max(boxcox_test$y)], 1)
boxcox_test_new_feat <- ((dec_data$Men_1500_delta + boxcox_test_feat_min) ^ boxcox_test_lambda - 1) / boxcox_test_lambda
hist(boxcox_test_new_feat)

# Preprocess by scaling variables using a robust scaling technique.
# This technique handles outliers better than other scaling techniques
# whose assumptions are met by our data (e.g., Min-Max scaling, which
# does not require a Gaussian-distributed variable in order validly
# transform).
dec_data_t <- dec_data

mods <- c()
lambdas <- c()

# Box-Cox transform the deltas.
for (pred_delta in predictors_delta) {
	# Determine the smallest value by which we can modify all of the items in the
	# feature without creating an error in boxcox() due to items below 0.
	if (any(dec_data[[pred_delta]] <= 0)) {
		mod <- abs(min(dec_data[[pred_delta]])) + 0.01
	} else {
		mod <- 0
	}
	mods <- c(mods, mod)

	# Perform Box-Cox analysis using lm() and obtain optimal lambda.
	bc_results <- boxcox(lm((dec_data[[pred_delta]] + mod) ~ 1),
						 lambda=seq(-20, 20, 0.1))
	bc_lambda <- round(bc_results$x[which.max(bc_results$y)], 2)
	lambdas <- c(lambdas, bc_lambda)

	# Replace old feature with new transformed feature.
	dec_data_t[[pred_delta]] <- ((dec_data[[pred_delta]] + mod) ^ bc_lambda - 1) / bc_lambda
}

print(head(dec_data_t[predictors_delta]))
hist(dec_data_t$Men_1500_delta)

print("Box-Cox Mods:")
print(mods)

# Robust scaling.
dec_data_t[predictors] <- RobScale(dec_data_t[predictors])

## Log transform the outcome deltas.
#dec_data_t[predictors_delta] <- log(dec_data_t[predictors_delta] + 100)

# Do the same for the full decathlon dataset.
#dec_data_fb_t <- dec_data_all[c(predictors_fb, outcome)]
#dec_data_fb_t[predictors_fb] <- RobScale(dec_data_fb_t[predictors_fb])

# Is there collinearity?
print("Collinearity analysis in transformed data:")
corr = cor(dec_data_t[predictors])
cor_gathered = cor_gather(corr)
cor_gathered_diff_vars = cor_gathered[cor_gathered["var1"] != cor_gathered["var2"],]
# Double-checked this thoroughly.
print(cor_gathered[cor_gathered["cor"] > 0.5,], n=35)

# Also take a look at this (same information but removes correlations
# between a variable and itself):
print(cor_gathered_diff_vars[cor_gathered_diff_vars["cor"] > 0.5,], n=35)

# Same thing but for larger dataframe.
#corr_all = cor(dec_data_all[predictors_fb])
#cor_gathered_all = cor_gather(corr_all)
#print(cor_gathered_all[cor_gathered_all["cor"] > 0.5,], n=64)

# Log-transform predictors.
#dec_data_t <- dec_data

## We need to add a value to our deltas to so that they are all
## positive.
#smallest_delta = min(dec_data_t[predictors_delta]) - 1
#dec_data_t[predictors_delta] <- dec_data_t[predictors_delta] - smallest_delta

## Perform the log transformation.
#dec_data_t[predictors] <- log(dec_data_t[predictors])

form <- build_formula(outcome, predictors)

#form_first_best <- build_formula(outcome, c(predictors_first, predictors_best))

# Take a look at the distributions of the variables so we can see about
# putting them on the same scale for comparison.
#for (predictor in predictors) {
	#shapiro.test(dec_data$predictor)
#}

# Fit gamma GLM to transformed data.
# https://stackoverflow.com/questions/17186670/error-while-testing-glm-with-gamma-family
#mod_gamma <- glm(form, data=dec_data, family=Gamma(link="inverse"),
				 #control=list(trace=TRUE))
# Try the log link.
# I also tried to include only the first-season best marks and it really
# distorted the standardized deviance residuals, the working residuals,
# and the partial residuals. This is in keeping with the fact that one of
# the assumptions of the GLM is that we include all predictors.
mod_gamma <- glm(form, data=dec_data_t, family=Gamma(link="log"),
				 control=list(trace=TRUE))
mod_gamma_inverse <- glm(form, data=dec_data_t, family=Gamma(link="inverse"),
						 control=list(trace=TRUE))
mod_gamma_identity <- glm(form, data=dec_data_t, family=Gamma(link="identity"),
						  control=list(trace=TRUE))

#mod_gamma_inverse_fb <- glm(form_first_best, data=dec_data_fb_t,
							#family=Gamma(link="inverse"),
							#control=list(trace=TRUE))
# https://youtu.be/vGOpEpjz2Ks?si=9NtqM-Gtm4_mnPS8
#bc_results = boxcox(mod_gamma, lambda=seq(-3, 3, 1/10))
#lambda_opt =round(bc_results$x[which(bc_results$y == max(bc_results$y))], 2)

#outcome_bc = paste("(All_Time_PB)^", lambda_opt, sep="")
#form_bc = paste(c(paste(outcome_bc, predictors[1], sep=" ~ "), predictors[2:20]),
				#collapse=" + ")
#mod_gamma_bc <- glm(form_bc, data=dec_data, family=Gamma(link="log"),
				 #control=list(trace=TRUE))

# ================ Estimating Phi ================
# Since the estimate of phi is so small, the saddlepoint approximation will
# be very accurate.
phi.pearson <- summary(mod_gamma)$dispersion
phi.pearson_inverse <- summary(mod_gamma_inverse)$dispersion
phi.pearson_identity <- summary(mod_gamma_identity)$dispersion

# ================ Summary and Coefficients ================
# Coefficients for all predictors were significantly related to the
# outcome variable.
summary(mod_gamma)
summary(mod_gamma_inverse)
summary(mod_gamma_identity)

# ================ Collinearity ================
print("Variance Inflation Factors:")
mod_vif <- vif(mod_gamma)
print(mod_vif)
mod_vif > 5.0
any(mod_vif > 5.0)

#vif(mod_gamma_inverse_fb)

# Nope; the highest variance inflation factor was for the 400m, which was
# ~4.4; definitely some multicollinearity there but nothing that needs to
# be addressed with corrective measures.
#
# Importantly, documentation for vif() also notes that, "Through a further
# generalization, the implementation here is applicable to other sorts of
# models, in particular weighred linear models, generalized linear models,
# and mixed-effects models."

# Let's do that for the other models as well.
print(vif(mod_gamma_inverse) > 5)
print(any(vif(mod_gamma_inverse) > 5))
print(vif(mod_gamma_identity) > 5)
print(any(vif(mod_gamma_identity) > 5))

# ================ Outliers and Influential Observations ================
fit_model_sans_influential <- function(mdl, dataset, form) {
	im <- influence.measures(mdl)
	names(im)

	# Get column sums for 
	im_col_sums = colSums(im$is.inf)

	plot(cooks.distance(mdl), type="h", ylab="Cook's Distance", las=1)

	# Find the indeces of observations that were flagged as influential
	# by more than one metric.
	idx_inf_multiple <- which(rowSums(im$is.inf) > 1)
	print("Influential Outlier Indeces:")
	print(idx_inf_multiple)

	# Keep track of the index at which Cook's Distance is highest.
	idx_cooks_d_max <- which.max(cooks.distance(mdl))

	print("Residual with highest Cook's Distance in flagged set?")
	print(any(idx_cooks_d_max == idx_inf_multiple))

	# Subset the data.
	data_subset <- dataset[-idx_inf_multiple, ]
	print("Data Subset Size:")
	print(dim(data_subset))
	print(head(data_subset))
	print("Compare with original dim:")
	print(dim(dataset))

	# Update the model.
	#mdl_updated <- update(mdl, subset=(-idx_inf_multiple))
	mdl_updated <- glm(form, data=data_subset,
					   family=Gamma(link=mdl$call$family$link),
					   control=list(trace=TRUE))

	print("Datasets Match:")
	print(all(data_subset == eval(mdl_updated$call$data)))

	# Return the updated model and dataset.
	return(list("model_updated"=mdl_updated, "data_updated"=data_subset))
}

inf_measures <- influence.measures(mod_gamma)
names(inf_measures)
# inf_measures$is.inf returns a logical dataframe in which each value indicates
# whether each row was marked as influential using the index specified by the
# given column. Then, we sum this logical vector columnwise so that each
# each value tells us how many examples were flagged as influential based on
# each metric.
inf_measures_col_sums = colSums(inf_measures$is.inf)
inf_measures_col_sums

# Let's have a look at Cook's Distance for these observations.
plot(cooks.distance(mod_gamma), type="h", ylab="Cook's Distance", las=1)

# Find those observations that were flagged as influential by more than one metric.
idx_inf_more_than_one <- which(rowSums(inf_measures$is.inf) > 1)
print(dec_data_t[idx_inf_more_than_one, ])

# Let's see which metrics they were flagged for.
print(inf_measures$is.inf[idx_inf_more_than_one, ])

# Let's see how much this agrees with the observation that has the highest value of
# Cook's distance.
idx_inf_cooks_d <- which.max(cooks.distance(mod_gamma))
idx_inf_cooks_d

# New comment: nothing was flagged by Cook's D for the log link model. However, the data point for
# which the value of Cook's D was maximal is included in the set of points that will get thrown out.

# Old comment:
# Observation 585 was flagged by more than two metrics as reported by influence.measures()
# and was has a really high value of Cook's Distance despite not being marked as influential
# along that axis. Hence, we should probably remove at least that observation.

# Update the model using the data with the influential observations removed.
dec_data_t_subset <- dec_data_t[-idx_inf_more_than_one, ]
#mod_gamma_updated <- update(mod_gamma, subset=(-idx_inf_more_than_one))
mod_gamma_updated <- glm(form, data=dec_data_t_subset,
						 family=Gamma(link="log"),
						 control=list(trace=TRUE))

# Double-check that those two datasets are the same.
print(all(dec_data_t_subset == eval(mod_gamma_updated$call$data)))

# Do the same for the other two models.
#inf_measures_inverse = influence.measures(mod_gamma_inverse)
#idx_inf_more_than_one_inverse = which(rowSums(inf_measures_inverse$is.inf) > 1)
#print(dec_data[idx_inf_more_than_one_inverse, ])
#dec_data_t_subset_inverse = dec_data_t[-idx_inf_more_than_one_inverse]
#mod_gamma_inverse_updated <- glm(form, data=dec_data_t_subset_inverse, family=Gamma(link="inverse"),
								 #control=list(trace=TRUE))

#inf_measures_identity <- influence.measures(mod_gamma_identity)
#idx_inf_more_than_one_identity = which(rowSums(inf_measures_identity$is.inf) > 1)
#print(dec_data[which(rowSums(inf_measures_identity$is.inf) > 1), ])
#dec_data_t_subset_identity = dec_data_t[-idx_inf_more_than_one_identity]
#mod_gamma_identity_updated <- glm(form, data=dec_data_t_subset_identity, family=Gamma(link="identity"),
								 #control=list(trace=TRUE))

updated_model_test_results <- fit_model_sans_influential(mod_gamma, dec_data_t, form)
updated_model_test <- updated_model_test_results$model_updated
updated_dataset_test <- updated_model_test_results$data_updated

# Compare the model we generated using our initial script and our
# new function.
all.equal(updated_model_test, mod_gamma_updated, tolerance=0.0000)
# Only the 'call' component doesn't match. That makes sense.
# Contrast when comparing two models we know are different:
# all.equal(mod_gamma, mod_gamma_updated, tolerance=0.0000)
# Test the datasets themselves for equality.
print(all(updated_dataset_test == eval(mod_gamma_updated$call$data)))
# All right; the data match, the models match along every dimension except
# the 'call' object, which I believe is due to the fact that they were (1)
# called from different pieces of code (different scopes specifically) or
# (2) that they were fitted on data objects that had different identifiers
# or memory addresses in spite of being the same in terms of value.
print(updated_model_test$family)
print(mod_gamma_updated$family)
all.equal(updated_model_test$family, mod_gamma_updated$family, tolerance=0.00000)

# Refit all models without outliers.
mod_gamma_inverse_update_results <- fit_model_sans_influential(mod_gamma_inverse,
															   dec_data_t,
															   form)
mod_gamma_identity_update_results <- fit_model_sans_influential(mod_gamma_identity,
																dec_data_t,
																form)
# Extract models.
mod_gamma_inverse_updated <- mod_gamma_inverse_update_results$model_updated
mod_gamma_identity_updated <- mod_gamma_identity_update_results$model_updated
# Extract datasets.
dec_data_t_subset_inverse <- mod_gamma_inverse_update_results$data_updated
dec_data_t_subset_identity <- mod_gamma_identity_update_results$data_updated

# =============== Compare the Models from a Deviance Standpoint ================

print(mod_gamma_updated$deviance)
print(mod_gamma_inverse_updated$deviance)
print(mod_gamma_identity_updated$deviance)

# =============== Fit a model without the delta ================

#form_no_deltas = build_formula("All_Time_PB", predictors_first)
#mod_gamma_no_deltas <- glm(form_no_deltas, data=dec_data_t, family=Gamma(link="log"),
						   #control=list(trace=TRUE))

# ================ Assessing Coefficients ================
summary(mod_gamma_updated)
summary(mod_gamma_inverse_updated)
summary(mod_gamma_identity_updated)

print("P values for coefficients in each model:")
coef_p_thresh <- 0.0001
summ_log <- summary(mod_gamma_updated)
summ_inv <- summary(mod_gamma_inverse_updated)
summ_ide <- summary(mod_gamma_identity_updated)
bool_p_above_log <- summ_log$coefficients[, 4] > coef_p_thresh
bool_p_above_inv <- summ_inv$coefficients[, 4] > coef_p_thresh
bool_p_above_ide <- summ_ide$coefficients[, 4] > coef_p_thresh
print(names(summ_log$coefficients[, 1])[bool_p_above_log])
print(names(summ_inv$coefficients[, 1])[bool_p_above_inv])
print(names(summ_ide$coefficients[, 1])[bool_p_above_ide])

# Let's take a look at what the model says about our coefficients.
# Coefficients as sorted by original algorithm:
# sort(abs(mod_gamma_updated$coefficients), decreasing=TRUE) == coef_sorted_abs
#    (Intercept)   Men_PV_first   Men_JT_first Men_1500_first   Men_LJ_first   Men_SP_first Men_110h_first   Men_HJ_first Men_1500_delta   Men_DT_first   Men_PV_delta  Men_400_delta   Men_LJ_delta 
#    8.944698968    0.017356780    0.012030668    0.011931433    0.011607824    0.010931102    0.010264587    0.010028795    0.010013069    0.009710495    0.009356215    0.008813456    0.008321229 
# Men_110h_delta  Men_400_first  Men_100_first   Men_HJ_delta   Men_SP_delta   Men_DT_delta   Men_JT_delta  Men_100_delta 
#    0.008166618    0.007799429    0.007302260    0.006938241    0.006520734    0.005602855    0.005327921    0.004757099 

sort_coefs_by_abs <- function(coefs) {
	idx <- sort(abs(coefs), decreasing=TRUE, index.return=TRUE)$ix
	return(coefs[idx])
}

#coef_sorted = mod_gamma_updated$coefficients[sort(abs(mod_gamma_updated$coefficients), decreasing=TRUE, index.return=TRUE)$ix]
coef_sorted <- sort_coefs_by_abs(mod_gamma_updated$coefficients)

# Do this for the other two updated models as well.
coef_sorted_inverse <- sort_coefs_by_abs(mod_gamma_inverse_updated$coefficients)
#coef_sorted_inverse = mod_gamma_inverse_updated$coefficients[sort(abs(mod_gamma_inverse_updated$coefficients), decreasing=TRUE)]

coef_sorted_identity <- sort_coefs_by_abs(mod_gamma_identity_updated$coefficients)
# Old method outlined here:
#coef_sorted_identity = sort(abs(mod_gamma_identity_updated$coefficients),
							#decreasing=TRUE)

# Plot the coefficients of the identity model.
# [DEPRECATED/Not used.]
# gen_plotting_colors <- function(model_coefs) {
# 	plt_col <- c("red")
# 	for (name in names(model_coefs)) {
# 		print(name)
# 		print(grep("first", name))
# 		if (grep("first", name) > 0) {
# 			plt_col <- c(plt_col, "red")
# 		} else {
# 			plt_col <- c(plt_col, "blue")
# 		}
# 	}
# 	return(plt_col)
# }

png("Images/gamma_analysis_r/first_delta_minus_outliers/log/coef_sorted.png")
barplot(coef_sorted[-1], las=2, col="#2797ab")
dev.off()

png("Images/gamma_analysis_r/first_delta_minus_outliers/inverse/coef_sorted.png")
barplot(coef_sorted_inverse[-1], las=2, col="#2797ab")
dev.off()

png("Images/gamma_analysis_r/first_delta_minus_outliers/identity/coef_sorted.png")
barplot(coef_sorted_identity[-1], las=2, col="#2797ab")
dev.off()

# ================ Diagnostic Plots ================

diagnostic_plots <- function(model, dataset, output_path="") {
	# ================ PLOTS TO ASSESS SYSTEMATIC COMPONENT ================
	# Plot residuals to assess model.
	png(paste(output_path, "standardized_deviance_vs_log_fitted.png", sep=""),
		width=450, 480)
	scatter.smooth(rstandard(model) ~ log(fitted(model)),
				   las=1, ylab="Standardized deviance residual", xlab="log(Fitted values)")
	dev.off()

	#readline("Press Enter to Continue")

	# Standardized deviance residuals.
	png(paste(output_path, "predictor_standardized_residual_vs_value.png", sep=""),
		width=900, height=900)
	par(mfrow=c(5, 4))
	for (predictor in predictors) {
		scatter.smooth(rstandard(model) ~ log(dataset[[predictor]] + 100), las=1,
					   ylab="Standardized deviance residual",
					   xlab=paste("log(", predictor, ")"))
	}
	dev.off()

	#readline("Press Enter to Continue")

	# Working residuals.
	# These residuals suggest a very suitable link function.
	png(paste(output_path, "linear_predictor_vs_working_responses.png", sep=""),
		width=480, height=480)
	par(mfrow=c(1, 1))
	z <- resid(model, type="working") + model$linear.predictor
	plot(z ~ model$linear.predictor,
		 las=1,
		 xlab="Working Responses",
		 ylab="Linear Predictor")
	abline(0, 1)  # Line of equality.
	dev.off()

	#readline("Press Enter to Continue")

	# Partial residuals to ensure that variables have been included on appropriate scales.
	# Unscaled, these residuals indicate we need to do some more scaling on our data.
	png(paste(output_path, "partial_residuals.png", sep=""),
		width=900, height=900)
	par(mfrow=c(5, 4))
	# Is this fine or is this not ok?
	# I changed this on 7/16/2024. Basically, I didn't
	# assign to `model$call$data` but instead passed the
	# dataset directly to `termplot`.
	# model$call$data <- dataset
	termplot(model, data=dataset, partial.resid=TRUE, las=1, col.res="red", ask=FALSE)
	dev.off()

	#readline("Press Enter to Continue")

	# ================ PLOTS TO ASSESS RANDOM COMPONENT ================

	# Grab the quantile residuals from the gamma GLM.
	# For interpretation purposes:
	# https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot
	# This plot shows that our distribution of quantile residuals is
	# lightly tailed.
	png(paste(output_path, "quantile_residuals.png", sep=""),
		width=480, height=480)
	par(mfrow=c(1, 1))
	qr.model <- qresid(model)
	qqnorm(qr.model, las=1)
	qqline(qr.model)
	dev.off()

	#readline("Press Enter to Continue")

	# ================ ASSESSING OUTLIERS AND INFLUENTIAL OBSERVATIONS ================

	# Compute raw quantile residuals, raw deviance residuals, standardized deviance
	# residuals, and Studentized residuals.
	rs <- cbind(rD=resid(model),  # Raw deviance residuals.
				"r'D"=rstandard(model),  # Standardized deviance residuals.
				"r''"=rstudent(model),  # Studentized residuals.
				rQ=qresid(model)  # Raw quantile residuals.
	)

	# Check for large residuals.
	resid_large <- apply(abs(rs), 2, max)
	print("Check for large residuals:")
	print(resid_large)
}

diagnostic_plots(mod_gamma, dec_data_t,
				 "Images/gamma_analysis_r/first_delta/")
# diagnostic_plots(mod_gamma_bc)
# Updated models.
diagnostic_plots(mod_gamma_updated, dec_data_t_subset,
				 "Images/gamma_analysis_r/first_delta_minus_outliers/log/")
diagnostic_plots(mod_gamma_inverse_updated, dec_data_t_subset_inverse,
				 "Images/gamma_analysis_r/first_delta_minus_outliers/inverse/")
diagnostic_plots(mod_gamma_identity_updated, dec_data_t_subset_identity,
				 "Images/gamma_analysis_r/first_delta_minus_outliers/identity/")

#diagnostic_plots(mod_gamma_no_deltas, dec_data_t,
				 #"Images/gamma_analysis_r/first_only/")

#diagnostic_plots(mod_gamma_inverse_fb, dec_data_fb_t,
				 #"Images/gamma_analysis_r/first_best/inverse/")

# ================ Compute Metrics ================

# Used this resource here:
# https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/dealing_with_right_skewed_data.html
predictions = predict.glm(mod_gamma_updated, newdata=dec_data_t_subset, type="response")
rmse = sqrt(mean((dec_data_t_subset$All_Time_PB - predictions)^2))
print(rmse)

predictions_inverse = predict.glm(mod_gamma_inverse_updated,
								  newdata=dec_data_t_subset_inverse,
								  type="response")
rmse_inverse = sqrt(mean((dec_data_t_subset_inverse$All_Time_PB - predictions_inverse)^2))
print(rmse_inverse)

predictions_identity = predict.glm(mod_gamma_identity_updated,
								   newdata=dec_data_t_subset_identity,
								   type="response")
rmse_identity = sqrt(mean((dec_data_t_subset_identity$All_Time_PB - predictions_identity)^2))
print(rmse_identity)

# Let's make a scatterplot of actual values vs. predictions.
plot(dec_data_t_subset$All_Time_PB, predictions,
	 xlab="True Values", ylab="Predictions", main="Predictions for Log Link Gamma Model")
plot(dec_data_t_subset_inverse$All_Time_PB, predictions_inverse,
	 xlab="True Values", ylab="Predictions", main="Predictions for Inverse Link Gamma Model")
plot(dec_data_t_subset_identity$All_Time_PB, predictions_identity,
	 xlab="True Values", ylab="Predictions", main="Predictions for Identity Link Gamma Model")

# Grab data so we can use it in the article.
sum_mod_log <- tidy(mod_gamma_updated)
sum_mod_inverse <- tidy(mod_gamma_inverse_updated)
sum_mod_identity <- tidy(mod_gamma_identity_updated)

# Change column names so these look better after output.
new_col_names <- c("Term", "Estimate", "Std. Error", "Statistic", "p")
colnames(sum_mod_log) <- new_col_names
colnames(sum_mod_inverse) <- new_col_names
colnames(sum_mod_identity) <- new_col_names

# Use knitr to convert those to markdown and save them.
sink("output/tabular/mod_gamma_sum_log.md")
kable(sum_mod_log)
sink()

sink("output/tabular/mod_gamma_sum_inverse.md")
kable(sum_mod_inverse)
sink()

sink("output/tabular/mod_gamma_sum_identity.md")
kable(sum_mod_identity)
sink()

# Plot coefficients.
#df_coef_sorted_log = data.frame(name=sum_mod_log["Term"],
								#value=sum_mod_log["Estimate"],
								#error=sum_mod_log["Std. Error"])
#df_coef_sorted_inverse = data.frame(name=names(coef_sorted_inverse)[-1], value=as.numeric(coef_sorted_inverse)[-1])
#df_coef_sorted_identity = data.frame(name=names(coef_sorted_identity)[-1], value=as.numeric(coef_sorted_identity)[-1])

#ggplot(data=dec_data, mapping=aes(x=Men_PV_first, y=Men_PV_delta)) + geom_point()

png("Images/gamma_analysis_r/first_delta_minus_outliers/log/coef.png")
ggplot(data=data.frame(sum_mod_log[-1, ]), mapping=aes(x=Term, y=Estimate)) +
	geom_bar(stat="identity", fill="steelblue") +
	geom_errorbar(aes(x=Term, ymin=Estimate - Std..Error, ymax=Estimate + Std..Error), width=0.4) +
	theme(axis.text.x=element_text(angle=90, hjust=1))
dev.off()

png("Images/gamma_analysis_r/first_delta_minus_outliers/inverse/coef.png")
ggplot(data=data.frame(sum_mod_inverse[-1, ]), mapping=aes(x=Term, y=Estimate)) +
	geom_bar(stat="identity", fill="steelblue") +
	geom_errorbar(aes(x=Term, ymin=Estimate - Std..Error, ymax=Estimate + Std..Error), width=0.4) +
	theme(axis.text.x=element_text(angle=90, hjust=1))
dev.off()

png("Images/gamma_analysis_r/first_delta_minus_outliers/identity/coef.png")
ggplot(data=data.frame(sum_mod_identity[-1, ]), mapping=aes(x=Term, y=Estimate)) +
	geom_bar(stat="identity", fill="steelblue") +
	geom_errorbar(aes(x=Term, ymin=Estimate - Std..Error, ymax=Estimate + Std..Error), width=0.4) +
	theme(axis.text.x=element_text(angle=90, hjust=1))
dev.off()

# Compare the coefficients.
# Rank each coefficient's position in each model.
# Edit 7/17/2024. I decided not to review this code to save time since this
# isn't making it into the final manuscript.
# feats <- names(coef(mod_gamma_updated))[-1]
# 
# coef_sorted_names_all <- data.frame(log=names(coef_sorted)[-1],
# 									inverse=names(coef_sorted_inverse)[-1],
# 									identity=names(coef_sorted_identity)[-1])
# 
# ranks <- list(log=c(), inverse=c(), identity=c())
# for (model in c("log", "inverse", "identity")) {
# 	for (feat in feats) {
# 		ranks[[feat]] <- c(ranks[[feat]], which(coef_sorted_names_all[[model]] == feat)[[1]])
# 		#rank_df[[feat]] <- c(rank_df[[feat]],
# 							 #feat=which(coef_sorted_names_all[[model]] == feat)[[1]])
# 	}
# }
# 
# rank_df <- data.frame(model=c("log", "inverse", "identity"))
# for (model in c("log", "inverse", "identity")) {
# 	for (feat in feats) {
# 		print(model)
# 		print(feat)
# 		rank_df[[feat]] <- ranks[[feat]]
# 	}
# }
# 
# # Copy this for later use and drop the "model" column.
# rank_df_by_event <- rank_df
# rank_df_by_event <- rank_df_by_event[-1]
# event_rank_means <- tidy(rank_df_by_event)$mean
# 
# rank_df <- transpose(rank_df, make.names="model")
# row.names(rank_df) <- feats
# rank_df$mean <- event_rank_means
# 
# sink("output/tabular/coef_ranks.md")
# kable(round(rank_df, 2))
# sink()
# 
# # Let's look at different event groups to start.
# event_groups <- list(sprints=c("Men_100_first", "Men_100_delta",
# 							   "Men_110h_first", "Men_110h_delta",
# 							   "Men_400_first", "Men_400_delta"),
# 					 jumps=c("Men_LJ_first", "Men_LJ_delta",
# 							 "Men_HJ_first", "Men_HJ_delta",
# 							 "Men_PV_first", "Men_PV_delta"),
# 					 throws=c("Men_SP_first", "Men_SP_delta",
# 							  "Men_DT_first", "Men_DT_delta",
# 							  "Men_JT_first", "Men_JT_delta"),
# 					 endurance=c("Men_1500_first", "Men_1500_delta"))
# 
# # Testing calculation of means:
# mean(as.matrix(rank_df[event_groups$throws, 1:length(rank_df)-1])) == mean(rank_df[event_groups$throws, ]$mean)
# 
# event_group_rank_means <- list("sprints"=NULL, "jumps"=NULL,
# 							   "throws"=NULL, "endurance"=NULL)
# 
# for (group in names(event_groups)) {
# 	feat_names <- event_groups[group]
# 	print(feat_names)
# 
# 	df_portion <- rank_df[unlist(feat_names), 1:length(rank_df) - 1]
# 
# 	print(df_portion)
# 	print(as.matrix(df_portion))
# 
# 	#rank_df[event_groups[["throws"]], 1:length(rank_df) - 1]
# 	event_group_rank_means[group] <- mean(as.matrix(df_portion))
# }

# Look at the question of selecting a future elite decathlete.
coef_first_log <- coefficients(mod_gamma_updated)[2:11]
coef_first_inverse <- coefficients(mod_gamma_inverse_updated)[2:11]
coef_first_identity <- coefficients(mod_gamma_identity_updated)[2:11]

coef_sorted_first_log <- sort_coefs_by_abs(coef_first_log)
coef_sorted_first_inverse <- sort_coefs_by_abs(coef_first_inverse)
coef_sorted_first_identity <- sort_coefs_by_abs(coef_first_identity)

print(coef_sorted_first_log)
print(coef_sorted_first_inverse)
print(coef_sorted_first_identity)

# Now, address the question of what is most important to improve in.
coef_delta_log <- coefficients(mod_gamma_updated)[12:21]
coef_delta_inverse <- coefficients(mod_gamma_inverse_updated)[12:21]
coef_delta_identity <- coefficients(mod_gamma_identity_updated)[12:21]

coef_sorted_delta_log <- sort_coefs_by_abs(coef_delta_log)
coef_sorted_delta_inverse <- sort_coefs_by_abs(coef_delta_inverse)
coef_sorted_delta_identity <- sort_coefs_by_abs(coef_delta_identity)

print(coef_sorted_delta_log)
print(coef_sorted_delta_inverse)
print(coef_sorted_delta_identity)

# Save some plots of these.

path_img_out <- "Images/gamma_analysis_r/first_delta_minus_outliers/"
margins = c(10, 8, 2, 2)

ylim_log = c(-0.05, 0.05)
ylim_log_abs = c(0, 0.025)
#ylim_log_delta = c(-0.01, 0.025)
ylim_log_delta = c(0, 0.025)
ylim_identity = c(0, 200)
ylim_identity_abs = range(c(0, 200))
#ylim_identity_delta = range(c(-50, 200))
ylim_identity_delta = range(c(0, 200))
ylim_inverse = c(0, 0.01)
ylim_inverse_abs = rev(range(c(-3.2e-06, 0)))
#ylim_inverse_delta = rev(range(c(-3.2e-06, 1.0e-06)))
ylim_inverse_delta = rev(range(c(-3.2e-06, 0)))

# plot_coefs <- function(path_out, path_img_out, coefs, margins, ylim_vals, color) {
# 	# First, choose ylim values based on the image we're producing.
# 	ylim_key = path_out[gregexpr("/", path_out)]
# 
# 	png(paste(path_img_out, path_out, sep=""),
# 		width=600, height=600)
# 	par(mar=margins)
# 	if (!is.na(ylim)) {
# 		barplot(coefs, ylim=ylim, col=color, las=2)
# 	} else {
# 		barplot(coefs, col=color, las=2)
# 	}
# 	dev.off()
# }
# plot_coefs("log/coef_sorted_first.png", path_img_out,
# 		   coef_sorted_first_log, margins, NA, "steelblue")

plot_model_coefs <- function(model, type="all", y_label="", y_label_line=4, ylim=NA, margins=NA,
							 img_width=600, img_height=600, error_bars="SEM", sort_decreasing=TRUE, path_out="") {
	# All coefficient information from the model, including standard errors, etc.
	mod_info <- summary(model)$coefficients

	if (type == "all") {
		coef_range <- 2:21
	}
	else if (type == "first") {
		coef_range <- 2:11
	}
	else if (type == "delta") {
		coef_range <- 12:21
	}

	# Grab relevant coefficients and errors.
	coefs <- mod_info[, "Estimate"][coef_range]
	# coefs_abs <- abs(coefs)

	if (error_bars == "SEM") {
		errors <- mod_info[, "Std. Error"][coef_range]
		# segments_lower <- coefs_abs - errors
		# segments_upper <- coefs_abs + errors
		segments_lower <- coefs - errors
		segments_upper <- coefs + errors
	}
	else if (error_bars == "CI") {
		# Thanks: https://www.econometrics-with-r.org/5.2-cifrc.html
		errors <- confint(model)[coef_range, ]
		segments_lower <- errors[, 1]
		segments_upper <- errors[, 2]

		print("Segments:")
		print(segments_upper)
		print(segments_lower)
	}

	# Find the indeces that appropriately sort the coefficients.
	sorted_ix <- sort(coefs, index.return=TRUE, decreasing=sort_decreasing)$ix

	# Sort coefficients and then sort segments ("errors") to pair each with its appropriate
	# coefficient.
	coefs_sorted = coefs[sorted_ix]
	segments_lower_sorted = segments_lower[sorted_ix]
	segments_upper_sorted = segments_upper[sorted_ix]

	# Modify the names of the coefficients to make them look better.
	new_coef_names_sorted <- c()
	for (name in names(coefs_sorted)) {
		name_parts <- strsplit(name, "_")[[1]]
		new_name <- paste(name_parts[2], name_parts[3])
		new_coef_names_sorted = c(new_coef_names_sorted, new_name)
	}

	names(coefs_sorted) <- new_coef_names_sorted

	# Plot the coefficients.
	png(path_out, width=img_width, height=img_height)
	par(mar=margins)
	#bar_centers <- boxplot(
	bar_centers <- barplot(
			coefs_sorted,
			ylim=ylim,
			col="grey",
			las=2,
			cex.axis=2,
			cex.names=2,
			cex.lab=2)
	title(ylab=y_label, line=y_label_line, cex.lab=2)

	# segments() has strange behavior when mixing named and unnamed inputs.
	# Hence, we get rid of the names.
	names(segments_lower_sorted) <- NULL
	names(segments_upper_sorted) <- NULL

	segments(x0=bar_centers,
			 y0=segments_lower_sorted,
			 x1=bar_centers,
			 y1=segments_upper_sorted)

	# Arrows to complete the error bars.
	arrows(x0=bar_centers - 0.2, y0=segments_lower_sorted, x1=bar_centers + 0.2, y1=segments_lower_sorted, length=0)
	arrows(x0=bar_centers - 0.2, y0=segments_upper_sorted, x1=bar_centers + 0.2, y1=segments_upper_sorted, length=0)

	dev.off()
}

# Plot sorted model coefficients.
error_bars = "CI"

# Firsts/FSB coefficients.
plot_model_coefs(model=mod_gamma_updated,
				 # This used to be "coef_abs_sorted_first". I changed it to reflect the fact we're
				 # just sorting the coefficients by their values and not by their absolute values. The
				 # other output file names have been modified in keeping with this theme.
				 path_out=paste(path_img_out, "log/coef_sorted_first.png", sep=""),
				 type="first",
				 y_label="log(PB)",
				 y_label_line=6,
				 ylim=ylim_log_abs,
				 margins=margins,
				 error_bars=error_bars)
plot_model_coefs(model=mod_gamma_inverse_updated,
				 path_out=paste(path_img_out, "inverse/coef_sorted_first.png", sep=""),
				 type="first",
				 y_label="1/PB",
				 y_label_line=7,
				 ylim=ylim_inverse_abs,
				 margins=margins,
				 error_bars=error_bars,
				 sort_decreasing=FALSE)
plot_model_coefs(model=mod_gamma_identity_updated,
				 path_out=paste(path_img_out, "identity/coef_sorted_first.png", sep=""),
				 type="first",
				 y_label="PB",
				 y_label_line=4,
				 ylim=ylim_identity_abs,
				 margins=margins,
				 error_bars=error_bars)
# plot_model_coefs(model=mod_gamma_identity_updated,
# 				 path_out=paste(path_img_out, "identity/match_test.png", sep=""),
# 				 type="first",
# 				 y_label="Overall Score",
# 				 y_label_line=4,
# 				 ylim=ylim_identity_abs,
# 				 margins=margins,
# 				 error_bars=error_bars)
# plot_model_coefs(model=mod_gamma_identity_updated,
# 				 path_out=paste(path_img_out, "identity/coef_abs_sorted_first_CI.png", sep=""),
# 				 type="first",
# 				 y_label="Overall Score",
# 				 y_label_line=4,
# 				 ylim=ylim_identity_abs,
# 				 margins=margins,
# 				 margins=margins,
# 				 error_bars=error_bars)

# Deltas.
plot_model_coefs(model=mod_gamma_updated,
				 path_out=paste(path_img_out, "log/coef_sorted_delta.png", sep=""),
				 type="delta",
				 y_label="log(PB)",
				 y_label_line=6,
				 ylim=ylim_log_delta,
				 margins=margins,
				 error_bars=error_bars)
plot_model_coefs(model=mod_gamma_inverse_updated,
				 path_out=paste(path_img_out, "inverse/coef_sorted_delta.png", sep=""),
				 type="delta",
				 y_label="1/PB",
				 y_label_line=6.5,
				 ylim=ylim_inverse_delta,
				 margins=margins,
				 error_bars=error_bars,
				 sort_decreasing=FALSE)
plot_model_coefs(model=mod_gamma_identity_updated,
				 path_out=paste(path_img_out, "identity/coef_sorted_delta.png", sep=""),
				 type="delta",
				 y_label="PB",
				 y_label_line=4,
				 ylim=ylim_identity_delta,
				 margins=margins,
				 error_bars=error_bars)

# Spearman's rho (rank correlations).
rank_corr_log_inverse = cor.test(
	x=coef(mod_gamma_updated)[2:21],
	y=coef(mod_gamma_inverse_updated)[2:21],
	method="spearman"
)
rank_corr_identity_inverse = cor.test(
	x=coef(mod_gamma_identity_updated)[2:21],
	y=coef(mod_gamma_inverse_updated)[2:21],
	method="spearman"
)
rank_corr_log_identity = cor.test(
	x=coef(mod_gamma_updated)[2:21],
	y=coef(mod_gamma_identity_updated)[2:21],
	method="spearman"
)
rank_corr_log_inverse
rank_corr_identity_inverse
rank_corr_log_identity

# Comparing specific coefficients.
# DEPRECATED as of 7/19/2024. No longer using confidence
# interval overlap to compare coefficients.
CIs_overlap <- function(a, b) {
	a_bottom = a[1]
	a_top = a[2]

	b_bottom = b[1]
	b_top = b[2]

	if (a_top < b_bottom || b_top < a_bottom) {
		return(FALSE)
	} 
	else {
		return(TRUE)
	}
}

CI_overlap_matrix <- function(model, coef_idx_start = 2, coef_idx_end = 21) {
	# Obtain model confidence intervals and coefficient names.
	CIs <- confint(model)[coef_idx_start:coef_idx_end,]
	coef_names <- names(coefficients(model))[coef_idx_start:coef_idx_end]
	n_coef = coef_idx_end - coef_idx_start + 1

	print("Confidence Intervals:")
	print(CIs)

	data_out <- matrix(nrow = n_coef, ncol = n_coef, dimnames = list(coef_names, coef_names))

	for (i in 1:n_coef) {
		for (j in 1:n_coef) {
			res <- CIs_overlap(CIs[i,], CIs[j,])
			if (res) {
				# data_out[i, j] <- "."
				data_out[i, j] <- 0
			}
			# i coefficient's CI is above j coefficient's CI.
			else if (CIs[i,][1] > CIs[j,][2]){
				# data_out[i, j] <- coef_names[i]
				data_out[i, j] <- -10
			}
			# i coefficient's CI is below j coefficient's CI.
			else if (CIs[i,][2] < CIs[j,][1]) {
				# data_out[i, j] <- coef_names[j]
				data_out[i, j] <- 10
			}
		}
	}	
	return (data_out)
}

linear_hypothesis_matrix <- function(model, coef_idx_start = 2, coef_idx_end = 21) {
	# CIs <- confint(model)[coef_idx_start:coef_idx_end,]
	coefs = coefficients(model)[coef_idx_start:coef_idx_end]
	coef_names <- names(coefs)
	n_coef = coef_idx_end - coef_idx_start + 1

	data_out <- matrix(nrow = n_coef, ncol = n_coef, dimnames = list(coef_names, coef_names))

	for (i in 1:n_coef) {
		for (j in 1:n_coef) {
			if (i == j) {
				data_out[i, j] <- 0
			}
			else {
				# Test linear hypothesis that the two are equal.
				res <- linearHypothesis(model, paste(coef_names[i], "-", coef_names[j], "= 0"))
				# res <- glht(model, linfct=paste(coef_names[i], "<", coef_names[j]))
				# Extract p value for two-sided hypothesis test.				
				sig = res[2, 4]

				# This is from when we were using linearHypothesis:
				# Changed this from `sig < 0.05` to 'sig <= 0.05' on 7/19/2024
				# because P(p < alpha) = P(p <= alpha) = alpha.
				# https://www.researchgate.net/post/If_p-value_is_exactly_equal_to_005_is_that_significant_or_insignificant
				if (sig <= 0.05) {
					if (coefs[i] > coefs[j]) {
						data_out[i, j] <- -10
					}
					else {
						data_out[i, j] <- 10
					}
				}
				else {
					data_out[i, j] <- 0
				}
			}
		}
	}

	return(data_out)
}

# Comparison of coefficients using confidence intervals deprecated
# in favor of `linearHypothesis()` on 7/19/2024.
# CI_log = confint(mod_gamma_updated)[2:21,]
# CI_inverse = confint(mod_gamma_inverse_updated)[2:21,]
# CI_identity = confint(mod_gamma_identity_updated)[2:21,]
# 
# # ============ First-Season Bests ============
# # ======== Log model ========
# # 110h and the DT. Expected: False.
# CIs_overlap(CI_log[2,], CI_log[8,])
# 
# # 110h and the HJ. Expected: False.
# CIs_overlap(CI_log[2,], CI_log[5,])
# 
# # 110h and 400m. Expected: True.
# CIs_overlap(CI_log[2,], CI_log[3,])
# 
# # HJ and LJ.
# CIs_overlap(CI_log[4,], CI_log[5,])
# 
# # ======== Inverse model ========
# # 110h and the DT. Expected: False.
# CIs_overlap(CI_inverse[2,], CI_inverse[8,])
# 
# # 110h and the SP. Expected: False.
# CIs_overlap(CI_inverse[2,], CI_inverse[7,])
# 
# # 110h and the HJ. Expected: False.
# CIs_overlap(CI_inverse[2,], CI_inverse[5,])
# 
# # 110h and 400m. Expected: True.
# CIs_overlap(CI_inverse[2,], CI_inverse[3,])
# 
# # High jump and javelin throw. Expected: True.
# CIs_overlap(CI_inverse[5,], CI_inverse[9,])
# 
# # LJ and DT.
# CIs_overlap(CI_inverse[4,], CI_inverse[8,])
# 
# # LJ and SP.
# CIs_overlap(CI_inverse[4,], CI_inverse[7,])
# 
# # ======== Identity model ========
# # 110h and the DT. Expected: False.
# CIs_overlap(CI_identity[2,], CI_identity[8,])
# 
# # 110h and the HJ. Expected: False.
# CIs_overlap(CI_identity[2,], CI_identity[5,])
# 
# # 110h and 400m. Expected: True.
# CIs_overlap(CI_identity[2,], CI_identity[3,])
# 
# # LJ and HJ.
# CIs_overlap(CI_identity[4,], CI_identity[5,])
# 
# # LJ and SP.
# CIs_overlap(CI_identity[4,], CI_identity[7,])
# 
# # ============ Deltas ============
# # ======== Log Model ========
# # LJ and HJ.
# CIs_overlap(CI_log[14,], CI_log[15,])
# 
# # 110m hurdles and 1500m.
# CIs_overlap(CI_log[12,], CI_log[20,])
# 
# # 110m hurdles and 100m.
# CIs_overlap(CI_log[12,], CI_log[11,])
# 
# # 400m and SP.
# CIs_overlap(CI_log[13,], CI_log[17,])
# 
# # 400m and HJ.
# CIs_overlap(CI_log[13,], CI_log[15,])
# 
# # 400m and DT. Expected: True.
# CIs_overlap(CI_log[13,], CI_log[18,])
# 
# # ======== Inverse Model ========
# # PV and SP.
# CIs_overlap(CI_inverse[16,], CI_inverse[17,])
# 
# # LJ and HJ.
# CIs_overlap(CI_inverse[14,], CI_inverse[15,])
# 
# # 1500m and 110h.
# CIs_overlap(CI_inverse[12,], CI_inverse[20,])
# 
# # 110m hurdles and the 100m.
# CIs_overlap(CI_inverse[12,], CI_inverse[11,])
# 
# # 400m and SP.
# CIs_overlap(CI_inverse[13,], CI_inverse[17,])
# 
# # 400m and HJ.
# CIs_overlap(CI_inverse[13,], CI_inverse[15,])
# 
# # 400m and DT. Expected: True.
# CIs_overlap(CI_inverse[13,], CI_inverse[18,])
# 
# # ======== Identity Model ========
# # PV and LJ.
# CIs_overlap(CI_identity[16,], CI_identity[14,])
# 
# # PV and SP.
# CIs_overlap(CI_identity[16,], CI_identity[17,])
# 
# # LJ and HJ.
# CIs_overlap(CI_identity[14,], CI_identity[15,])
# 
# # 1500m and 110h.
# CIs_overlap(CI_identity[12,], CI_identity[20,])
# 
# # 110m hurdles and the 100m.
# CIs_overlap(CI_identity[12,], CI_identity[11,])
# 
# # 400m and SP.
# CIs_overlap(CI_identity[13,], CI_identity[17,])
# 
# # 400m and HJ.
# CIs_overlap(CI_identity[13,], CI_identity[15,])
# 
# # 400m and DT. Expected: False.
# CIs_overlap(CI_identity[13,], CI_identity[18,])
# 
# # If we're going to blow up familywise error, let's at least see
# # the full picture.
CI_matrix_log_first <- CI_overlap_matrix(mod_gamma_updated, coef_idx_start = 2, coef_idx_end = 11)
CI_matrix_inverse_first <- CI_overlap_matrix(mod_gamma_inverse_updated, coef_idx_start = 2, coef_idx_end = 11)
CI_matrix_identity_first <- CI_overlap_matrix(mod_gamma_identity_updated, coef_idx_start = 2, coef_idx_end = 11)
CI_matrix_log_delta <- CI_overlap_matrix(mod_gamma_updated, coef_idx_start = 12, coef_idx_end = 21)
CI_matrix_inverse_delta <- CI_overlap_matrix(mod_gamma_inverse_updated, coef_idx_start = 12, coef_idx_end = 21)
CI_matrix_identity_delta <- CI_overlap_matrix(mod_gamma_identity_updated, coef_idx_start = 12, coef_idx_end = 21)

write.csv(CI_matrix_log_first, file = "CI_matrix_log_first.csv")
write.csv(CI_matrix_inverse_first, file = "CI_matrix_inverse_first.csv")
write.csv(CI_matrix_identity_first, file = "CI_matrix_identity_first.csv")
write.csv(CI_matrix_log_delta, file = "CI_matrix_log_delta.csv")
write.csv(CI_matrix_inverse_delta, file = "CI_matrix_inverse_delta.csv")
write.csv(CI_matrix_identity_delta, file = "CI_matrix_identity_delta.csv")

# Pairwise comparison of coefficients.
lh_test <- linearHypothesis(mod_gamma_updated, "Men_PV_first - Men_110h_first = 0")

lh_matr_log_first <- linear_hypothesis_matrix(mod_gamma_updated, 2, 11)
lh_matr_inv_first <- linear_hypothesis_matrix(mod_gamma_inverse_updated, 2, 11)
lh_matr_ide_first <- linear_hypothesis_matrix(mod_gamma_identity_updated, 2, 11)

lh_matr_log_delta <- linear_hypothesis_matrix(mod_gamma_updated, 12, 21)
lh_matr_inv_delta <- linear_hypothesis_matrix(mod_gamma_inverse_updated, 12, 21)
lh_matr_ide_delta <- linear_hypothesis_matrix(mod_gamma_identity_updated, 12, 21)

write.csv(lh_matr_log_first, file = "lh_matrix_log_first.csv")
write.csv(lh_matr_inv_first, file = "lh_matrix_inv_first.csv")
write.csv(lh_matr_ide_first, file = "lh_matrix_ide_first.csv")

write.csv(lh_matr_log_delta, file = "lh_matrix_log_delta.csv")
write.csv(lh_matr_inv_delta, file = "lh_matrix_inv_delta.csv")
write.csv(lh_matr_ide_delta, file = "lh_matrix_ide_delta.csv")

#png(paste(path_img_out, "log/coef_sorted_first.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(coef_sorted_first_log, ylim=ylim_log, col="steelblue", las=2)
#dev.off()

#png(paste(path_img_out, "inverse/coef_sorted_first.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(coef_sorted_first_inverse, ylim=ylim, col="steelblue", las=2)
#dev.off()

#png(paste(path_img_out, "identity/coef_sorted_first.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(coef_sorted_first_identity, ylim=ylim, col="steelblue", las=2)
#dev.off()

# ---
# Note: These are the plots that are actually ending up in the paper.

#png(paste(path_img_out, "log/coef_abs_sorted_first.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(abs(coef_sorted_first_log), ylim=ylim_log_abs, col="grey", las=2)
#dev.off()

#png(paste(path_img_out, "inverse/coef_abs_sorted_first.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(abs(coef_sorted_first_inverse), ylim=ylim_inverse_abs, col="grey", las=2)
#dev.off()

#png(paste(path_img_out, "identity/coef_abs_sorted_first.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(abs(coef_sorted_first_identity), ylim=ylim_identity_abs, col="grey", las=2)
#dev.off()

# Deltas.

#png(paste(path_img_out, "log/coef_sorted_delta.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(coef_sorted_delta_log, ylim=ylim, col="steelblue", las=2)
#dev.off()

#png(paste(path_img_out, "inverse/coef_sorted_delta.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(coef_sorted_delta_inverse, ylim=ylim, col="steelblue", las=2)
#dev.off()

#png(paste(path_img_out, "identity/coef_sorted_delta.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(coef_sorted_delta_identity, ylim=ylim, col="steelblue", las=2)
#dev.off()

# ---
# Note: these are actually showing up in the paper!

#png(paste(path_img_out, "log/coef_abs_sorted_delta.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#bar_centers_log_delta <- barplot(abs(coef_sorted_delta_log),
								 #ylim=ylim_log_abs,
								 #density=((coef_sorted_delta_log > 0) * 100),
								 #col="grey", las=2)
#dev.off()

#png(paste(path_img_out, "inverse/coef_abs_sorted_delta.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(abs(coef_sorted_delta_inverse), ylim=ylim_inverse_abs, col="grey", las=2)
#dev.off()

#png(paste(path_img_out, "identity/coef_abs_sorted_delta.png", sep=""),
	#width=600, height=600)
#par(mar=margins)
#barplot(abs(coef_sorted_delta_identity), ylim=ylim_identity_abs, col="grey", las=2)
#dev.off()

# Pairwise comparison of coefficients.

# Write the data subsets for each model to the disk for the next step.
write.csv(dec_data_t_subset, "dec_data_t_subset_log.csv")
write.csv(dec_data_t_subset_inverse, "dec_data_t_subset_inv.csv")
write.csv(dec_data_t_subset_identity, "dec_data_t_subset_ide.csv")

# Quick check: ensure that confidence intervals are the same with or without
# specification of 0.95 level.
identical(confint(mod_gamma_updated), confint(mod_gamma_updated, level = 0.95))
# All good.

# Check to see which p-values are large enough that we need to report them.
p_vals_log <- summary(mod_gamma_updated)$coefficients[, 4]
p_vals_inv <- summary(mod_gamma_inverse_updated)$coefficients[, 4]
p_vals_ide <- summary(mod_gamma_identity_updated)$coefficients[, 4]
print("Check to ensure all predictors are significantly related to outcome at p < 0.0001:")
print("Log:")
p_vals_log < 0.0001
print("Inverse:")
p_vals_inv < 0.0001
print("Identity:")
p_vals_ide < 0.0001
# Get rounded forms of those values:
print("Raw p values for Men's 100 Delta:")
p_vals_log["Men_100_delta"]
p_vals_inv["Men_100_delta"]
p_vals_ide["Men_100_delta"]
print("Rounded to 3 decimal places:")
round(p_vals_log["Men_100_delta"], 3)
round(p_vals_inv["Men_100_delta"], 3)
round(p_vals_ide["Men_100_delta"], 3)

## Sources not already cited:
## Note: last time I copied history from elinks was 1/11/2024 at 2:44pm.
##
## https://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
## https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
## https://www.w3schools.com/r/r_for_loop.asp
## https://www.youtube.com/watch?v=LaVyUoTqM90
## https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/
## https://library.virginia.edu/data/articles/understanding-deviance-residuals
## https://stackoverflow.com/questions/7535982/how-to-reset-parmfrow-in-r
## https://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r
## https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot
## https://search.r-project.org/CRAN/refmans/statmod/html/qresiduals.html
## https://stackoverflow.com/questions/11025123/how-to-convert-r-markdown-to-pdf
## https://bookdown.org/yihui/rmarkdown-cookbook/
## https://stackoverflow.com/questions/28766443/add-a-constant-value-to-all-rows-in-a-dataframe
## https://blogs.sas.com/content/iml/2011/04/27/log-transformations-how-to-handle-negative-data-values.html
## https://www.statmethods.net/advgraphs/layout.html
## https://blogs.sas.com/content/iml/2011/04/27/log-transformations-how-to-handle-negative-data-values.html
## https://www.statmethods.net/advgraphs/layout.html
## https://stackoverflow.com/questions/23717397/how-to-use-a-string-variable-to-select-a-data-frame-column-using-notation
## https://stats.oarc.ucla.edu/r/modules/subsetting-data/
## https://rpubs.com/benhorvath/glm_diagnostics
## https://statisticsbyjim.com/regression/check-residual-plots-regression-analysis/
## https://en.wikipedia.org/wiki/Saddlepoint_approximation_method
## https://stackoverflow.com/questions/19309333/how-to-perform-log2-transform-on-specific-columns-in-r
## https://sparkbyexamples.com/r-programming/remove-column-in-r/
## https://stackoverflow.com/questions/48520517/how-to-solve-error-of-log-produces-nans-in-r
## https://r-coder.com/box-cox-transformation-r/
## https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/
## https://stats.stackexchange.com/questions/525735/how-to-model-heavily-left-skewed-data
## https://towardsdatascience.com/transforming-skewed-data-73da4c2d0d16
## https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/dealing_with_right_skewed_data.html
## https://stats.stackexchange.com/questions/525735/how-to-model-heavily-left-skewed-data
## https://en.wikipedia.org/wiki/Partial_residual_plot
## https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/termplot
## https://youtu.be/vGOpEpjz2Ks?si=2k0dx8PF3iQI47q2
## https://www.statology.org/test-for-normality-in-r/
## https://neptune.ai/blog/data-preprocessing-guide
## https://towardsdatascience.com/all-about-feature-scaling-bcc0ad75cb35
## https://towardsdatascience.com/all-about-feature-scaling-bcc0ad75cb35
## https://search.r-project.org/CRAN/refmans/DescTools/html/RobScale.html
## https://en.wikipedia.org/wiki/Robust_measures_of_scale
## https://en.wikipedia.org/wiki/Robust_measures_of_scale
## https://stackoverflow.com/questions/51841506/data-standardization-vs-normalization-vs-robust-scaler
## https://en.wikipedia.org/wiki/Robust_measures_of_scale
## https://en.wikipedia.org/wiki/Robust_measures_of_scale
## https://www.rdocumentation.org/packages/Causata/versions/4.2-0/topics/Where
## https://stat.ethz.ch/R-manual/R-devel/library/utils/html/stack.html
## https://stackoverflow.com/questions/65359379/unstacking-dataframe-in-r-and-python
## https://towardsdatascience.com/reshape-r-dataframes-wide-to-long-with-melt-tutorial-and-visualization-ddf130cd9299
## https://stackoverflow.com/questions/24334202/hierarchical-indexing-in-r-dataframe
## https://www.statology.org/r-size-of-data-frame/
## https://search.r-project.org/CRAN/refmans/rstatix/html/cor_reshape.html
## https://search.r-project.org/CRAN/refmans/rstatix/html/cor_reshape.html
## https://cran.r-project.org/doc/manuals/R-exts.html
## https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/
## https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/
## https://www.rdocumentation.org/packages/regclass/versions/1.6/topics/VIF
## https://www.statology.org/variance-inflation-factor-r/
## https://www.rdocumentation.org/packages/regclass/versions/1.6/topics/VIF
## https://www.statology.org/variance-inflation-factor-r/
## https://bookdown.org/ndphillips/YaRrr/logical-indexing.html
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/which.html
## https://stackoverflow.com/questions/59649809/extract-dataframe-from-lm-model-object
## https://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame
## https://stackoverflow.com/questions/7144118/how-can-i-save-a-plot-as-an-image-on-the-disk
## Pandoc two images side by side markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20two%20images%20side%20by%20side%20markdown&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704285905
## Pandoc Markdown to PDF image position - Stack Overflow	https://stackoverflow.com/questions/49482221/pandoc-markdown-to-pdf-image-position	1704285924
## two images side by side pandoc - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=two%20images%20side%20by%20side%20pandoc&btnG=Google%20Search&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704286315
## robust scaling - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=robust%20scaling&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704377845
## sklearn.preprocessing.RobustScaler the R Graph Gallery	https://r-graph-gallery.com/4-barplot-with-error-bar.html	1704644462
## r - Rotating x label text in ggplot - Stack Overflow	https://stackoverflow.com/questions/36682451/rotating-x-label-text-in-ggplot	1704651283
## Google	http://www.google.com/	1704651285
## compute explained variance for a generalized linear model - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=compute%20explained%20variance%20for%20a%20generalized%20linear%20model&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704655125
## generalized linear model - Measure of explained variance for Poisson GLM (log-link function) - Cross Validated	https://stats.stackexchange.com/questions/124306/measure-of-explained-variance-for-poisson-glm-log-link-function	1704655176
## Google Scholar	https://scholar.google.com/	1704655181
## Sign in - Google Accounts	https://accounts.google.com/v3/signin/identifier?continue=https%3A%2F%2Fscholar.google.com%2F&hl=en&ifkv=ASKXGp3TG8lZx7nJKX7MBlWeIMY_kmVjcxVJSTe0YD6y5CBSWOVPlt6dM5xHgBKa5pO_UvfDUkFP&flowName=WebLiteSignIn&flowEntry=ServiceLogin&dsh=S1577781399%3A1704655203110086	1704655193
## Sign in - Google Accounts	https://accounts.google.com/v3/signin/rejected?continue=https://scholar.google.com/&dsh=S1577781399:1704655203110086&epd=AagDPRjAlhmlyORtavVh9bZTD_OyYLDj0nVaLccdJY-jYL7kchxXN_q4vg&flowEntry=ServiceLogin&flowName=WebLiteSignIn&hl=en&idnf=pbattles@iu.edu&ifkv=ASKXGp3TG8lZx7nJKX7MBlWeIMY_kmVjcxVJSTe0YD6y5CBSWOVPlt6dM5xHgBKa5pO_UvfDUkFP&rhlk=js&rrk=88	1704655201
## How to use Pandoc image alignment to align two images in the same row? - Stack Overflow	https://stackoverflow.com/questions/15367332/how-to-use-pandoc-image-alignment-to-align-two-images-in-the-same-row	1704655755
## How to use Pandoc image alignment to align two images in the same row? - Stack Overflow	https://stackoverflow.com/questions/15367332/how-to-use-pandoc-image-alignment-to-align-two-images-in-the-same-row	1704655755
## find index of an item in an array R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=find%20index%20of%20an%20item%20in%20an%20array%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704656298
## indexing - Is there an R function for finding the index of an element in a vector? - Stack Overflow	https://stackoverflow.com/questions/5577727/is-there-an-r-function-for-finding-the-index-of-an-element-in-a-vector	1704656497
## data extraction and manipulation in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=data%20extraction%20and%20manipulation%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704656510
## dyplr - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=dyplr&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704656535
## A Grammar of Data Manipulation • dplyr	https://dplyr.tidyverse.org/	1704657724
## convert list to dataframe in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=convert%20list%20to%20dataframe%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704657736
## r - Convert a list to a data frame - Stack Overflow	https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame	1704658012
## Concatenating a list of data frames | R-bloggers	https://www.r-bloggers.com/2014/06/concatenating-a-list-of-data-frames/	1704658261
## add a row to a dataframe in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=add%20a%20row%20to%20a%20dataframe%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704658273
## dataframe - How to add a row to a data frame in R? - Stack Overflow	https://stackoverflow.com/questions/28467068/how-to-add-a-row-to-a-data-frame-in-r	1704658455
## specify shape of dataframe before adding data in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=specify%20shape%20of%20dataframe%20before%20adding%20data%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704658467
## dataframe - Shape data frame by adding zero in R - Stack Overflow	https://stackoverflow.com/questions/52668152/shape-data-frame-by-adding-zero-in-r	1704659574
## HTTP redirect	https://stat.ethz.ch/R-manual/R-devel/library/base/help/row.names.html	1704659594
## R set dataframe row names - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20set%20dataframe%20row%20names&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704659604
## How to change Row Names of DataFrame in R ? - GeeksforGeeks	https://www.geeksforgeeks.org/how-to-change-row-names-of-dataframe-in-r/	1704659971
## transpose dataframe in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=transpose%20dataframe%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704659981
## r - Transpose a data frame - Stack Overflow	https://stackoverflow.com/questions/6778908/transpose-a-data-frame	1704661012
## how to put a referencable table in markdown pandoc - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20put%20a%20referencable%20table%20in%20markdown%20pandoc&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661025
## Referencing inline-Latex tables in Pandoc Markdown - TeX - LaTeX Stack Exchange	https://tex.stackexchange.com/questions/540740/referencing-inline-latex-tables-in-pandoc-markdown	1704661116
## Pandoc reference a table in markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20reference%20a%20table%20in%20markdown&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661135
## Referencing tables in pandoc - TeX - LaTeX Stack Exchange	https://tex.stackexchange.com/questions/139106/referencing-tables-in-pandoc	1704661208
## AUR pandoc-tablenos - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=AUR%20pandoc-tablenos&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661525
## AUR (en) - pandoc-tablenos	https://aur.archlinux.org/packages/pandoc-tablenos	1704661531
## AUR (en) - pandoc-xnos	https://aur.archlinux.org/packages/pandoc-xnos	1704661669
## pandoc reference table in markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=pandoc%20reference%20table%20in%20markdown&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661909
## Pandoc - Pandoc User’s Guide	https://pandoc.org/MANUAL.html	1704662276
## Pandoc \ref for table - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20%5cref%20for%20table&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704662287
## GitHub - tomduck/pandoc-xnos: Library code for pandoc-fignos/eqnos/tablenos/secnos.	https://github.com/tomduck/pandoc-xnos	1704662380
## GitHub - tomduck/pandoc-tablenos: A pandoc filter for numbering tables and table references.	https://github.com/tomduck/pandoc-tablenos	1704662385
## Google Image Result for https://opengraph.githubassets.com/f063d1b0b362cdbcc529713e2bff2ae94dc53d6431673e07d2bb3375d6190e61/tomduck/pandoc-xnos/issues/11	https://www.google.com/imgres?imgurl=https://opengraph.githubassets.com/f063d1b0b362cdbcc529713e2bff2ae94dc53d6431673e07d2bb3375d6190e61/tomduck/pandoc-xnos/issues/11&imgrefurl=https://github.com/tomduck/pandoc-xnos/issues/11&h=600&w=1200&tbnid=hH3Mheadt60YyM&q=pandoc-tablenos+cannot+understand+pandocversion%3D3.1.6&tbnh=75&tbnw=150&usg=AI4_-kS3xTEF7RqKLKKndVva__bWdC6LRg&vet=1&docid=02q66oX_8hYfaM&hl=en&sa=X&ved=2ahUKEwi0otCymsyDAxXdHzQIHVx4B0QQ9QF6BAgAEAQ	1704662489
## pandoc-tablenos cannot understand pandocversion=3.1.6 - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=pandoc-tablenos%20cannot%20understand%20pandocversion%3d3.1.6&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704662494
## `--filter pandoc-xnos` not working · Issue #11 · tomduck/pandoc-xnos · GitHub	https://github.com/tomduck/pandoc-xnos/issues/11	1704663067
## R row-wise mean for dataframe - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20row-wise%20mean%20for%20dataframe&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704663080
## row - R dplyr rowwise mean or min and other methods? - Stack Overflow	https://stackoverflow.com/questions/31598935/r-dplyr-rowwise-mean-or-min-and-other-methods	1704727786
## Spearman rank test - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Spearman%20rank%20test&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704727815
## Spearman's rank correlation coefficient - Wikipedia	https://en.wikipedia.org/wiki/Spearman's_rank_correlation_coefficient	1704731267
## how to index dataframe with list R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20index%20dataframe%20with%20list%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704731282
## Chapter 3 Working with lists and data frames | Population Health Data Science with R	https://bookdown.org/taragonmd/phds/working-with-lists-and-data-frames.html	1704741700
## index dataframe by row R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=index%20dataframe%20by%20row%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704741713
## How to Select Rows by Index in R (With Examples) - Statology	https://www.statology.org/r-select-rows-by-index/	1704742379
## R drop last column of dataframe - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20drop%20last%20column%20of%20dataframe&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704742390
## r - Drop last 5 columns from a dataframe without knowing specific number - Stack Overflow	https://stackoverflow.com/questions/26483643/drop-last-5-columns-from-a-dataframe-without-knowing-specific-number	1704742485
## take mean of an entire dataframe - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=take%20mean%20of%20an%20entire%20dataframe&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704742497
## take mean of an entire dataframe R - Google Search	http://www.google.com/search?sca_esv=596620277&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=take%20mean%20of%20an%20entire%20dataframe%20R	1704742503
## How to find the mean of all values in an R data frame? - GeeksforGeeks	https://www.geeksforgeeks.org/how-to-find-the-mean-of-all-values-in-an-r-data-frame/	1704743255
## convert list to vector in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=convert%20list%20to%20vector%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704743267
## Converting a List to Vector in R Language - unlist() Function - GeeksforGeeks	https://www.geeksforgeeks.org/converting-a-list-to-vector-in-r-language-unlist-function/	1704743470
## HTTP redirect	https://stat.ethz.ch/R-manual/R-devel/library/base/help/typeof.html	1704743901
## get object type in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=get%20object%20type%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704743904
## how to index R dataframe by columns programmatically - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20index%20R%20dataframe%20by%20columns%20programmatically&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704743941
## How to Select DataFrame Columns by Index in R? - GeeksforGeeks	https://www.geeksforgeeks.org/how-to-select-dataframe-columns-by-index-in-r/	1704744191
## dplyr select for rows - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=dplyr%20select%20for%20rows&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704744211
## Keep rows that match a condition — filter • dplyr	https://dplyr.tidyverse.org/reference/filter.html	1704744460
## R select rows by name without hard coding - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20select%20rows%20by%20name%20without%20hard%20coding&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704744486
## Subset DataFrame and Matrix by Row Names in R - GeeksforGeeks	https://www.geeksforgeeks.org/subset-dataframe-and-matrix-by-row-names-in-r/	1704817560
## Analyze coefficients for models with groups of features - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Analyze%20coefficients%20for%20models%20with%20groups%20of%20features&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704817597
## Interpreting models with lots of coefficients - Google Search	http://www.google.com/search?sca_esv=596923945&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=Interpreting%20models%20with%20lots%20of%20coefficients	1704820605
## How to Interpret P-values and Coefficients in Regression Analysis - Statistics By Jim	https://statisticsbyjim.com/regression/interpret-coefficients-p-values-regression/	1704844569
## set margins in Pandoc - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=set%20margins%20in%20Pandoc&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704844579
## r - Set margin size when converting from Markdown to PDF with pandoc - Stack Overflow	https://stackoverflow.com/questions/13515893/set-margin-size-when-converting-from-markdown-to-pdf-with-pandoc	1704921952
## argsort in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=argsort%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704921962
## User akrun - Stack Overflow	https://stackoverflow.com/users/3732271/akrun	1704922442
## r - how to get index of sorted array elements - Stack Overflow	https://stackoverflow.com/questions/27121133/how-to-get-index-of-sorted-array-elements	1704924896
## vim list all buffers - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=vim%20list%20all%20buffers&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704924906
## open a hidden buffer in vim - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=open%20a%20hidden%20buffer%20in%20vim&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704925177
## Vim buffer FAQ | Vim Tips Wiki | Fandom	https://vim.fandom.com/wiki/Vim_buffer_FAQ	1704925244
## can't access certain buffers vim - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=can't%20access%20certain%20buffers%20vim&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704925267
## can't access certain buffers neovim - Google Search	http://www.google.com/search?sca_esv=597319875&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=can't%20access%20certain%20buffers%20neovim	1705019750
## interpreting a model with a lot of coefficients - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=interpreting%20a%20model%20with%20a%20lot%20of%20coefficients&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705019763
## highly dimensional models - Google Search	http://www.google.com/search?sca_esv=597665655&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=highly%20dimensional%20models	1705019783
## Kinovea - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Kinovea&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705085580
## Kinovea	https://www.kinovea.org/	1705085592
## machine learning - When is a model high-dimensional? - Cross Validated	https://stats.stackexchange.com/questions/560082/when-is-a-model-high-dimensional	1705085855
## run kinovea on linux - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=run%20kinovea%20on%20linux&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705086066
## Google	http://www.google.com/	1705086068
## Reddit - Dive into anything	https://www.reddit.com/r/linux_gaming/comments/xanzzv/where_do_programs_get_installed_when_using_bottles/	1705086085
## where does bottles store application containers - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=where%20does%20bottles%20store%20application%20containers&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705086097
## bottles AUR - Google Search	http://www.google.com/search?sca_esv=597865028&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=bottles%20AUR	1705087915
## Bottles - ArchWiki	https://wiki.archlinux.org/title/Bottles	1705087920
## Installers - Bottles	https://docs.usebottles.com/bottles/installers	1705087969
## Welcome - Bottles	https://docs.usebottles.com/	1705088136
## Environments - Bottles	https://docs.usebottles.com/getting-started/environments	1705088147

## I added these, then cleared the cache.
## Pandoc two images side by side markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20two%20images%20side%20by%20side%20markdown&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704285905
## Pandoc Markdown to PDF image position - Stack Overflow	https://stackoverflow.com/questions/49482221/pandoc-markdown-to-pdf-image-position	1704285924
## two images side by side pandoc - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=two%20images%20side%20by%20side%20pandoc&btnG=Google%20Search&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704286315
## robust scaling - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=robust%20scaling&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704377845
## sklearn.preprocessing.RobustScaler — scikit-learn 1.3.2 documentation	https://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.RobustScaler.html	1704379301
## Bar plot in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Bar%20plot%20in%20R&btnG=Google%20Search&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704379321
## Quick-R: Bar Plots	https://www.statmethods.net/graphs/bar.html	1704379360
## slice indexing in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=slice%20indexing%20in%20R&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704379371
## How to slice data from a middle index until the end without using `length` in R (like you can in python)? - Stack Overflow	https://stackoverflow.com/questions/15127457/how-to-slice-data-from-a-middle-index-until-the-end-without-using-length-in-r	1704379567
## rotate barplot index label R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=rotate%20barplot%20index%20label%20R&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704379587
## graph - Rotating x axis labels in R for barplot - Stack Overflow	https://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot	1704379827
## valid R plotting colors - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=valid%20R%20plotting%20colors&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704379836
## Colors in R - Easy Guides - Wiki - STHDA	http://www.sthda.com/english/wiki/colors-in-r	1704379941
## check if a string is in another string R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=check%20if%20a%20string%20is%20in%20another%20string%20R&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704379952
## r - Test if characters are in a string - Stack Overflow	https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string	1704380001
## Google Images	https://www.google.com/imghp?hl=en&tab=wi	1704380005
## Google	https://www.google.com/webhp?tab=iw	1704380007
## test if a string contains another string in R - Google Search	https://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=test%20if%20a%20string%20contains%20another%20string%20in%20R&iflsig=AO6bgOgAAAAAZZbUgxf9-sjqVOcSYHNFrCQ3_Mt0W-Cr&gbv=1	1704380019
## If string contains x do y in R - Stack Overflow	https://stackoverflow.com/questions/69074035/if-string-contains-x-do-y-in-r	1704380277
## iterate through names in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=iterate%20through%20names%20in%20R&btnG=Google%20Search&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704380295
## r - Loop through data frame and variable names - Stack Overflow	https://stackoverflow.com/questions/16714020/loop-through-data-frame-and-variable-names	1704380463
## R argument is of length 0 - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20argument%20is%20of%20length%200&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704380475
## How to Fix in R: argument is of length zero - Statology	https://www.statology.org/r-argument-is-of-length-zero/	1704567972
## Numpy Citation - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Numpy%20Citation&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704567983
## NumPy - Citing NumPy	https://numpy.org/citing-numpy/	1704568464
## Pandoc using tex for figures breaks references - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20using%20tex%20for%20figures%20breaks%20references&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704572876
## latex multiple references in one \ref - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=latex%20multiple%20references%20in%20one%20%5cref&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704572891
## cross referencing - Reference multiple subfloated figures using \ref{X,Y,Z} to give Fig. 1a,1b,1c in text - TeX - LaTeX Stack Exchange	https://tex.stackexchange.com/questions/20698/reference-multiple-subfloated-figures-using-refx-y-z-to-give-fig-1a-1b-1c-in	1704573329
## R model summary to markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20model%20summary%20to%20markdown&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704573360
## knitr - Regression tables in Markdown format (for flexible use in R Markdown v2) - Stack Overflow	https://stackoverflow.com/questions/24342162/regression-tables-in-markdown-format-for-flexible-use-in-r-markdown-v2	1704573487
## Google	https://www.google.com/	1704573493
## how to close a file in neovim - Google Search	https://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20close%20a%20file%20in%20neovim&iflsig=ANes7DEAAAAAZZnIUE41iNZ88shitF2-2XLwoNGyJMZ9&gbv=1	1704573502
## How do I exit nvim? (neovim) - Stack Overflow	https://stackoverflow.com/questions/74369322/how-do-i-exit-nvim-neovim	1704573687
## how to write model summary to file in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20write%20model%20summary%20to%20file%20in%20R&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704573755
## R broom - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20broom&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704574230
## Introduction to broom	https://cran.r-project.org/web/packages/broom/vignettes/broom.html	1704574332
## Sign in - Google Accounts	https://accounts.google.com/v3/signin/identifier?continue=https%3A%2F%2Fscholar.google.com%2F&hl=en&ifkv=ASKXGp39q5LveESWlOMYjD--V63xAja-q_HRvn3_VP9sJ7F8qqSQi1sWtJcQ0iMH3zNs5GEBOeeQ4A&flowName=WebLiteSignIn&flowEntry=ServiceLogin&dsh=S2113985945%3A1704574385955412	1704574375
## Sign in - Google Accounts	https://accounts.google.com/v3/signin/rejected?continue=https://scholar.google.com/&dsh=S2113985945:1704574385955412&epd=AagDPRhYOIrmCJvCcpjwrC93LcXXByfURTj56WkCmFdfD3mDJ7mI_Yr4Qw&flowEntry=ServiceLogin&flowName=WebLiteSignIn&hl=en&idnf=pbattles@iu.edu&ifkv=ASKXGp39q5LveESWlOMYjD--V63xAja-q_HRvn3_VP9sJ7F8qqSQi1sWtJcQ0iMH3zNs5GEBOeeQ4A&rhlk=js&rrk=88	1704574396
## r - How to save summary(lm) to a file? - Stack Overflow	https://stackoverflow.com/questions/30371516/how-to-save-summarylm-to-a-file	1704575036
## knitr convert dataframe to markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=knitr%20convert%20dataframe%20to%20markdown&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704575057
## 10.1 The function knitr::kable() | R Markdown Cookbook	https://bookdown.org/yihui/rmarkdown-cookbook/kable.html	1704575441
## R change dataframe column names - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20change%20dataframe%20column%20names&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704575716
## pandoc automatically add contents of another file - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=pandoc%20automatically%20add%20contents%20of%20another%20file&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704575741
## Embedding one markdown document in another - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Embedding%20one%20markdown%20document%20in%20another&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704575794
## Markdown and including multiple files - Stack Overflow	https://stackoverflow.com/questions/4779582/markdown-and-including-multiple-files	1704575910
## Pandoc include contents of one markdown file in another - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20include%20contents%20of%20one%20markdown%20file%20in%20another&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704575932
## pandoc-include: Include other Markdown files	https://hackage.haskell.org/package/pandoc-include	1704581944
## how to add error bars to barplot R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20add%20error%20bars%20to%20barplot%20R&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704581966
## ggplot double vector - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=ggplot%20double%20vector&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704582209
## extract values from named numeric - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=extract%20values%20from%20named%20numeric&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704582581
## r - How do I extract just the number from a named number (without the name)? - Stack Overflow	https://stackoverflow.com/questions/15736719/how-do-i-extract-just-the-number-from-a-named-number-without-the-name	1704582586
## 2.1 Creating a Scatter Plot | R Graphics Cookbook, 2nd edition	https://r-graphics.org/recipe-quick-scatter	1704582862
## barplot with ggplot2 - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=barplot%20with%20ggplot2&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704582874
## ggplot2 barplots : Quick start guide - R software and data visualization - Easy Guides - Wiki - STHDA	http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization	1704582981
## arch linux ggplot2 not showing up - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=arch%20linux%20ggplot2%20not%20showing%20up&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704583045
## how to uninstall packages R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20uninstall%20packages%20R&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704583057
## ggplot2 plots not displaying - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=ggplot2%20plots%20not%20displaying&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704583072
## r - ggplot plots in scripts do not display in Rstudio - Stack Overflow	https://stackoverflow.com/questions/26643852/ggplot-plots-in-scripts-do-not-display-in-rstudio	1704583262
## ggplot plots not showing - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=ggplot%20plots%20not%20showing&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704583273
## "Plots" pane on Windows not showing ggplot2::geom_line() when it contains an alpha · Issue #2196 · rstudio/rstudio · GitHub	https://github.com/rstudio/rstudio/issues/2196	1704583658
## ggplot rotate labels - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=ggplot%20rotate%20labels&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704583667
## Barplot with error bars – the R Graph Gallery	https://r-graph-gallery.com/4-barplot-with-error-bar.html	1704644462
## r - Rotating x label text in ggplot - Stack Overflow	https://stackoverflow.com/questions/36682451/rotating-x-label-text-in-ggplot	1704651283
## compute explained variance for a generalized linear model - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=compute%20explained%20variance%20for%20a%20generalized%20linear%20model&iflsig=AO6bgOgAAAAAZZVk38DWlFGb4TR5KP6BMJHR-1tmYfdT&gbv=1	1704655125
## generalized linear model - Measure of explained variance for Poisson GLM (log-link function) - Cross Validated	https://stats.stackexchange.com/questions/124306/measure-of-explained-variance-for-poisson-glm-log-link-function	1704655176
## Google Scholar	https://scholar.google.com/	1704655181
## Sign in - Google Accounts	https://accounts.google.com/v3/signin/identifier?continue=https%3A%2F%2Fscholar.google.com%2F&hl=en&ifkv=ASKXGp3TG8lZx7nJKX7MBlWeIMY_kmVjcxVJSTe0YD6y5CBSWOVPlt6dM5xHgBKa5pO_UvfDUkFP&flowName=WebLiteSignIn&flowEntry=ServiceLogin&dsh=S1577781399%3A1704655203110086	1704655193
## Sign in - Google Accounts	https://accounts.google.com/v3/signin/rejected?continue=https://scholar.google.com/&dsh=S1577781399:1704655203110086&epd=AagDPRjAlhmlyORtavVh9bZTD_OyYLDj0nVaLccdJY-jYL7kchxXN_q4vg&flowEntry=ServiceLogin&flowName=WebLiteSignIn&hl=en&idnf=pbattles@iu.edu&ifkv=ASKXGp3TG8lZx7nJKX7MBlWeIMY_kmVjcxVJSTe0YD6y5CBSWOVPlt6dM5xHgBKa5pO_UvfDUkFP&rhlk=js&rrk=88	1704655201
## How to use Pandoc image alignment to align two images in the same row? - Stack Overflow	https://stackoverflow.com/questions/15367332/how-to-use-pandoc-image-alignment-to-align-two-images-in-the-same-row	1704655755
## find index of an item in an array R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=find%20index%20of%20an%20item%20in%20an%20array%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704656298
## indexing - Is there an R function for finding the index of an element in a vector? - Stack Overflow	https://stackoverflow.com/questions/5577727/is-there-an-r-function-for-finding-the-index-of-an-element-in-a-vector	1704656497
## data extraction and manipulation in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=data%20extraction%20and%20manipulation%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704656510
## dyplr - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=dyplr&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704656535
## A Grammar of Data Manipulation • dplyr	https://dplyr.tidyverse.org/	1704657724
## convert list to dataframe in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=convert%20list%20to%20dataframe%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704657736
## r - Convert a list to a data frame - Stack Overflow	https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame	1704658012
## Concatenating a list of data frames | R-bloggers	https://www.r-bloggers.com/2014/06/concatenating-a-list-of-data-frames/	1704658261
## add a row to a dataframe in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=add%20a%20row%20to%20a%20dataframe%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704658273
## dataframe - How to add a row to a data frame in R? - Stack Overflow	https://stackoverflow.com/questions/28467068/how-to-add-a-row-to-a-data-frame-in-r	1704658455
## specify shape of dataframe before adding data in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=specify%20shape%20of%20dataframe%20before%20adding%20data%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704658467
## dataframe - Shape data frame by adding zero in R - Stack Overflow	https://stackoverflow.com/questions/52668152/shape-data-frame-by-adding-zero-in-r	1704659574
## HTTP redirect	https://stat.ethz.ch/R-manual/R-devel/library/base/help/row.names.html	1704659594
## R set dataframe row names - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20set%20dataframe%20row%20names&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704659604
## How to change Row Names of DataFrame in R ? - GeeksforGeeks	https://www.geeksforgeeks.org/how-to-change-row-names-of-dataframe-in-r/	1704659971
## transpose dataframe in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=transpose%20dataframe%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704659981
## r - Transpose a data frame - Stack Overflow	https://stackoverflow.com/questions/6778908/transpose-a-data-frame	1704661012
## how to put a referencable table in markdown pandoc - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20put%20a%20referencable%20table%20in%20markdown%20pandoc&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661025
## Referencing inline-Latex tables in Pandoc Markdown - TeX - LaTeX Stack Exchange	https://tex.stackexchange.com/questions/540740/referencing-inline-latex-tables-in-pandoc-markdown	1704661116
## Pandoc reference a table in markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20reference%20a%20table%20in%20markdown&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661135
## Referencing tables in pandoc - TeX - LaTeX Stack Exchange	https://tex.stackexchange.com/questions/139106/referencing-tables-in-pandoc	1704661208
## AUR pandoc-tablenos - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=AUR%20pandoc-tablenos&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661525
## AUR (en) - pandoc-tablenos	https://aur.archlinux.org/packages/pandoc-tablenos	1704661531
## AUR (en) - pandoc-xnos	https://aur.archlinux.org/packages/pandoc-xnos	1704661669
## pandoc reference table in markdown - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=pandoc%20reference%20table%20in%20markdown&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704661909
## Pandoc - Pandoc User’s Guide	https://pandoc.org/MANUAL.html	1704662276
## Pandoc \ref for table - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Pandoc%20%5cref%20for%20table&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704662287
## GitHub - tomduck/pandoc-xnos: Library code for pandoc-fignos/eqnos/tablenos/secnos.	https://github.com/tomduck/pandoc-xnos	1704662380
## GitHub - tomduck/pandoc-tablenos: A pandoc filter for numbering tables and table references.	https://github.com/tomduck/pandoc-tablenos	1704662385
## Google Image Result for https://opengraph.githubassets.com/f063d1b0b362cdbcc529713e2bff2ae94dc53d6431673e07d2bb3375d6190e61/tomduck/pandoc-xnos/issues/11	https://www.google.com/imgres?imgurl=https://opengraph.githubassets.com/f063d1b0b362cdbcc529713e2bff2ae94dc53d6431673e07d2bb3375d6190e61/tomduck/pandoc-xnos/issues/11&imgrefurl=https://github.com/tomduck/pandoc-xnos/issues/11&h=600&w=1200&tbnid=hH3Mheadt60YyM&q=pandoc-tablenos+cannot+understand+pandocversion%3D3.1.6&tbnh=75&tbnw=150&usg=AI4_-kS3xTEF7RqKLKKndVva__bWdC6LRg&vet=1&docid=02q66oX_8hYfaM&hl=en&sa=X&ved=2ahUKEwi0otCymsyDAxXdHzQIHVx4B0QQ9QF6BAgAEAQ	1704662489
## pandoc-tablenos cannot understand pandocversion=3.1.6 - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=pandoc-tablenos%20cannot%20understand%20pandocversion%3d3.1.6&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704662494
## `--filter pandoc-xnos` not working · Issue #11 · tomduck/pandoc-xnos · GitHub	https://github.com/tomduck/pandoc-xnos/issues/11	1704663067
## R row-wise mean for dataframe - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20row-wise%20mean%20for%20dataframe&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704663080
## row - R dplyr rowwise mean or min and other methods? - Stack Overflow	https://stackoverflow.com/questions/31598935/r-dplyr-rowwise-mean-or-min-and-other-methods	1704727786
## Spearman rank test - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Spearman%20rank%20test&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704727815
## Spearman's rank correlation coefficient - Wikipedia	https://en.wikipedia.org/wiki/Spearman's_rank_correlation_coefficient	1704731267
## how to index dataframe with list R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20index%20dataframe%20with%20list%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704731282
## Chapter 3 Working with lists and data frames | Population Health Data Science with R	https://bookdown.org/taragonmd/phds/working-with-lists-and-data-frames.html	1704741700
## index dataframe by row R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=index%20dataframe%20by%20row%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704741713
## How to Select Rows by Index in R (With Examples) - Statology	https://www.statology.org/r-select-rows-by-index/	1704742379
## R drop last column of dataframe - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20drop%20last%20column%20of%20dataframe&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704742390
## r - Drop last 5 columns from a dataframe without knowing specific number - Stack Overflow	https://stackoverflow.com/questions/26483643/drop-last-5-columns-from-a-dataframe-without-knowing-specific-number	1704742485
## take mean of an entire dataframe - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=take%20mean%20of%20an%20entire%20dataframe&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704742497
## take mean of an entire dataframe R - Google Search	http://www.google.com/search?sca_esv=596620277&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=take%20mean%20of%20an%20entire%20dataframe%20R	1704742503
## How to find the mean of all values in an R data frame? - GeeksforGeeks	https://www.geeksforgeeks.org/how-to-find-the-mean-of-all-values-in-an-r-data-frame/	1704743255
## convert list to vector in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=convert%20list%20to%20vector%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704743267
## Converting a List to Vector in R Language - unlist() Function - GeeksforGeeks	https://www.geeksforgeeks.org/converting-a-list-to-vector-in-r-language-unlist-function/	1704743470
## HTTP redirect	https://stat.ethz.ch/R-manual/R-devel/library/base/help/typeof.html	1704743901
## get object type in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=get%20object%20type%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704743904
## how to index R dataframe by columns programmatically - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=how%20to%20index%20R%20dataframe%20by%20columns%20programmatically&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704743941
## How to Select DataFrame Columns by Index in R? - GeeksforGeeks	https://www.geeksforgeeks.org/how-to-select-dataframe-columns-by-index-in-r/	1704744191
## dplyr select for rows - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=dplyr%20select%20for%20rows&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704744211
## Keep rows that match a condition — filter • dplyr	https://dplyr.tidyverse.org/reference/filter.html	1704744460
## R select rows by name without hard coding - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=R%20select%20rows%20by%20name%20without%20hard%20coding&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704744486
## Subset DataFrame and Matrix by Row Names in R - GeeksforGeeks	https://www.geeksforgeeks.org/subset-dataframe-and-matrix-by-row-names-in-r/	1704817560
## Analyze coefficients for models with groups of features - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Analyze%20coefficients%20for%20models%20with%20groups%20of%20features&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704817597
## Interpreting models with lots of coefficients - Google Search	http://www.google.com/search?sca_esv=596923945&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=Interpreting%20models%20with%20lots%20of%20coefficients	1704820605
## How to Interpret P-values and Coefficients in Regression Analysis - Statistics By Jim	https://statisticsbyjim.com/regression/interpret-coefficients-p-values-regression/	1704844569
## set margins in Pandoc - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=set%20margins%20in%20Pandoc&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704844579
## r - Set margin size when converting from Markdown to PDF with pandoc - Stack Overflow	https://stackoverflow.com/questions/13515893/set-margin-size-when-converting-from-markdown-to-pdf-with-pandoc	1704921952
## argsort in R - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=argsort%20in%20R&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704921962
## User akrun - Stack Overflow	https://stackoverflow.com/users/3732271/akrun	1704922442
## r - how to get index of sorted array elements - Stack Overflow	https://stackoverflow.com/questions/27121133/how-to-get-index-of-sorted-array-elements	1704924896
## vim list all buffers - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=vim%20list%20all%20buffers&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704924906
## open a hidden buffer in vim - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=open%20a%20hidden%20buffer%20in%20vim&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704925177
## Vim buffer FAQ | Vim Tips Wiki | Fandom	https://vim.fandom.com/wiki/Vim_buffer_FAQ	1704925244
## can't access certain buffers vim - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=can't%20access%20certain%20buffers%20vim&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1704925267
## can't access certain buffers neovim - Google Search	http://www.google.com/search?sca_esv=597319875&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=can't%20access%20certain%20buffers%20neovim	1705019750
## interpreting a model with a lot of coefficients - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=interpreting%20a%20model%20with%20a%20lot%20of%20coefficients&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705019763
## highly dimensional models - Google Search	http://www.google.com/search?sca_esv=597665655&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=highly%20dimensional%20models	1705019783
## Kinovea - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Kinovea&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705085580
## Kinovea	https://www.kinovea.org/	1705085592
## machine learning - When is a model high-dimensional? - Cross Validated	https://stats.stackexchange.com/questions/560082/when-is-a-model-high-dimensional	1705085855
## run kinovea on linux - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=run%20kinovea%20on%20linux&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705086066
## Reddit - Dive into anything	https://www.reddit.com/r/linux_gaming/comments/xanzzv/where_do_programs_get_installed_when_using_bottles/	1705086085
## where does bottles store application containers - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=where%20does%20bottles%20store%20application%20containers&iflsig=ANes7DEAAAAAZZsLvMpKMi0xYoo8mWBPx6KJ5QAbFuWa&gbv=1	1705086097
## bottles AUR - Google Search	http://www.google.com/search?sca_esv=597865028&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=bottles%20AUR	1705087915
## Bottles - ArchWiki	https://wiki.archlinux.org/title/Bottles	1705087920
## Installers - Bottles	https://docs.usebottles.com/bottles/installers	1705087969
## Welcome - Bottles	https://docs.usebottles.com/	1705088136
## Environments - Bottles	https://docs.usebottles.com/getting-started/environments	1705088147
## where does bottles store different application containers - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=where%20does%20bottles%20store%20different%20application%20containers&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705088877
## search entire system linux - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=search%20entire%20system%20linux&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705088889
## Linux: How can I find a file on my system? - nixCraft	https://www.cyberciti.biz/faq/linux-how-can-i-find-a-file-on-my-system/	1705089259
## kino and raw1394 problem / Multimedia and Games / Arch Linux Forums	https://bbs.archlinux.org/viewtopic.php?id=38826	1705089293
## Use Kinovea on arch linux - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Use%20Kinovea%20on%20arch%20linux&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705089349
## Kinovea on linux - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Kinovea%20on%20linux&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705089362
## Kinovea on Linux (Page 1) — General — Kinovea - Forums	https://www.kinovea.org/en/forum/viewtopic.php?id=178	1705089606
## manjaro keyboard input super slow - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=manjaro%20keyboard%20input%20super%20slow&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705089952
## Ultralytics pose - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Ultralytics%20pose&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705103265
## Predict - Ultralytics YOLOv8 Docs	https://docs.ultralytics.com/modes/predict/	1705103388
## which version of .net does Kinovea use - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=which%20version%20of%20.net%20does%20Kinovea%20use&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705106728
## soffice calc save as pdf - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=soffice%20calc%20save%20as%20pdf&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705153306
## python sysargs - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=python%20sysargs&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705153316
## python process arguments - Google Search	http://www.google.com/search?sca_esv=598151369&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=python%20process%20arguments	1705153332
## python documentation command line arguments - Google Search	http://www.google.com/search?sca_esv=598151369&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=python%20documentation%20command%20line%20arguments	1705153349
## How to say always wear underwear in latin - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=How%20to%20say%20always%20wear%20underwear%20in%20latin&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705153476
## argparse — Parser for command-line options, arguments and sub-commands — Python 3.12.1 documentation	https://docs.python.org/3/library/argparse.html	1705155004
## Search — Python 3.7.17 documentation	https://docs.python.org/3.7/search.html?q=kwargs	1705155032
## Search — Python 3.7.17 documentation	https://docs.python.org/3.7/search.html?q=kwarg	1705155039
## Python documentation kwargs - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Python%20documentation%20kwargs&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705173374
## Python generator - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Python%20generator&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705173632
## Python documentation generator - Google Search	http://www.google.com/search?sca_esv=598210265&hl=en&gbv=1&ie=ISO-8859-1&oq=&aqs=&q=Python%20documentation%20generator	1705173648
## python documentation generator - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=python%20documentation%20generator&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705173720
## pydoc — Documentation generator and online help system — Python 3.12.1 documentation	https://docs.python.org/3/library/pydoc.html	1705174009
## Ultralytics YOLOv8 Pose predict - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=Ultralytics%20YOLOv8%20Pose%20predict&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705174019
## Pose - Ultralytics YOLOv8 Docs	https://docs.ultralytics.com/tasks/pose/	1705175155
## python documentation importlib - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=python%20documentation%20importlib&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705175166
## Built-in Functions — Python 3.12.1 documentation	https://docs.python.org/3/library/functions.html	1705175369
## import python module that is not in the current directory - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=import%20python%20module%20that%20is%20not%20in%20the%20current%20directory&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705175381
## Learn Python: How To Import Libraries From Another Directory	https://ioflood.com/blog/python-import-from-another-directory/	1705176703
## OpenCV2 documentation - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=OpenCV2%20documentation&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705176713
## OpenCV: OpenCV modules	https://docs.opencv.org/4.x/	1705176718
## OpenCV: Video I/O	https://docs.opencv.org/4.x/dd/de7/group__videoio.html	1705177018
## pygame documentation - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=pygame%20documentation&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705177027
## Pygame Front Page — pygame v2.6.0 documentation	https://www.pygame.org/docs/	1705177032
## pygame.event — pygame v2.6.0 documentation	https://www.pygame.org/docs/ref/event.html	1705177036
## pygame get screen dimensions - Google Search	http://www.google.com/search?ie=ISO-8859-1&hl=en&source=hp&q=pygame%20get%20screen%20dimensions&iflsig=ANes7DEAAAAAZaGk6TYyhzIPi_fcrhb1-uTsBwS9Edyc&gbv=1	1705178547
## pygame.display — pygame v2.6.0 documentation	https://www.pygame.org/docs/ref/display.html	1705178557
## pygame.Surface — pygame v2.6.0 documentation	https://www.pygame.org/docs/ref/surface.html	1705258609
## "elinks" export all open tabs to ".txt" at DuckDuckGo	https://lite.duckduckgo.com/lite/?q=%22elinks%22%20export%20all%20open%20tabs%20to%20%22.txt%22&norw=1	1705258648
## Google	http://www.google.com/	1705260365
## pygame.key — pygame v2.6.0 documentation	https://www.pygame.org/docs/ref/key.html	1705265667
## python - Import module from subfolder - Stack Overflow	https://stackoverflow.com/questions/8953844/import-module-from-subfolder	1705265961
## importlib — The implementation of import — Python 3.12.1 documentation	https://docs.python.org/3/library/importlib.html	1705265998
## DuckDuckGo	https://lite.duckduckgo.com/lite/	1705266002
## python 3.x - How to import a module from a subfolder in python3 (with empty __init__.py) - Stack Overflow	https://stackoverflow.com/questions/48129733/how-to-import-a-module-from-a-subfolder-in-python3-with-empty-init-py	1705266012
## JSON Syntax	https://www.w3schools.com/js/js_json_syntax.asp	1705268193
## plot - R is plotting labels off the page - Stack Overflow	https://stackoverflow.com/questions/2807060/r-is-plotting-labels-off-the-page	1705432079
## r - Set plot margin of png plot device using par - Stack Overflow	https://stackoverflow.com/questions/14878217/set-plot-margin-of-png-plot-device-using-par	1705433915
## Google	http://www.google.com/	1705433917
## Manjaro acting very slow - Support - Manjaro Linux Forum	https://forum.manjaro.org/t/manjaro-acting-very-slow/20093	1705433937
## Physiology, Skeletal Muscle - StatPearls - NCBI Bookshelf	https://www.ncbi.nlm.nih.gov/books/NBK537139/	1705509309
## python - NumPy, RuntimeWarning: invalid value encountered in power - Stack Overflow	https://stackoverflow.com/questions/45384602/numpy-runtimewarning-invalid-value-encountered-in-power	1705618860
## How to Set Plot Background Color in Matplotlib? - GeeksforGeeks	https://www.geeksforgeeks.org/how-to-set-plot-background-color-in-matplotlib/	1705681422
## Specifying colors — Matplotlib 3.8.2 documentation	https://matplotlib.org/stable/users/explain/colors/colors.html	1705681768
## https://www.color-name.com/color-image?c=C0C8CF&square&tx	https://www.color-name.com/color-image?c=C0C8CF&square&tx	1705681816
## Light Blue Grey color hex code is #C0C8CF	https://www.color-name.com/light-blue-grey.color	1705682949
## Text, labels and annotations — Matplotlib 3.8.2 documentation	https://matplotlib.org/stable/gallery/text_labels_and_annotations/index.html	1705682970
## Annotating a plot — Matplotlib 3.8.2 documentation	https://matplotlib.org/stable/gallery/text_labels_and_annotations/annotation_basic.html	1705687766
## orthography - Timepoint vs. time point - English Language & Usage Stack Exchange	https://english.stackexchange.com/questions/63098/timepoint-vs-time-point	1705798760
## DuckDuckGo	https://lite.duckduckgo.com/lite/	1705798768
## Pandoc - Pandoc User’s Guide	https://pandoc.org/MANUAL.html	1705798781
## https://duckduckgo.com/l/?uddg=https://pandoc.org/chunkedhtml-demo/10.4-styling-the-slides.html&rut=be2de6ab5603ef0f55e5971d7508f9452c1bd76fab679ace9697105af6b20bad	https://duckduckgo.com/l/?uddg=https%3A%2F%2Fpandoc.org%2Fchunkedhtml%2Ddemo%2F10.4%2Dstyling%2Dthe%2Dslides.html&rut=be2de6ab5603ef0f55e5971d7508f9452c1bd76fab679ace9697105af6b20bad	1705798859
## https://duckduckgo.com/l/?uddg=https://pandoc.org/chunkedhtml-demo/6.2-variables.html&rut=9d262e664e7fc6d451906466976114167cba46b52ccceeeede44b3ac431716d0	https://duckduckgo.com/l/?uddg=https%3A%2F%2Fpandoc.org%2Fchunkedhtml%2Ddemo%2F6.2%2Dvariables.html&rut=9d262e664e7fc6d451906466976114167cba46b52ccceeeede44b3ac431716d0	1705798864
## beamer at DuckDuckGo	https://lite.duckduckgo.com/lite/?q=beamer&sites=pandoc.org	1705798865
## https://www.statology.org/r-find-character-in-string/
## https://www.google.com/search?client=firefox-b-1-d&q=string+indexof+in+R
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/substr.html
## https://stackoverflow.com/questions/30265728/in-r-base-plot-move-axis-label-closer-to-axis
## https://stackoverflow.com/questions/30265728/in-r-base-plot-move-axis-label-closer-to-axis
## https://stackoverflow.com/questions/20745426/cex-axis-only-affects-y-axis-not-x-axis
## https://www.digitalocean.com/community/tutorials/strsplit-function-in-r
## https://www.geeksforgeeks.org/replace-specific-characters-in-string-in-r/
## https://www.geeksforgeeks.org/replace-specific-characters-in-string-in-r/
## https://stat.ethz.ch/R-manual/R-patched/library/base/html/get.html
## https://stat.ethz.ch/R-manual/R-patched/library/base/html/get.html
## https://www.geeksforgeeks.org/extract-just-number-from-named-numeric-vector-in-r/
## https://stackoverflow.com/questions/10655287/r-deep-vs-shallow-copies-pass-by-reference
## https://www.reddit.com/r/Rlanguage/comments/v68cpu/how_do_i_make_a_deep_copy_of_a_data_frame/
## https://stats.stackexchange.com/questions/3853/how-to-increase-size-of-label-fonts-in-barplot
## https://r-graph-gallery.com/209-the-options-of-barplot.html
## https://community.rstudio.com/t/shading-portion-of-each-bar-in-a-bar-chart/73615
## https://r-graph-gallery.com/4-barplot-with-error-bar.html
## https://www.r-bloggers.com/2015/08/building-barplots-with-error-bars/
## https://stats.stackexchange.com/questions/27511/extract-standard-errors-of-coefficient-linear-regression-r
## https://stats.stackexchange.com/questions/27511/extract-standard-errors-of-coefficient-linear-regression-r
## https://www.econometrics-with-r.org/5.2-cifrc.html
## https://stats.stackexchange.com/questions/398737/pairwise-comparisons-of-regression-coefficients
## https://stats.stackexchange.com/questions/398737/pairwise-comparisons-of-regression-coefficients
## https://www.google.com/search?client=firefox-b-1-d&q=determine+if+two+model+coefficients+are+different
## https://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions
## https://www.youtube.com/watch?v=5wyeMOG6eZ0
## https://www.google.com/search?client=firefox-b-1-d&q=testing+two+model+coefficients+for+equality
## https://www.youtube.com/watch?v=qQgDqgF_arw
## https://www.google.com/search?client=firefox-b-1-d&q=R+test+equality+of+two+model+coefficients
## https://stackoverflow.com/questions/37591550/testing-the-equality-of-multiple-coefficients-in-r
## https://stackoverflow.com/questions/37591550/testing-the-equality-of-multiple-coefficients-in-r
## https://www.r-bloggers.com/2021/02/testing-the-equality-of-regression-coefficients/
## https://www.google.com/search?client=firefox-b-1-d&q=should+error+bars+be+standard+deviation+or+standard+error
## https://en.wikipedia.org/wiki/Standard_error
## https://www.google.com/search?client=firefox-b-1-d&q=what+is+familywise+error
## https://en.wikipedia.org/wiki/Family-wise_error_rate
## https://www.google.com/search?client=firefox-b-1-d&q=how+to+compare+model+coefficients
## https://www.econometrics-with-r.org/5.2-cifrc.html
## https://rpubs.com/aaronsc32/spearman-rank-correlation
## https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
## https://stackoverflow.com/questions/68010125/is-there-a-statistical-test-that-can-compare-two-ordered-lists
## https://stats.stackexchange.com/questions/79597/test-whether-two-rank-orders-differ
## https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test
## https://www.google.com/search?client=firefox-b-1-d&q=Spearman+Rank+correlation
## https://forum.posit.co/t/how-to-clear-the-r-environment/14303
## https://r-graph-gallery.com/77-turn-y-axis-upside-down.html
## https://www.statology.org/linearhypothesis-r/
## https://search.r-project.org/CRAN/refmans/car/html/linearHypothesis.html
## https://kzee.github.io/CoeffDiff_Demo.html
## https://builtin.com/data-science/step-step-explanation-principal-component-analysis
## https://stackoverflow.com/questions/60722913/r-code-to-test-the-difference-between-coefficients-of-regressors-from-one-panel
## https://stats.stackexchange.com/questions/478408/compare-regression-coefficients-within-the-same-model
## https://www.google.com/search?channel=ftrc&client=firefox-b-1-d&q=if+two+means+are+not+equal+and+one+number+is+greater+than+the+other%2C+can+I+say+that+one+is+greater
## https://www.rdocumentation.org/packages/car/versions/3.1-2/topics/linearHypothesis
## https://stackoverflow.com/questions/54611507/how-to-interpret-the-results-of-linearhypothesis-function-when-comparing-regress
## https://stackoverflow.com/questions/54611507/how-to-interpret-the-results-of-linearhypothesis-function-when-comparing-regress
## https://www.google.com/search?client=firefox-b-1-d&q=latex+subcaption

