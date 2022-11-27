salaries <- read.csv("salaries_clean.csv")
View(salaries)

df <- salaries[, c("job_title_category", "total_experience_years", "employer_experience_years", "annual_base_pay", "signing_bonus", "annual_bonus")]
View(df)

check_NaN <- apply(apply(df, 2, is.na), 2, which)
View(check_NaN)


df$annual_base_pay[df$annual_base_pay == 0] <- NA
df <- na.omit(df)
View(df)


#data visualization
#calculate mean, median, standard deviation, min, max,...

continuous_var <- as.data.frame(apply(df[, c("total_experience_years", "employer_experience_years", "annual_base_pay", "signing_bonus", "annual_bonus")], 2, function(x) c(mean(x), median(x), sd(x), max(x), min(x))))

rownames(continuous_var) <- c("mean", "median", "sd", "max", "min")

View(continuous_var)

#create statistics for categories

# <- apply(df[, c("Applied Science")], 2, table)
#(categorical_variable)


cal_statistics <- function(x){
  length_salaries <- tapply(x$annual_base_pay, x$job_title_category, length)
  mean_salaries <- tapply(x$annual_base_pay, x$job_title_category, mean)
  sd_salaries <- tapply(x$annual_base_pay, x$job_title_category, sd)
  max_salaries <- tapply(x$annual_base_pay, x$job_title_category, max)
  min_salaries <- tapply(x$annual_base_pay, x$job_title_category, min)
  q25_salaries <- tapply(x$annual_base_pay, x$job_title_category, quantile, probs = 0.25, na.rm = TRUE)
  q75_salaries <- tapply(x$annual_base_pay, x$job_title_category, quantile, probs = 0.75, na.rm = TRUE)
  
  return (cbind(length_salaries, mean_salaries, sd_salaries, min_salaries, max_salaries ,q25_salaries, q75_salaries))
}
descriptive_stats <- cal_statistics(df)
View(descriptive_stats)


boxplot(df$annual_base_pay ~ df$job_title_category, main = "Distribution", xlab = "job_title-category", ylab = "annual_base_pay", col = "cyan")


#remove outliers

remove_outliers <- function(x) {
  tmp <- subset(df, df$job_title_category == x)
  qnt <- quantile(tmp$annual_base_pay, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5*IQR(tmp$annual_base_pay, na.rm = TRUE)
  tmp$annual_base_pay[tmp$annual_base_pay < (qnt[1] - H)] <- NA
  tmp$annual_base_pay[tmp$annual_base_pay > qnt[2] + H] <- NA
  return(tmp)
}

applied_science <- remove_outliers("Applied Science")
data <- remove_outliers("Data")
engineering <- remove_outliers("Engineering")
management <- remove_outliers("Management")
operations <- remove_outliers("Operations")
other <- remove_outliers("Other")
software <- remove_outliers("Software")
web <- remove_outliers("Web")

df <- rbind(applied_science, data, engineering, management, operations, other, software, web)
df <- na.omit(df)
View(df)
boxplot(df$annual_base_pay ~ df$job_title_category, main = "Distribution", xlab = "job_title-category", ylab = "annual_base_pay", col = "cyan")

new_stats <- cal_statistics(df)
View(new_stats)

hist(df$annual_base_pay, main = "Distribution of point in annual_base_pay", xlab = "annual_base_pay", ylab = "Number of people", xlim = c(0,300000), ylim = c(0,300), labels = TRUE, col = "cyan")

pairs(df$annual_base_pay ~ df$total_experience_years, main = "Distribution of point in annual_base_pay by total_experience_years", labels = c("annual_base_pay", "total_experience_years"), col = "pink")
pairs(df$annual_base_pay ~ df$employer_experience_years, main = "Distribution of annual_base_pay by employer_experience_years", labels = c("annual_base_pay", "employer_experience_years"), col = "pink")
pairs(df$annual_base_pay ~ df$signing_bonus, main = "Distribution of annual_base_pay by signing_bonus", labels = c("annual_base_pay", "signing_bonus"), col = "pink")
pairs(df$annual_base_pay ~ df$annual_bonus, main = "Distribution of annual_base_pay by annual_bonus", labels = c("annual_base_pay", "annual_bonus"), col = "pink")













