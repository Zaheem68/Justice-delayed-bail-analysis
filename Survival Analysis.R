
library(data.table)
library(dplyr)
library(lubridate)

print("Loading massive dataset...")
population_data <- fread("E:/Air University/4th sem/Applied Stats Lab/Applied Stats Project/Bail - Case dataset/Compiled Bail case data.csv")

# 3. Data Cleaning
clean_data <- population_data %>%
  mutate(
    DATE_FILED = ymd(DATE_FILED), 
    DECISION_DATE = ymd(DECISION_DATE),
    LAST_SYNC_TIME = ymd(LAST_SYNC_TIME),
    
   
    Duration_Days = as.numeric(difftime(
      if_else(is.na(DECISION_DATE), LAST_SYNC_TIME, DECISION_DATE), 
      DATE_FILED, 
      units = "days"
    )),
    
    # Survival Analysis ke liye Censoring Variable
    Status = if_else(CURRENT_STATUS == "Disposed", 1, 0)
  ) %>%
  # Missing aur invalid rows ko filter out karein
  filter(!is.na(Duration_Days) & Duration_Days > 0)

# 4. 10,000 ka Simple Random Sample (SRS) Nikalna
set.seed(123) # Seed fix kiya taake hamesha same 10k log select hon
sample_10k <- clean_data %>% sample_n(10000)

# 5. SPSS ke liye file Save karna
write.csv(sample_10k, "Cleaned_10k_Sample_For_SPSS.csv", row.names = FALSE)
save_path <- "E:/Air University/4th sem/Applied Stats Lab/Applied Stats Project/Cleaned_10k_Sample_For_SPSS.csv"

write.csv(sample_10k, save_path, row.names = FALSE)

print("File forcibly saved to your E: drive folder successfully!")
print("10,000 SRS File Saved Successfully for SPSS!")

# ---------------------------------------------------------
# PHASE 0: EXPLORATORY DATA ANALYSIS (EDA)
# ---------------------------------------------------------

library(ggplot2)

cat("\n--- PHASE 0: STARTING EDA (EXPLORATORY DATA ANALYSIS) ---\n")

# Tumhara main folder ka path jahan sab kuch save hoga
save_folder <- "E:/Air University/4th sem/Applied Stats Lab/Applied Stats Project/"

# 1. FEATURE 1: Bail Types ki Distribution
cat("\nGenerating and Saving Bail Type Distribution Plot...\n")
plot_bail_types <- ggplot(sample_10k, aes(x = Mapped_Bail, fill = Mapped_Bail)) +
  geom_bar(color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Bail Types", x = "Bail Type", y = "Number of Cases") +
  theme(legend.position = "none")

print(plot_bail_types)
# Yeh line automatically is graph ko tumhare folder mein save kar degi
ggsave(paste0(save_folder, "EDA_1_Bail_Types.png"), plot = plot_bail_types, width = 8, height = 6, dpi = 300)


# 2. FEATURE 2: Case Status
sample_10k_eda <- sample_10k %>%
  mutate(Status_Label = if_else(Status == 1, "Disposed", "Pending"))

cat("\nGenerating and Saving Case Status Plot...\n")
plot_status <- ggplot(sample_10k_eda, aes(x = Status_Label, fill = Status_Label)) +
  geom_bar(color = "black", width = 0.5, alpha = 0.8) +
  scale_fill_manual(values = c("Disposed" = "#2ecc71", "Pending" = "#e74c3c")) +
  theme_minimal() +
  labs(title = "Case Status: Disposed vs Pending", x = "Current Status", y = "Count") +
  theme(legend.position = "none")

print(plot_status)
# Auto-save
ggsave(paste0(save_folder, "EDA_2_Case_Status.png"), plot = plot_status, width = 8, height = 6, dpi = 300)


# 3. FEATURE 3: Boxplot
cat("\nGenerating and Saving Duration Boxplot...\n")
plot_boxplot <- ggplot(sample_10k, aes(x = Mapped_Bail, y = Duration_Days, fill = Mapped_Bail)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
  theme_minimal() +
  labs(title = "Boxplot of Court Duration by Bail Type",
       subtitle = "Red stars indicate extreme outliers (cases delayed for years)",
       x = "Bail Type", y = "Duration in Days") +
  theme(legend.position = "none")

print(plot_boxplot)
# Auto-save
ggsave(paste0(save_folder, "EDA_3_Duration_Boxplot.png"), plot = plot_boxplot, width = 8, height = 6, dpi = 300)

cat("\n--- EDA COMPLETE. ALL 3 GRAPHS SAVED IN YOUR FOLDER! ---\n")
# ---------------------------------------------------------
# PHASE 1: THE REALITY CHECK (NORMALITY TESTING)
# ---------------------------------------------------------

# 1. Load required libraries
library(dplyr)

# 2. Load the 10k sample we saved earlier
# (Make sure your working directory is set to where the file is saved)
sample_10k <- read.csv("E:/Air University/4th sem/Applied Stats Lab/Applied Stats Project/Cleaned_10k_Sample_For_SPSS.csv")

# 3. VISUAL NORMALITY CHECKS 
# We will display two plots side-by-side to show the examiner the exact shape of the data.
par(mfrow=c(1,2)) # This splits the plot window into 1 row, 2 columns

# Plot A: The Histogram
hist(sample_10k$Duration_Days, 
     main = "Histogram of Bail Case Durations", 
     xlab = "Days in Court", 
     ylab = "Frequency",
     col = "darkred", 
     breaks = 50)

# Plot B: The Q-Q Plot (Quantile-Quantile Plot)
# If the data were perfectly normal, the dots would strictly follow the diagonal line.
qqnorm(sample_10k$Duration_Days, main = "Q-Q Plot of Case Durations")
qqline(sample_10k$Duration_Days, col = "blue", lwd = 2)

# Reset plot window to normal
par(mfrow=c(1,1))

# 4. FORMAL NORMALITY TEST (Kolmogorov-Smirnov Test)
# Note: Shapiro-Wilk fails above 5000 rows, so we use KS-Test for large samples.
# We are comparing our data's distribution against a theoretical perfect Normal distribution.

cat("\n--- Kolmogorov-Smirnov Test for Normality ---\n")
ks_result <- ks.test(sample_10k$Duration_Days, 
                     "pnorm", 
                     mean = mean(sample_10k$Duration_Days), 
                     sd = sd(sample_10k$Duration_Days))

print(ks_result)

# Interpretation Printout
if(ks_result$p.value < 0.05) {
  cat("\nCONCLUSION: The p-value is less than 0.05. The data is NOT normally distributed.\n")
  cat("Parametric tests (like T-tests) are strictly invalid at this stage.\n")
} else {
  cat("\nCONCLUSION: The data is normally distributed.\n")
}
# ---------------------------------------------------------
# PHASE 2: THE FIRST ATTEMPT (NON-PARAMETRIC TEST)
# ---------------------------------------------------------

# 1. Filter the dataset to include ONLY the two bail types we want to compare
filtered_sample <- sample_10k %>% 
  filter(Mapped_Bail %in% c("REGULAR BAIL", "ANTICIPATORY BAIL"))

# 2. Calculate the actual Median Days to get our real-world insight
cat("\n--- Median Court Duration by Bail Type ---\n")
medians <- filtered_sample %>%
  group_by(Mapped_Bail) %>%
  summarize(Median_Days = median(Duration_Days, na.rm = TRUE),
            Total_Cases = n())
print(medians)

# 3. Run the Formal Mann-Whitney U Test (Wilcoxon Rank Sum Test)
cat("\n--- Mann-Whitney U Test Results ---\n")
mw_result <- wilcox.test(Duration_Days ~ Mapped_Bail, data = filtered_sample)
print(mw_result)

# 4. Interpretation Printout
if(mw_result$p.value < 0.05) {
  cat("\nCONCLUSION: The p-value is < 0.05. There is a statistically significant difference in court delays between Anticipatory and Regular Bail.\n")
} else {
  cat("\nCONCLUSION: There is NO significant difference between the two bail types.\n")
}

# ---------------------------------------------------------
# PHASE 3: THE MATHEMATICAL FIX (TRANSFORMATION & PARAMETRIC)
# ---------------------------------------------------------

cat("\n--- PHASE 3: PARAMETRIC TEST PREPARATION ---\n")

# 1. The Trap: We MUST drop pending cases to use Parametric tests
# We only keep Disposed/Resolved cases (Status == 1)
disposed_cases <- filtered_sample %>% 
  filter(Status == 1)

dropped_count <- nrow(filtered_sample) - nrow(disposed_cases)
cat("\nCRITICAL WARNING: Dropped", dropped_count, "pending cases just to satisfy Parametric test assumptions! This is a massive loss of real-world data.\n")

# 2. Apply Log Transformation to fix the right-skewness
disposed_cases <- disposed_cases %>%
  mutate(Log_Duration = log(Duration_Days))

# 3. Visual Check: Did the transformation work?
# We plot the new Log_Duration. It should look much more like a Bell Curve.
hist(disposed_cases$Log_Duration, 
     main = "Histogram of LOG(Duration_Days)", 
     xlab = "Log transformed Days in Court", 
     col = "forestgreen", 
     breaks = 50)

# 4. The Parametric Test (Independent Samples T-Test)
# Now that the data is "normal", we run the standard T-test.
cat("\n--- Independent Samples T-Test Results (On Log Data) ---\n")
t_result <- t.test(Log_Duration ~ Mapped_Bail, data = disposed_cases)
print(t_result)

# 5. Interpretation Printout
if(t_result$p.value < 0.05) {
  cat("\nCONCLUSION: Even after transformation and losing pending data, the Parametric T-Test shows a significant difference between Anticipatory and Regular Bail.\n")
} else {
  cat("\nCONCLUSION: After transformation, no significant difference was found.\n")
}
# ---------------------------------------------------------
# PHASE 4: THE ULTIMATE SOLUTION (SURVIVAL ANALYSIS)
# ---------------------------------------------------------

# 1. Survival package load karein
# (Agar yeh error de ke package nahi hai, toh console mein pehle install.packages("survival") run kar lena)
library(survival)

cat("\n--- PHASE 4: KAPLAN-MEIER SURVIVAL ANALYSIS ---\n")

# 2. Kaplan-Meier Model Fit Karna
# Yahan hum wapas 'filtered_sample' use kar rahay hain (jis mein Pending aur Disposed dono hain)
# Surv() function automatically Censoring (Status) ko handle kar leta hai.
km_fit <- survfit(Surv(Duration_Days, Status) ~ Mapped_Bail, data = filtered_sample)

# 3. Visual Insight: Survival Curve Plot Karna (The "Wow" Factor)
# Yeh graph dikhayega ke waqt ke sath sath cases kaise solve hote hain.
plot(km_fit, 
     col = c("red", "blue"), 
     lwd = 2,
     main = "Kaplan-Meier Survival Curve: Case Duration",
     xlab = "Days in Court",
     ylab = "Probability of Remaining Pending")

# Graph par legend (labels) lagana
legend("topright", 
       legend = unique(filtered_sample$Mapped_Bail), 
       col = c("red", "blue"), 
       lwd = 2)

# 4. The Log-Rank Test (Final Statistical Proof)
# Yeh test T-Test aur Mann-Whitney ka baap hai kyunke yeh censored data ko accept karta hai.
cat("\n--- Log-Rank Test Results ---\n")
log_rank_test <- survdiff(Surv(Duration_Days, Status) ~ Mapped_Bail, data = filtered_sample)
print(log_rank_test)

# 5. Final Conclusion Printout
cat("\n=======================================================\n")
cat("PROJECT CONCLUSION: \n")
cat("By evolving from Non-Parametric to Parametric, and finally to Survival Analysis,\n")
cat("we successfully analyzed court delays without losing a single row of pending data!\n")
cat("=======================================================\n")
# ---------------------------------------------------------
# ---------------------------------------------------------
# THE "ALL FEATURES" CORRELATION HEATMAP (FORCED ENCODING)
# ---------------------------------------------------------

cat("\nGenerating and Saving ALL Features Heatmap...\n")

library(ggcorrplot)
library(dplyr)

# 1. Sab features ko zabardasti numbers mein convert karna
all_features_data <- sample_10k %>%
  # Dates ko exclude kar rahe hain warna error aayega (Days already calculated hain)
  select(-DATE_FILED, -DECISION_DATE, -LAST_SYNC_TIME) %>%
  # Text (Categorical) variables ko pehle Factor aur phir Number (1,2,3...) mein badlo
  mutate_if(is.character, as.factor) %>%
  mutate_all(as.numeric)

# 2. Correlation Calculate Karna
cor_matrix_all <- cor(all_features_data, method = "spearman", use = "pairwise.complete.obs")

# 3. Mega Heatmap Design Karna
all_heatmap <- ggcorrplot(cor_matrix_all, 
           method = "square", 
           type = "lower",     
           lab = FALSE,        # Dabbo mein numbers OFF kar diye hain warna 20x20 ki grid mein kachra ban jayega
           colors = c("#e74c3c", "white", "#3498db"), 
           title = "The 'All Features' Correlation Matrix (Complete Overview)",
           ggtheme = theme_minimal()) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8), # Labels chotay aur tehhray kiye
    axis.text.y = element_text(size = 8)
  )

print(all_heatmap)

# 4. Save karna (Iska size kafi bara rakha hai taake 20+ columns fit aa sakein)
ggsave(paste0(save_folder, "All_Features_Correlation.png"), plot = all_heatmap, width = 16, height = 12, dpi = 300)

cat("\n--- ALL FEATURES HEATMAP SAVED AS 'All_Features_Correlation.png'! ---\n")

# ---------------------------------------------------------
# COMPREHENSIVE (YET CLEAN) CORRELATION MATRIX
# ---------------------------------------------------------

cat("\nGenerating and Saving Comprehensive Correlation Heatmap...\n")

library(ggcorrplot)
library(dplyr)

# 1. DAKSH database ke mutabiq sirf Logical Numeric Features select kiye:
# IDs, alphanumeric strings, aur useless variables ko totally bahar rakha hai.
comprehensive_data <- sample_10k %>%
  select(
    Duration_Days,
    HEARING_COUNT,
    PENDING_DAYS,
    Status
    # Agar tumhare data mein 'YEAR' ya 'DISPOSAL_DAYS' ka proper column mojood hai, 
    # toh line 14 ke aakhir mein comma (,) laga kar unhe bhi yahan likh do.
  )

# 2. Correlation Calculate Karna
cor_matrix_comp <- cor(comprehensive_data, method = "spearman", use = "pairwise.complete.obs")

# 3. Comprehensive Heatmap Design Karna
comp_heatmap <- ggcorrplot(cor_matrix_comp, 
           method = "square", 
           type = "lower",     
           lab = TRUE,         
           lab_size = 4.5,       # Size thora adjust kiya taake sab dabbo mein fit aaye
           colors = c("#e74c3c", "white", "#3498db"), 
           title = "Comprehensive Feature Correlation Matrix (DAKSH Database)",
           ggtheme = theme_minimal()) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # Niche wale labels ko thora tehra (angled) kar diya taake overlap na hon
  )

print(comp_heatmap)

# 4. Automatically save in folder
ggsave(paste0(save_folder, "Comprehensive_Correlation_Matrix.png"), plot = comp_heatmap, width = 9, height = 7, dpi = 300)

cat("\n--- COMPREHENSIVE CORRELATION HEATMAP SAVED AS 'Comprehensive_Correlation_Matrix.png'! ---\n")

# ---------------------------------------------------------
# THE "ALL FEATURES" HEATMAP (WITH NUMBERS INCLUDED)
# ---------------------------------------------------------

cat("\nGenerating and Saving ALL Features Heatmap with Numbers...\n")

library(ggcorrplot)
library(dplyr)

# 1. Sab features ko zabardasti numbers mein convert karna
all_features_data <- sample_10k %>%
  select(-DATE_FILED, -DECISION_DATE, -LAST_SYNC_TIME) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_all(as.numeric)

# 2. Correlation Calculate Karna
cor_matrix_all_nums <- cor(all_features_data, method = "spearman", use = "pairwise.complete.obs")

# 3. Mega Heatmap Design Karna (Numbers ON)
all_heatmap_nums <- ggcorrplot(cor_matrix_all_nums, 
           method = "square", 
           type = "lower",     
           lab = TRUE,           # YEH RAHI TUMHARI DEMAND: Numbers WAPAS ON kar diye!
           lab_size = 2.5,       # Font size chota kiya taake 20x20 grid mein overlap na hon
           colors = c("#e74c3c", "white", "#3498db"), 
           title = "The 'All Features' Correlation Matrix (With Exact Values)",
           ggtheme = theme_minimal()) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  )

print(all_heatmap_nums)

# 4. Save karna (Size intentionally 18x14 rakha hai taake zoom karne par picture phatay nahi)
ggsave(paste0(save_folder, "All_Features_With_Numbers.png"), plot = all_heatmap_nums, width = 18, height = 14, dpi = 300)

cat("\n--- ALL FEATURES HEATMAP WITH NUMBERS SAVED! ---\n")cat("\n--- FOCUSED HEATMAP WITH NUMBERS SAVED! ---\n")