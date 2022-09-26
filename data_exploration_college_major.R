# Loading the data
df <- read_csv('salaries_by_college_major.csv')

# Inspecting the data
head(df)

# How many rows and how many columns does our dataframe have?
nrow(df)
ncol(df)

# What are the labels for the columns?
# Do the columns have names?
colnames(df)

# Are there any missing values in our dataframe?
# Does our dataframe contain any bad data?

# Check if a value is missing
is.na(df)

# Count the number of missing values per column
colSums(is.na(df))

# Identify the position of the columns with at least one missing value
which(colSums(is.na(df))>0)

# Return the column names with missing values
names(which(colSums(is.na(df))>0))

# Droping Missing Values 
clean_df <- drop_na(df)

# Find the highest starting Median salary
max(clean_df$`Starting Median Salary`)

# which college major earns this much on average?
clean_df$`Undergraduate Major`[which(clean_df$`Starting Median Salary` == max(clean_df$`Starting Median Salary`))]

# What college major has the highest mid-career salary?
clean_df$`Undergraduate Major`[which(clean_df$`Mid-Career Median Salary` == max(clean_df$`Mid-Career Median Salary`))]

# How much do graduates with this major earn?
max(clean_df$`Mid-Career Median Salary`)

# Which college major has the lowest starting salary?
clean_df$`Undergraduate Major`[which(clean_df$`Starting Median Salary` == min(clean_df$`Starting Median Salary`))]

# how much do graduates earn after university?
min(clean_df$`Starting Median Salary`)

# Which college major has the lowest mid-career salary?
clean_df$`Undergraduate Major`[which(clean_df$`Mid-Career Median Salary` == min(clean_df$`Mid-Career Median Salary`))]

# how much can people expect to earn with this degree? 
min(clean_df$`Mid-Career Median Salary`)

# Calculate the difference between the earnings of the 10th and 90th percentile
clean_df <- add_column(clean_df, Spread = clean_df$`Mid-Career 90th Percentile Salary` - clean_df$`Mid-Career 10th Percentile Salary`, .after = 1)

# Sorting by the Lowest Spread and only seeing the name of the degree and the major
low_risk <- clean_df[order(clean_df$Spread),][,c('Undergraduate Major', 'Spread')]

# The degrees with the highest potential
clean_df[order(clean_df$`Mid-Career 90th Percentile Salary`, decreasing = TRUE),][,c('Undergraduate Major', 'Mid-Career 90th Percentile Salary')]

# Find the degrees with the greatest Spread in salaries
clean_df[order(clean_df$Spread, decreasing = TRUE),][,c('Undergraduate Major', 'Spread')]

# How many majors we have in each category
clean_df %>% 
  count(Group)

# The average salary by group
clean_df %>% 
  group_by(Group) %>% 
  summarise(across(where(is.numeric), mean)) 
