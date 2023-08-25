## Tech Layoffs 2022-2023.....................
## Keya Mondal 
options(repos = c(CRAN = "http://cran.rstudio.com")) # repo-path setup
# install packages..............................
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
install.packages("wordcloud")
library("wordcloud")
install.packages("tm")
library("tm")
# Import dataset
# First we can assign a specific variable name to the dataset that we are going to export:
Total_tech_layoffs <- read.csv("C:/Users/91731/Documents/technology_layoff/tech_layoffs.csv")

# Display the first six rows of loaded dataset
head(Total_tech_layoffs)

# Display the column names of dataset
colnames(Total_tech_layoffs)

# Display the structure of dataset
str(Total_tech_layoffs)

# Replace 'unclear' with 0 in the 'total_layoffs' column
Total_tech_layoffs$total_layoffs[Total_tech_layoffs$total_layoffs == "Unclear"] <- 0

# Display the first Six rows of dataset
head(Total_tech_layoffs)
# wordcloud on Industry

wordcloud(words = Total_tech_layoffs$industry,min.freq = 1,random.order=FALSE, rot.per=0.5,
          colors=brewer.pal(4, "Dark2"))

# Replace 'Unclear' with 0 in the 'total_layoffs' column
clean_data <- Total_tech_layoffs %>%
  mutate(total_layoffs = ifelse(total_layoffs == "Unclear", 0, total_layoffs)) %>%
  distinct(company, .keep_all = TRUE)

# View the cleaned data
head(clean_data)
# Display the first ten rows of dataset
tibble(Total_tech_layoffs)


# Which companies have the highest numbers of layoffs.?
  
  # Replace "Unclear" with 0 in the "total_layoffs" 
# group the data by company and calculate the total layoffs per company
layoffs_by_company <- Total_tech_layoffs %>%
  group_by(company) %>%
  summarize(total_layoffs = sum(as.numeric(as.character(total_layoffs)), na.rm = TRUE))

# sort the data in descending order based on total layoffs
layoffs_by_company <- layoffs_by_company[order(-layoffs_by_company$total_layoffs),]
# # subset by condition 5000 layoff
df0<-subset(layoffs_by_company, total_layoffs >=5000)

# Barplot
ggplot(df0, aes(x=company, y=total_layoffs)) + 
  geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7))

# print the top 10 companies with the highest number of layoffs
top_10_layoffs <- head(layoffs_by_company, 15)
print(top_10_layoffs)
# calculate the total number of layoffs across all companies
total_layoffs <- sum(layoffs_by_company$total_layoffs, na.rm = TRUE)
# print the total number of layoffs
cat("Total number of layoffs: ", total_layoffs, "\n")
#What is the Average impacted workforce percentage across all companies.?
# Calculate the average impacted workforce percentage across all companies
avg_impacted_percentage <- mean(as.numeric(Total_tech_layoffs$impacted_workforce_percentage), na.rm = TRUE)
colnames(Total_tech_layoffs)
str(Total_tech_layoffs)
# Print the result
cat("The average impacted workforce percentage across all companies is:", round(avg_impacted_percentage, 2), "%")
# which industry is most effect

# Group the data by industry and calculate the total number of layoffs for each industry
industry_layoffs <- Total_tech_layoffs %>%
  # Check data type of total_layoffs and convert to numeric if necessary
  mutate(total_layoffs = as.numeric(total_layoffs)) %>%
  group_by(industry) %>%
  summarise(total_layoffs = sum(total_layoffs))
# Sort the data by total layoffs in descending order
industry_layoffs <- industry_layoffs %>%
  arrange(desc(total_layoffs))

df2<-subset(industry_layoffs, total_layoffs >=3500)
# pie plot number of layoffs for each industry

pie(df2$total_layoffs,labels = df2$industry,col = rainbow(length(df2$total_layoffs)))
legend("topright",df2$industry,cex = 0.45,fill = rainbow(length(df2$total_layoffs)))


# View the top 10 industries with the most layoffs
top_industries <- head(industry_layoffs, 10)
top_industries



-----------------------------------------------------------------------

# group the data by state and calculate the total number of layoffs
state_layoffs <- Total_tech_layoffs %>%
  group_by(headquarter_location) %>%
  summarize(total_layoffs = sum(total_layoffs, na.rm = TRUE)) %>%
  na.omit()
# select the top 10 states with the highest number of layoffs
top_10_states <- state_layoffs %>%
  slice_max(total_layoffs, n = 10)
#####################
# convert the total_layoffs column to numeric
total_tech_layoffs$total_layoffs <- as.numeric(total_tech_layoffs$total_layoffs)

# abbreviate the headquarter_location names
Total_tech_layoffs$headquarter_location <- gsub("New York", "NY", Total_tech_layoffs$headquarter_location)
Total_tech_layoffs$headquarter_location <- gsub("San Francisco", "SF", Total_tech_layoffs$headquarter_location)
Total_tech_layoffs$headquarter_location <- gsub("San Jose", "SJ", Total_tech_layoffs$headquarter_location)
Total_tech_layoffs$headquarter_location <- gsub("Los Angeles", "LA", Total_tech_layoffs$headquarter_location)
Total_tech_layoffs$headquarter_location <- gsub("Mountain View", "MV", Total_tech_layoffs$headquarter_location)
Total_tech_layoffs$headquarter_location <- gsub("Chicago", "Ch", Total_tech_layoffs$headquarter_location)
Total_tech_layoffs$headquarter_location <- gsub("Seattle", "SE", Total_tech_layoffs$headquarter_location)
Total_tech_layoffs$headquarter_location <- gsub("Menolo Park", "MP", Total_tech_layoffs$headquarter_location)

# create a bar chart showing the top 10 states with the highest number of layoffs
install.packages("ggplot2")
library("ggplot2")
ggplot(top_10_states, aes(x = headquarter_location, y = Total_tech_layoffs$total_layoffs)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 States with the Highest Number of Layoffs in the Tech Industry",
       x = "State",
       y = "Total Number of Layoffs",caption = "Muhammad Sheheryar")
  
# select the top 10 states with the highest number of layoffs