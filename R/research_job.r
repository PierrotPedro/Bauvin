# General
library(tidyverse)
# Webscraping 
library(rvest)
library(RSelenium)
library(seleniumPipes)
# Geo data
library(tidygeocoder)
library(leaflet)
library(rnaturalearth)
library(sf)
# NLP
library(udpipe)
library(textrank)
library(wordcloud)
# Cleaning
library(stringr)
# Additional functions presented at the end of the post 
source('R/research_job_fct.R') 

# ----- Test, 1 page -----

url = "https://fr.indeed.com/jobs?q=data%20scientist&l=France&from=searchOnHP"
# Headless Firefox browser
exCap <- list("moz:firefoxOptions" = list(args = list('--headless')))
rD <- rsDriver(browser = "firefox", extraCapabilities = exCap, port=1111L,
               verbose = TRUE)
remDr <- rD$client
# https://www.reddit.com/r/Rlanguage/comments/cjotjs/rselenium_error_in_checkerrorres_undefined_error/
remDr$open() 
# Navigate to the url
remDr$navigate(url)
# Store page source 
web_page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()

web_page %>%
    html_element(css = "div.jobsearch-JobCountAndSortPane-jobCount") %>% # selecting with css 
    html_text2() %>% # Transform to text
    str_remove_all("[^0-9.-]") %>% # Clean the data to only get numbers
    substr(start = 2, stop = 8) %>% 
    as.numeric()


# ------ All pages ------

# Creating URL link corresponding to the first 40 pages
base_url = "https://fr.indeed.com/jobs?q=data%20scientist&l=France&start="
url_list <- c(url, paste0(base_url, as.character(seq(from=10, to=400, by=10))))


# Looping through the URL list
res <- list()
for(i in 1:length(url_list)){
    # Navigate to the URL
    remDr$navigate(url_list[i])
    
    # Store page source 
    web_page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
    
    # Job title 
    job_title <- web_page %>%
        html_elements(css = ".mosaic-provider-jobcards .result") %>%
        html_elements(css = ".resultContent") %>%
        html_element("h2") %>%
        html_text2() %>%
        str_replace(".css.*;\\}", "")
    
    # URL for job post 
    job_url <- web_page %>%
        html_elements(css = ".mosaic-provider-jobcards .result")%>%
        html_elements(css = ".resultContent") %>%
        html_element("h2") %>%
        html_element("a") %>%
        html_attr('href') %>%
        lapply(function(x){paste0("https://fr.indeed.com", x)}) %>%
        unlist()
    
    # Data about company
    company_info <- web_page %>%
        html_elements(css = ".mosaic-provider-jobcards .result")%>%
        html_elements(css = ".resultContent")%>%
        html_element(css = ".company_location")%>%
        html_text2() %>%
        lapply(FUN = tidy_comploc) %>% # Function to clean the textual data
        do.call(rbind, .)
    
    # Data about job description
    job_desc <- web_page %>%
        html_elements(css = ".mosaic-provider-jobcards .result")%>%
        html_element(css =".slider_container .jobCardShelfContainer")%>%
        html_text2() %>%
        tidy_job_desc() # Function to clean the textual data related to job desc.
    
    # Data about salary (when indicated)
    salary_hour <- web_page %>%
        html_elements(css = ".mosaic-provider-jobcards .result .resultContent")%>%
        html_element(css = ".salaryOnly") %>%
        html_text2() %>%
        lapply(FUN = tidy_salary) %>% # Function to clean the data related to salary
        do.call(rbind, .)
    
    # Job posts in the same format
    final_df <- cbind(job_title, company_info, salary_hour, job_desc, job_url)
    colnames(final_df) <- c("Job_title", "Company", "Location", "Rating", "Low_salary", "High_salary", "Contract_info", "Job_desc", "url")
    res[[i]] <- final_df
    
    # Sleep 5 seconds, good practice for web scraping
    Sys.sleep(5)
}

# Gather all the job post in a tibble
final_df <- as_tibble(do.call("rbind", res))

# Final data cleaning
final_df <- final_df %>%
    mutate_at(c("Rating", "Low_salary", "High_salary"), as.numeric)

# Clean job title
final_df$Job_title_c <- clean_job_title(final_df$Job_title)  
final_df$Job_title_c <- as.factor(final_df$Job_title_c)

# Mapping locations 
final_df <- tidy_location(final_df)
final_df <- final_df %>%
    mutate(Loc_tidy_fr = paste(Loc_tidy, 'France')) %>%
    geocode(Loc_tidy_fr, method = 'arcgis', lat = latitude , long = longitude) %>%
    select(- Loc_tidy_fr)

# Downloading and cleaning each job description
job_descriptions <- list()
pb <- txtProgressBar(min = 1, max = length(final_df$url), style = 3)
for(i in 1:length(final_df$url)){
    remDr$navigate(final_df$url[i])
    web_page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
    job_descriptions[[i]] <- web_page %>%
        html_elements(css = ".jobsearch-JobComponent-description") %>%
        html_text2()
    Sys.sleep(2)
    setTxtProgressBar(pb, i)
}
# Gathering in dataframe
job_descriptions <- as.data.frame(do.call("rbind", job_descriptions))
names(job_descriptions) <- c("Description")
# Binding to same table:
final_df <- cbind(final_df, job_descriptions)
# Homogenize with custom function
final_df$Description_c <- lapply(final_df$Description, function(x){clean_job_desc(x)[[2]]})
final_df$Language <- textcat::textcat(final_df$Description)


vector_of_skills <- c("Python", "Jupyter Notebook", " R,", " SAS",
                      "Tensorflow", "Pytorch", 
                      "AWS", "GCP", "Azure", "BigQuery",
                      "SQL", "NoSQL", "Oracle", "Hadoop",
                      "Power BI", "Tableau", "D3js",
                      " Git", "Kaggle", "Java", "Excel")
list_of_skills <- as.list(vector_of_skills) ; names(list_of_skills) <- vector_of_skills
list_of_skills[["Python"]] <- c(list_of_skills[["Python"]], "numpy", "pandas", "sklearn", "scipy", "Scikit")
list_of_skills[["Tensorflow"]] <- c(list_of_skills[["Tensorflow"]], "Keras")
list_of_skills[["AWS"]] <- c(list_of_skills[["AWS"]], "Amazon Web", "Redshift", "EC2")
list_of_skills[["GCP"]] <- c(list_of_skills[["GCP"]], "Google Cloud")
list_of_skills[["Azure"]] <- c(list_of_skills[["Azure"]], "DocumentDB")
list_of_skills[[" R,"]] <- c(list_of_skills[[" R,"]], " R\\.", " R ", " R;", " R ;")
list_of_skills[["Hadoop"]] <- c(list_of_skills[["Hadoop"]], "MapReduce", " Hive", "Spark")
list_of_skills[["D3js"]] <- c(list_of_skills[["D3js"]], "D3\\.js")
for (i in vector_of_skills) {
    final_df[,i] <- NA
    final_df[,i] <- grepl(paste(list_of_skills[[i]], collapse = "|"), 
                          final_df$Description, 
                          ignore.case = TRUE)
}

