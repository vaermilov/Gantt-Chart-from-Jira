#### Gantt chart for agile project management ------------------

# This code is importing data from Jira and plotting a Gantt chart.
# It is supposed that Scrum framework is used and there are 
# several sprints containing issues.
# The data is exported from Jira using JirAgileR package, API
# token should be generated in the Jira account settings.

## 1. Input initial data ----------------------------------------

q_of_sprints <- 3         # how many sprints do you want to show
dates <- c("2022-05-22",
           "2022-05-29",
           "2022-06-05",
           "2022-06-12")  # set sprint dates

## 2. Installing packages ---------------------------------------

install.packages("JirAgileR")
library(JirAgileR)
install.packages("plan")
library(plan)

## 3. Save Jira credentials -------------------------------------

save_jira_credentials(
  domain = "Your_domain",
  username = "Your_Username",
  password = "Your_API_token",
  verbose = TRUE)

## 4. Import issues from Jira according to JQL queries ----------

issues <- c()
i <- 1
while (i <= q_of_sprints) {
  issues[[i]] <- as.data.frame(
    get_jira_issues(jql_query = 
                      paste0('sprint="Sprint ', i, '"'))) # JQL
  i <- i + 1}

## 5. Creating gantt object and filling in sprints --------------

g <- new("gantt")
i <- 1
while (i <= q_of_sprints) {
  g <- ganttAddTask(g, paste("Sprint", i))
  k <- length(issues[[i]]$key)
  while (k > 0) {
    g <- ganttAddTask(g, 
                      issues[[i]]$key[k], 
                      dates[i], 
                      dates[i + 1], 
                      done = 0)  
    k <- k - 1}
  i <- i +1}

# 6. Setting sprint readiness ----------------------------------

# In this section you can set progress for each item in each
# sprint. Items are numbered one after another (including 
# Sprint section names)

g@data$done[2] <- 10
g@data$done[3] <- 30
g@data$done[4] <- 20

# 7. Plotting Gantt chart --------------------------------------

font <- ifelse(is.na(g[["start"]]), 2, 1)
plot(g, ylabel = list(font = font),
     event.time = "2022-05-23", event.label = "Report Date")
par(lend="square")
legend("topright", pch=22, pt.cex=2, pt.bg=gray(c(0.3, 0.9)),
       border="black", xpd=NA,
       legend=c("Completed", "Not Yet Done"), title="MSc plan", bg="white")