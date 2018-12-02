library(rlang)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dplyr)
library(MASS)
library(readxl)

se <- read.csv("~/Documents/DePaul/Fall2018/DSC424/HW4/2016SchoolExplorer.csv", header = TRUE)
se = se[4:138]
#Convert 'N/A' strings to NA
se[ se == "N/A" ] = NA

#Remove dollar signs
se$School.Income.Estimate = as.numeric(gsub('[$,]', '', se$School.Income.Estimate))

#Removing Percent symbols
percent_cols =dplyr::select(se,Percent.Asian,Percent.Black,Percent.Black...Hispanic,Percent.ELL,Percent.Hispanic,Percent.of.Students.Chronically.Absent,Percent.White,Supportive.Environment..,Rigorous.Instruction..,Collaborative.Teachers..,Effective.School.Leadership..,Trust..,Strong.Family.Community.Ties..,Student.Attendance.Rate)
percent_cols = apply(percent_cols, 2, function(y) as.numeric(gsub("%", "", y)))

se$Percent.Asian = percent_cols[, 1]
se$Percent.Black = percent_cols[, 2]
se$Percent.Black...Hispanic = percent_cols[, 3]
se$Percent.ELL = percent_cols[, 4]
se$Percent.Hispanic = percent_cols[, 5]
se$Percent.of.Students.Chronically.Absent = percent_cols[, 6]
se$Percent.White = percent_cols[, 7]
se$Supportive.Environment.. = percent_cols[, 8]
se$Rigorous.Instruction.. = percent_cols[, 9]
se$Collaborative.Teachers.. = percent_cols[, 10]
se$Effective.School.Leadership.. = percent_cols[, 11]
se$Trust.. = percent_cols[, 12]
se$Strong.Family.Community.Ties.. = percent_cols[, 13]
se$Student.Attendance.Rate = percent_cols[, 14]


# after doing a colSums(is.na(se)), I found that there were over 100 rows with missing values across the following 4 columns, so I removed them.
se = se[!is.na(se$Rigorous.Instruction.Rating) ,]
se = se[!is.na(se$Student.Achievement.Rating) ,]
se = se[!is.na(se$Trust.Rating) ,]
se = se[!is.na(se$Supportive.Environment.Rating) ,]





# Here I handle all na values in School income by assigning them the average income of their corresponding district.
Districts = se$District
means = aggregate(.~Districts, data=se, mean)
income_means = means$School.Income.Estimate
income = se$School.Income.Estimate
d = se$District

for (x in 1:length(income))
  if (is.na(income[x]))
    income[x] = income_means[d[x]]
se$School.Income.Estimate = income


#converting decimal data from <fctr> to <dbl>
se$Economic.Need.Index = as.numeric(as.character(se$Economic.Need.Index))
se$Average.ELA.Proficiency = as.numeric(as.character(se$Average.ELA.Proficiency))
se$Average.Math.Proficiency = as.numeric(as.character(se$Average.Math.Proficiency))

#Creating dummy variables for categorical data
se$Community.School.= as.numeric(se$Community.School.)
se$Rigorous.Instruction.Rating = as.numeric(se$Rigorous.Instruction.Rating)
se$Collaborative.Teachers.Rating = as.numeric(se$Collaborative.Teachers.Rating)
se$Supportive.Environment.Rating = as.numeric(se$Supportive.Environment.Rating)
se$Effective.School.Leadership.Rating = as.numeric(se$Effective.School.Leadership.Rating)
se$Strong.Family.Community.Ties.Rating = as.numeric(se$Strong.Family.Community.Ties.Rating)
se$Trust.Rating = as.numeric(se$Trust.Rating)
se$Student.Achievement.Rating = as.numeric(se$Student.Achievement.Rating)

#now all of the missing values have been handled, thanks mostly to Will for working all this out and making the data workable!!!

rownames(se) <- make.names(se[,1], unique = TRUE)
se[,1] <- NULL
str(se)

#data exploration
pairs(~School.Income.Estimate+ Economic.Need.Index + Grade.3.ELA...All.Students.Tested + Grade.3.Math...All.Students.tested + Grade.5.ELA...All.Students.Tested + Grade.5.Math...All.Students.Tested,data=se, 
      main="Scatterplot Matrix")

pairs(~School.Income.Estimate+ Economic.Need.Index + Districts + Average.ELA.Proficiency+Average.Math.Proficiency ,data=se, 
      main="Scatterplot Matrix")


#Choose the data variables we want to use for the clustering
Schools = se[c(14,36,37)] 
str(Schools)

# Let's first scale the data by dividing each variable by its standard deviation:
std <- sapply(Schools, sd, na.rm = TRUE) # finding standard deviations of variables
head(std)

Schools.std <- sweep(Schools, 2, std, '/')
Schools.std[is.na(Schools.std)]<- 0
head(Schools.std)
str(Schools.std)

######################
#    how many clusters?    #
######################
fviz_nbclust(Schools, kmeans,
             method = "gap_stat")

fviz_nbclust(Schools, kmeans, method = "wss")

fviz_nbclust(Schools, kmeans, method = "silhouette")

# A K-means clustering with k = 2:
Schools.k2 <- kmeans(Schools.std, centers=2, iter.max=100, nstart=25)
Schools.k2

# Visualize
fviz_cluster(Schools.k2, data = Schools, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             geom = "point",
             ggtheme = theme_minimal())

# A K-means clustering with k = 3:
Schools.k3 <- kmeans(Schools.std, centers=3, iter.max=100, nstart=25)
Schools.k3

# Visualize
fviz_cluster(Schools.k3, data = Schools, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             geom = "point",
             ggtheme = theme_minimal())

# A K-means clustering with k = 4:
Schools.k4 <- kmeans(Schools.std, centers=4, iter.max=100, nstart=25)
Schools.k4

# Visualize
fviz_cluster(Schools.k4, data = Schools, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             geom = "point",
             ggtheme = theme_minimal())

# A K-means clustering with k = 5:
Schools.k5 <- kmeans(Schools.std, centers=5, iter.max=100, nstart=25)
Schools.k5

# Visualize
fviz_cluster(Schools.k5, data = Schools, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             geom = "point",
             ggtheme = theme_minimal())

Schools.k5$betweenss/Schools.k5$totss
Schools.k4$betweenss/Schools.k4$totss
Schools.k3$betweenss/Schools.k3$totss
Schools.k2$betweenss/Schools.k2$totss

#choose k = 2
Schools %>%
  as_tibble() %>%
  mutate(cluster = Schools.k2$cluster,
         state = row.names(Schools)) %>%
  ggplot(aes(Average.ELA.Proficiency, Average.Math.Proficiency, color = factor(cluster), label = state)) +
  geom_text() + ggtitle("Schools in two clusters") +
  xlab("Average ELA Scores") + ylab("Average Math Scores") + labs(color = "Cluster")

Schools %>%
  as_tibble() %>%
  mutate(cluster = Schools.k2$cluster,
         state = row.names(Schools)) %>%
  ggplot(aes(School.Income.Estimate, Average.Math.Proficiency, color = factor(cluster), label = state)) +
  geom_text()+ ggtitle("Schools in two clusters") +
  xlab("School Income Estimate") + ylab("Average Math Scores") + labs(color = "Cluster")


Schools %>%
  as_tibble() %>%
  mutate(cluster = Schools.k2$cluster,
         state = row.names(Schools)) %>%
  ggplot(aes(School.Income.Estimate, Average.ELA.Proficiency, color = factor(cluster), label = state)) +
  geom_text()+ ggtitle("Schools in two clusters") +
  xlab("School Income Estimate") + ylab("Average ELA Scores") + labs(color = "Cluster")

################################
# exploring the two clusters separately #
################################
# add cluster number to dataset
groups = Schools.k2$cluster
se$group <- groups

#explore based on cluster
plot( se$District, se$group,main="Scatterplot cluster districts", 
      ylab="Cluster", xlab="School District")
pairs(~se$group + se$Percent.Asian + se$Percent.Black + se$Percent.Black...Hispanic + se$Percent.ELL + se$Percent.Hispanic + se$Percent.White)
pairs(~se$group + se$Student.Attendance.Rate + se$Trust.. + se$Strong.Family.Community.Ties.. + se$Effective.School.Leadership.. + se$Collaborative.Teachers.. + se$Rigorous.Instruction..)
pairs(~se$Trust.. + se$Strong.Family.Community.Ties.. + se$Rigorous.Instruction.. + se$Percent.ELL)

i = 1
BinomialGroup =vector (,1167)
for (c in se$group){
  if (se$group[i] == 2){
    BinomialGroup[i] = 0
    i = i + 1
  } else {
    BinomialGroup[i] = 1
    i = i + 1
  }
}

BinomialGroup

mylogit <- glm(BinomialGroup ~ Percent.Asian + Percent.ELL + Percent.Hispanic + Percent.White + Student.Attendance.Rate + Strong.Family.Community.Ties..  + Rigorous.Instruction.. + Percent.ELL, data = se, family = "binomial")
summary(mylogit)

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

######################
#      separate clusters     #
######################
cluster1 <- se[ which(se$group=='1'), ]
cluster2 <- se[ which(se$group=='2'), ]

newC1 <- cluster1[c(31,23,15,16,18,20,21)]
head(newC1)

newC2 <- cluster2[c(31,23,15,16,18,20,21)]
head(newC2)

# Let's first scale the data by dividing each variable by its standard deviation:
std1 <- sapply(newC1, sd, na.rm = TRUE) # finding standard deviations of variables
head(std1)

std2 <- sapply(newC2, sd, na.rm = TRUE) # finding standard deviations of variables
head(std2)

######################
#  choose clusters for C1 #
######################
C1.std <- sweep(newC1, 2, std1, '/')
C1.std[is.na(C1.std)]<- 0
head(C1.std)
str(C1.std)

# how many clusters?
my.data.matrix <- C1.std 
my.k.choices <- 2:8
n <- length(my.data.matrix[,1])
wss1 <- (n-1)*sum(apply(my.data.matrix,2,var))
wss <- numeric(0)
for(i in my.k.choices) {
  W <- sum(kmeans(my.data.matrix,i)$withinss)
  wss <- c(wss,W)
}
wss <- c(wss1,wss)
plot(c(1,my.k.choices),wss,type='l',xlab='Number of clusters', ylab='Within-groups sum-of-squares', lwd=2)

### A little function to calculate the average silhouette width
### for a variety of choices of k:

my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(C1.std, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )

# A LARGE average silhouette width indicates that the observations are properly clustered.



######################
#  choose clusters for C2 #
######################
#cluster 2
C2.std <- sweep(newC2, 2, std2, '/')
C2.std[is.na(C2.std)]<- 0
head(C2.std)
str(C2.std)

# how many clusters?
my.data.matrix <- C2.std  
my.k.choices <- 2:8
n <- length(my.data.matrix[,1])
wss1 <- (n-1)*sum(apply(my.data.matrix,2,var))
wss <- numeric(0)
for(i in my.k.choices) {
  W <- sum(kmeans(my.data.matrix,i)$withinss)
  wss <- c(wss,W)
}
wss <- c(wss1,wss)
plot(c(1,my.k.choices),wss,type='l',xlab='Number of clusters', ylab='Within-groups sum-of-squares', lwd=2)


### A little function to calculate the average silhouette width
### for a variety of choices of k:

my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(C2.std, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )

# A LARGE average silhouette width indicates that the observations are properly clustered.

######################
#             C2 final               #
######################
C2.kmed.3 <- pam(C2.std, k=3, diss=F)
C2.kmed.3

C2.kmed.3$clustering  # printing the "clustering vector"

C2.kmed.3$silinfo$avg.width  #printing the average silhouette width

C2.3.clust <- lapply(1:3, function(nc) row.names(se)[C1.kmed.3$clustering==nc])  
C2.3.clust   # printing the clusters in terms of the car names

############# Visualization of Clusters:

## Built-in plots available with the pam function:

# The "clusplot":

plot(C2.kmed.3, which.plots=1, main =  "High Scoring Cluster Plot (PAM) where k = 3")

# The "silhouette plot":

plot(C2.kmed.3, which.plots=2)

# This shows which observations are "best clustered."

plot(newC2, col=C2.kmed.3$cluster, cex=.2, cex.labels=.8, gap = .2, labels = colnames(newC2) , main = "Relationship between 3 clusters with high test scores")

######################
#             C1 final               #
######################
#now do the low values cluster
C1.kmed.5 <- pam(C1.std, k=5, diss=F)
C1.kmed.5

C1.kmed.5$clustering  # printing the "clustering vector"

C1.kmed.5$silinfo$avg.width  #printing the average silhouette width


C1.5.clust <- lapply(1:3, function(nc) row.names(se)[C2.kmed.5$clustering==nc])  
C1.5.clust   # printing the clusters in terms of the car names

############# Visualization of Clusters:

## Built-in plots available with the pam function:

# The "clusplot":

plot(C1.kmed.5, which.plots=1, main = "")
title(main =  "Low Scoring Cluster Plot (PAM) where k = 5")
# The "silhouette plot":

plot(C1.kmed.5, which.plots=2)

# This shows which observations are "best clustered."

plot(newC1, col=C1.kmed.5$cluster, cex=.2, cex.labels=.5, gap = .2, labels = colnames(newC1), main = "Relationship between 5 clusters with low test scores")

######################
#             end vis              #
######################
# Visualize
fviz_cluster(C1.kmed.5, data = newC1, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             geom = "point",
             ggtheme = theme_minimal(),
             title = "Low Scoring Cluster Plot (PAM) where k = 5")

fviz_cluster(C2.kmed.3, data = newC2, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             geom = "point",
             ggtheme = theme_minimal(),
             title = "High Scoring Cluster Plot (PAM) where k = 3")
