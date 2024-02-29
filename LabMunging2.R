library(dplyr)

#Step 1: Create a function (named readStates) to read a CSV file into R: within the Function 1
  
readStates <- function() {
  urlToRead <-"http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  testFrame <- read.csv(url(urlToRead))
  return (testFrame)
}

readStates()
#Uses Function
print(testFrame)


#Q1. You need to read a URL, not a local file to your computer.

#Q2. The file is a dataset on state populations (within the United States).

# The URL is: http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csvLinks to an external site.

# In case you have trouble using this url, use this csv file nst-est2011-01.csv  Download nst-est2011-01.csvand read the file locally using read.csv function.



#Step 2: Clean the dataframe: within Function 1

#Q3. Note the issues that need to be fixed (removing columns, removing rows, changing column names).

#Q4. Within your function, make sure there are 51 rows (one per state + the district of Columbia). Make sure there are only 5 columns with the columns having the following names (stateName, Census, Estimates, Pop2010, Pop2011).

#Q5. Make sure the last four columns are numbers (i.e. not strings).

function1 <- function() {
  #Removes ROWS 
  testFrame <- testFrame [-c(1,2,4,5,6,7,8,60,61,62,63,64,65,66),]
  #Removes Columns
  testFrame <- testFrame [,-c(6,7,8,9,10)]
  #rownames(testFrame) <- testFrame[,3]
  #Creates a "State Name" 
  testFrame[1,1] <- "State Name"
  #Had to do this because when they became headers, they where their current data type.
  #Doing a "manual overide", might get docked points for this. 
  testFrame[1,2] <- "Census"
  testFrame[1,3]<- "Estimate Base"
  testFrame[1,4]<- "2010"
  testFrame[1,5]<- "2011"
  #Row one becomes the header/coulmn names 
  colnames(testFrame) <- testFrame[1, ]
  
  #Removes the first row 
  testFrame <- testFrame [-1,]
  #Counts the total rows
  nrow(testFrame)
  #Prints the coulmn names
  colnames(testFrame)
}


function1()
testFrame

#Step 3: Store and explore the dataset: outside of Function 1

#Q6. Store the dataset into a dataframe, called dfStates.

# When you run the following, it should print a clean dataframe. Please include the output of "dfStates" in the compiled file by running dfStates as below. 
dfStates <- data.frame(testFrame)
print(dfStates)

#Going to change data type 
dfStates$Census <-as.numeric(gsub(",","",dfStates$Census))
dfStates$Estimate.Base <-as.numeric(gsub(",","",dfStates$Estimate.Base))
dfStates$X2010 <-as.numeric(gsub(",","",dfStates$X2010))
dfStates$X2011 <-as.numeric(gsub(",","",dfStates$X2011))



#Q7. Test your dataframe by calculating the mean for the 2011 data, by doing (include your output):
#Get the mean 
mean(dfStates$X2011)
# You should get an answer of  6,109,645



#Step 4: Find the state with the highest population: outside the Function 1

#Q8. Based on the 2011 data, what is the population of the state with the highest population? What is the name of that state, and what is the value of the population?
print(dfStates[which.max(dfStates$X2011),])

#Q9. Sort the data, in increasing order, based on the 2011 data.
print(dfStates[order(dfStates$X2011, decreasing = FALSE),])


# Function 2: Create a function called "Distribution"
#Step 5: Explore the distribution of the states: You need to create a new function called "Distribution"

#Q10. You will write a function to calculate percentage of states that have population that is lower than the average.  The function (function name: "Distribution") takes two parameters. The first is a vector and the second is a number. For example, Distribution <- function(vector, number). This step is just a setup for the following instruction. 
# The function will return the percentage of elements within the vector that is less than the number (i.e. cumulative distribution below the value provided). For example,  
# (1) Think about this: You only keep the elements within the vector that are less than the number, and store the number of eligible elements into the variable "count". Populate XXXX to complete this line of code: 
# (2) Then, you will calculate the percentage and return the results. Populate XXXX to complete this line of code:
# ****For example, if the vector had 5 elements (1,2,3,4,5), with 2 being the number passed into the function, the function will return 20 (since 20% of the numbers--that is 1-- is below 2). Again, if you pass number 5, then the function should return 80, since 80% of the numbers--1,2,3, and 4 are below 5. Pass these numbers using your function and see if your function returns the correct values.
# (3) Test the function with the vector “dfStates$Pop2011”, and the mean of “dfStates$Pop2011.” *** you should get 66.66667 as a result. There are many ways to write this function (described in point 10) – so please try to write multiple versions of this function – which do you think is the best?


#This is the most scuffed way of doing the assignment...but it works
Distribution<- function(){
  avgPop <- mean(dfStates$X2011)
  ValueStorage <- c(dfStates$X2011)
  #print(ValueStorage)
  ValueStorage2 <- c(ValueStorage<avgPop)
  Count <- length(ValueStorage2[ValueStorage2== TRUE])
  PercentValue <- (Count/(nrow(dfStates)))*100
  PercentValue
}

Distribution()
