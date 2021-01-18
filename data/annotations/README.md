# Results question 1

.rds file is a data.frame with 3 columns and 8 rows. Freq column is a frequency of how many times given feature occurred in annotations. Freq_scales is the same frequency divided by maximum frequency. 

# Results question 2

.rds file is a 9 column data.frame. The 9th column indicates the id of the given observation. Each of the first 8 columns is associated with one variable and shows the order that was passed by the user as an answer to the second question.

# Results question 3

.rds file is a 9 column data.frame. 9th column indicates the id of the given observation. Each of the first 8 columns is associated with one variable and shows the value that was passed by the user as a response to the third question. Following encoding was applied
  
  * -2 - value highly decreases response
  * -1 - value slightly decreases response
  * 0 - value is neutral for response
  * 1 - value slightly increases response
  * 2 - value highly increases response
