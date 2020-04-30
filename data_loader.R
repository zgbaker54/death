
load_data = function(){

  library(splitstackshape)
  library(ggplot2)
  
  white = read.delim('load_data/Underlying Cause of Death, 1999-2018_white.txt',
                     header=TRUE)
  black = read.delim('load_data/Underlying Cause of Death, 1999-2018_black.txt',
                     header=TRUE)
  asian = read.delim('load_data/Underlying Cause of Death, 1999-2018_asian.txt',
                     header=TRUE)
  native = read.delim('load_data/Underlying Cause of Death, 1999-2018_native.txt',
                      header=TRUE)
  
  death_table = rbind(white, black, asian, native)
  
  death_table$DeathsBy1000 = round(death_table$Deaths/100)
  
  death_table2 = expandRows(death_table, 'DeathsBy1000')
  
  
  death_table3 = data.frame(death_table2$State, death_table2$Single.Year.Ages.Code, death_table2$Gender, death_table2$Race)
  
  names(death_table3)[names(death_table3) == "death_table2.State"] <- "state"
  names(death_table3)[names(death_table3) == "death_table2.Single.Year.Ages.Code"] <- "age"
  names(death_table3)[names(death_table3) == "death_table2.Gender"] <- "gender"
  names(death_table3)[names(death_table3) == "death_table2.Race"] <- "race"
  
  
  model <- lm(age ~ gender + state + race,
              data = death_table3)
  
  result = list(model, death_table)
  return(result)

}

get_prediction_and_plot = function(model, death_table, submitted_data){
  
  pred = predict.lm(model, submitted_data)
  
  prediction = pred[[1]]
  
  submitted_gender = as.character(submitted_data$gender)
  submitted_state = as.character(submitted_data$state)
  submitted_race = as.character(submitted_data$race)
  
  p = ggplot(data=subset(death_table,
                         Gender == submitted_gender &
                         State == submitted_state &
                         Race == submitted_race),
             aes(x=Single.Year.Ages.Code, y=Deaths)) +
    geom_point() +
    labs(
      x = 'Age of Death',
      y = 'Number of Deaths'
    )
  
  result = list(prediction, p)
  return(result)

}

