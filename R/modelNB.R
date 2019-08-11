#' modelNB Function
#'
#' This function allows you to check if a news article is authentic or fake.
#' It was trained using the Naive Bayes Algorithms from the package e1071, and the the Kaggle Fake News Dataset
#' @param fileName 
#' @export
#' @examples
#' modelNB(FileName)
#' @import RTextTools
#' @import tm
#' @import text2vec
#' @import tools
#' @import SnowballC
#' @importFrom readr read_file


# library(RTextTools)
# library(tm)
# library(text2vec)
# library(tools)
# library(SnowballC)
# library(readr)
# library(e1071)
# 
# 
# path_models = '/home/adelo/1-system/desktop/it_cct/5-Applied_Technology_Group_Project/gofaaaz-machine_learning/r_prog/1codes/0models/0finalModels'
# setwd(path_models)
# 
# model_NB     = readRDS('kfn1-kfn2-dataset3_100_weighting_FALSE_removeSparseTerms_0_90_NB_model.rds')
# dtm_train_NB = readRDS('kfn1-kfn2-dataset3_100_weighting_FALSE_removeSparseTerms_0_90_dtm_train.rds')


modelNB <- function(fileName){
  
  
  convert_count <- function(x) {
    y <- ifelse(x > 0, 1, 0)
    y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
    y
  }
  
  if(class(fileName) == "character" && length(fileName) == 1) {
    if(file.exists(fileName)){
      if(file_ext(fileName) == "txt"){
        data  <- data.frame(read_file(fileName))
        names(data)[1] <- "text"
      }else if(file_ext(fileName) == "csv"){
        data  <- data.frame(readLines(fileName))
        names(data)[1] <- "text"
      }else{
        print('The extension of the file entered is not supported. Make sure the file you are trying to read have a supported extension (.txt or .csv)')
      }
    } else{
      print("Warning!! The parameter you have entered it is NOT a valid path for a text file. The function is analysing the text contained in the «character» object you have entered as parameter.")
      data <- data.frame(fileName)
      names(data)[1] <- "text"
    }
  }else{
    print("Warning!! The parameter you have entered it is NOT a valid path for a file. The function is analysing the text contained in the object you have entered as parameter.")
    data <- data.frame(fileName)
    names(data)[1] <- "text"
  }
  
  
  #  Text processing ----
  text <- data$text
  
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en"))    # stopwords
  text <- stripWhitespace(text)                 # Remove blank space
  text <- SnowballC::wordStem(text, language = "english")  # Stemming Words
  
  
  onlyOne = FALSE
  if (length(text) == 1){
    onlyOne = TRUE
    text = c(text,"La funcion classify_models from RTextTools no funciona cuando length es 1, estamos entonces agregando un segundo valor")
  }
  
  
  dtm    <- create_matrix(text,
                          language          = "english",
                          # weighting         = function(x) weightTfIdf(x, normalize = F),
                          toLower           = TRUE,   # Converts to lowecase
                          removeNumbers     = TRUE,   # Removes numbers
                          removeStopwords   = TRUE,   # Removes stop words
                          removePunctuation = TRUE,   # Removes punctuation
                          stripWhitespace   = TRUE,   # Remove blank space
                          removeSparseTerms = 0.90,   # Remove all terms in the corpus whose sparsity is greater than 90%
                          stemWords         = TRUE,    # Applying stemming(involves trimming words suchs calling, called and calls to call)
                          originalMatrix    = dtm_train_NB
  )
  
  dtm_matrix  <- apply(dtm, 2, convert_count)
  
  dtm_frame  <- as.data.frame(as.matrix(dtm_matrix))
  
  #  Making predictions
  pred_NB = predict(model_NB, type = 'class', newdata = dtm_frame)
  
  if (onlyOne == TRUE){
    pred_NB = pred_NB[1]
  }
  
  pred_NB = as.numeric(as.character(pred_NB))
  
  return(pred_NB)
  
}



