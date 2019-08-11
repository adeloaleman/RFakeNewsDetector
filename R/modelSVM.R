#' modelSVM Function
#'
#' This function allows you to check if a news article is authentic or fake.
#' It was trained using the SVM Algorithms from the package RTextTools-e1071, and the the Kaggle Fake News Dataset
#' @param fileName 
#' @export
#' @examples
#' modelSVM(FileName)
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
# 
# 
# path_models = '/home/adelo/1-system/desktop/it_cct/5-Applied_Technology_Group_Project/gofaaaz-machine_learning/r_prog/1codes/0models/0finalModels'
# setwd(path_models)
# 
# models_GLMNET_MAXENT_SVM    = readRDS('kfn1-kfn2-dataset3_100_weighting_FALSE_removeSparseTerms_0_90_GLMNET_MAXENT_SVM_models.rds')
# dtm_train_GLMNET_MAXENT_SVM = readRDS('kfn1-kfn2-dataset3_100_weighting_FALSE_removeSparseTerms_0_90_dtm_train.rds')


modelSVM <- function(fileName){
  
  
  ResortDtm <- function(working.dtm) {
    working.df <- data.frame(i = working.dtm$i, j = working.dtm$j, v = working.dtm$v)  # create a data frame comprised of i,j,v values from the sparse matrix passed in.
    working.df <- working.df[order(working.df$i, working.df$j), ]                      # sort the data frame first by i, then by j.
    working.dtm$i <- working.df$i                                                      # reassign the sparse matrix' i values with the i values from the sorted data frame.
    working.dtm$j <- working.df$j                                                      # ditto for j values.
    working.dtm$v <- working.df$v                                                      # ditto for v values.
    return(working.dtm)                                                                # pass back the (now sorted) data frame.
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
                          # weighting       = function(x) weightTfIdf(x, normalize = F),
                          toLower           = TRUE,   # Converts to lowecase
                          removeNumbers     = TRUE,   # Removes numbers
                          removeStopwords   = TRUE,   # Removes stop words
                          removePunctuation = TRUE,   # Removes punctuation
                          stripWhitespace   = TRUE,   # Remove blank space
                          removeSparseTerms = 0.90,   # Remove all terms in the corpus whose sparsity is greater than 99%
                          stemWords         = TRUE,    # Applying stemming(involves trimming words suchs calling, called and calls to call)
                          originalMatrix    = dtm_train_GLMNET_MAXENT_SVM
  )
  
  
  dtm_resorted  <- ResortDtm(dtm)
  
  
  if (length(dtm[["i"]]) > 0){
    
    matrix_container <- create_container(dtm_resorted,
                                         t(integer(length(text))),
                                         trainSize = NULL,
                                         testSize  = 1:length(text),
                                         virgin    = FALSE
    )
    
    
    #  Making predictions
    pred_test_GLMNET_MAXENT_SVM <- classify_models(matrix_container, models_GLMNET_MAXENT_SVM)
    pred_test_SVM = factor(pred_test_GLMNET_MAXENT_SVM$SVM_LABEL)
    
  }else{
    print("Warning!! The length of the DTM is zero")
    pred_test_SVM = rep(1, length(text))
  }
  
  if (onlyOne == TRUE){
    pred_test_SVM = pred_test_SVM[1]
  }
  
  pred_test_SVM = as.numeric(as.character(pred_test_SVM))
  
  return(pred_test_SVM)
  
}



