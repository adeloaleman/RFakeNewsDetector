#' modelXGBoost Function
#'
#' This function allows you to check if a news article is authentic or fake.
#' It was trained using the xgboost package and the the Kaggle Fake News Dataset
#' @param fileName 
#' @export
#' @examples
#' modelXGBoost(FileName)
#' @import xgboost
#' @import tm
#' @import text2vec
#' @import tools
#' @import SnowballC
#' @importFrom readr read_file


# library(xgboost)
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
# model_XGB   = readRDS('kfn1-kfn2-dataset3_100_weighting_FALSE_removeSparseTerms_0_90_maxdepth_7_nrounds_20000_XGB_model.rds')
# vocab_train = readRDS('kfn1-kfn2-dataset3_100_weighting_FALSE_removeSparseTerms_0_90_maxdepth_7_nrounds_20000_vocab_train.rds')


modelXGBoost <- function(fileName){
  
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
  text <- removeWords(text, stopwords("en"))               # stopwords
  text <- stripWhitespace(text)                            # Remove blank space
  text <- SnowballC::wordStem(text, language = "english")  # Stemming Words
  
  
  #  Build a document-term matrix using the tokenized review text. This returns a dgCMatrix object ----
  dtm <- create_dtm(itoken(text,
                           preprocessor = tolower,
                           tokenizer    = word_tokenizer),
                    vocab_vectorizer(vocab_train))
  
  
  #  Making predictions from the model created and displaying a Confusion matrix ----
  # Create our prediction probabilities
  pred_prob <- predict(model_XGB, dtm)
  
  # Set our cutoff
  pred <- ifelse(pred_prob >= 0.5, 1, 0)
  # pred <- ifelse(pred_prob >= 0.5, 0, 1)
  
  prediction <- pred == 0
  
  return(pred)
  # return(prediction)
  
}



