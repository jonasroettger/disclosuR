#' Earnings call segmenter
#'
#' Converts one earnings call transcript from 'FairDisclosure' obtained from 'NexisUni' to an R data frame.
#' @param file The name of the PDF file which the data are to be read
#' from. If it does not contain an absolute path, the file name is
#' relative to the current working directory, getwd().
#' @param sentiment Performs dictionary-based sentiment analysis
#' based on the \code{\link[SentimentAnalysis]{analyzeSentiment}}
#' function (default: FALSE)
#' @param emotion Performs dictionary-based emotion analysis based on the
#' \code{\link[syuzhet]{get_nrc_sentiment}} function (default: FALSE)
#' @param regulatory_focus Calculates the number of words indicative for
#' promotion and prevention focus based on the
#' dictionary developed by \href{https://psycnet.apa.org/record/2015-41859-013}{Gamache et al., 2015}
#' (default: FALSE)
#' @param narcissism Counts the number of pronoun usage and calculates the ratio of first-person singular to
#' first-person plural pronouns. This measure is derived from \href{https://psycnet.apa.org/record/2015-06474-002}{Zhu & Chen, (2015}
#' (default: FALSE)
#' @param laughter Counts the number of times laughter was indicated in a quote. (default: FALSE)
#' @return An R data frame with each row representing one quote. The columns indicate the quarter, year, section
#' (presentation versus Q&A), the speaker's name, role, affiliation, and also three binary indicators on whether the
#' speaker is the host company's (1) CEO, (2) CFO, and/or (3) Chairman.
#' @examples
#' earnings_calls_df <- conference_call_segmenter(file = system.file("inst",
#' "examples",
#' "earnings_calls", "earnings_example_01.pdf",
#' package = "disclosuR"));
#' earnings_calls_df_sentiment <- conference_call_segmenter(file = system.file("inst",
#' "examples",
#' "newswire", "earnings_example_01.pdf",
#' package = "disclosuR"),
#' sentiment = TRUE);
#'
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
conference_call_segmenter <- function(file,
                                      sentiment = FALSE,
                                      emotion = FALSE,
                                      regulatory_focus = FALSE,
                                      laughter = FALSE,
                                      narcissism = FALSE){

  if(!file.exists(file)){

    warning("File does not exist")

  }else{

    message("File exists")
    # read pdf as text
    text <- paste(pdftools::pdf_text(file), sep = " ", collapse = " ")



    # get end of date and date
    date_end <- "Copyright"
    date <- stringr::str_match(stringr::str_replace_all(str_squish(text), "[\r\n]" , ""), paste("Wire", "\\s*(.*?)\\s*", date_end, sep = ""))[[2]]

    # convert the string to a date variable
    date <- as.Date(date, "%B %d, %Y %A")

    # create year variable
    year <- lubridate::year(date)

    # convert the date variable to a character variable containing the weekday
    weekday <- weekdays(date)

    # create quarter variable
    quarter <- substr(trimws(text, "l"), start = 1, stop = 2)

    # message date
    message(paste("date: ", date, sep = ""))

    # split text into vector of lines
    text_by_line <- strsplit(text, "\n")

    # remove title from page
    text <- stringr::str_remove_all(text, text_by_line[[1]][1])

    text_by_line <- strsplit(text, "\n")
    text_by_line <- text_by_line[[1]][16:length(text_by_line[[1]])]


    # starting 2012
    speaker_start <- which(text_by_line == "Corporate Participants")[1]
    speaker_end <- which(text_by_line == "Presentation")[1]

    # check if no speaker identifiable
    if(!is.na(speaker_start) & !is.na(speaker_end)){

      speakers_collapsed <- text_by_line[(speaker_start+2):(speaker_end-2)]
      speakers_collapsed <- ifelse(grepl("Page ", speakers_collapsed),
                                   speakers_collapsed[-which(grepl("Page ", speakers_collapsed))],
                                   speakers_collapsed)
      speakers_collapsed <- ifelse(grepl("Conference Call Participants", speakers_collapsed),
                                   speakers_collapsed[-which(grepl("Conference Call Participants", speakers_collapsed))],
                                   speakers_collapsed)

      speakers_collapsed <- speakers_collapsed[which(grepl("[^[:alnum:] ]", speakers_collapsed))]

      h = 1
      p = 1
      speaker <- c()
      company_and_role <- c()

      for(j in 1:length(speakers_collapsed)){

        # if the line is a speaker, add to speaker vector and iterate to next entry
        if(grepl("\\*", speakers_collapsed[j])){

          speaker[h] <- speakers_collapsed[j]
          h = h +1

          # if the previous lines have been speakers and this one is a company_and_role line, add as many time the company_and_role name as speakers were added
        }else if(!grepl("\\*", speakers_collapsed[j]) &&
                 j > 2 &&
                 grepl("\\*", speakers_collapsed[j-1]) &&
                 grepl("\\*", speakers_collapsed[j-2])){

          company_and_role[p] <- speakers_collapsed[j]
          company_and_role[p+1] <- speakers_collapsed[j]
          p = p +2

          # if the line is a company_and_role, add to company_and_role vector and iteratre to next entry
        }else if(!grepl("\\*", speakers_collapsed[j]) &&
                 j > 3 &&
                 grepl("\\*", speakers_collapsed[j-1]) &&
                 grepl("\\*", speakers_collapsed[j-2]) &&
                 grepl("\\*", speakers_collapsed[j-3])){

          company_and_role[p] <- speakers_collapsed[j]
          company_and_role[p+1] <- speakers_collapsed[j]
          company_and_role[p+2] <- speakers_collapsed[j]
          p = p +3

          # if the line is a company_and_role, add to company_and_role vector and iteratre to next entry
        }else if(!grepl("\\*", speakers_collapsed[j])){

          company_and_role[p] <- speakers_collapsed[j]
          p = p +1


        }
      }

      # create data frame with all speakers and their roles
      speakers_data_frame <- as.data.frame(cbind(speaker, company_and_role))
      min_name_length <- min(length(speakers_data_frame$speaker))

      message(paste("Number of participants: ", nrow(speakers_data_frame), sep = " "))

      if(ncol(speakers_data_frame) > 1){
        # seperate company_and_role and role; also: add host company_and_role indicator
        speakers_data_frame$company <- stringr::str_split_fixed(speakers_data_frame$company_and_role, " - ", 2)[,1]
        speakers_data_frame$host_company <- rep(speakers_data_frame$company[1], nrow(speakers_data_frame))
        speakers_data_frame$role <- stringr::str_split_fixed(speakers_data_frame$company_and_role, " - ", 2)[,2]

        # executive indicator
        speakers_data_frame$host_CEO <- ifelse(grepl("CEO", speakers_data_frame$role), 1, 0)
        speakers_data_frame$host_CFO <- ifelse(grepl("CFO", speakers_data_frame$role), 1, 0)
        speakers_data_frame$host_Chairman <- ifelse(grepl("Chairman", speakers_data_frame$role) |
                                                      grepl("chairman", speakers_data_frame$role), 1, 0)

        # delete asteriks
        speakers_data_frame$speaker <- sapply(speakers_data_frame$speaker, function(x) sub("\\* ", "", x))

        # make speakers upper case
        speakers_data_frame$speaker <- toupper(speakers_data_frame$speaker)

        # add OPERATOR to the speakers list
        speakers_data_frame[nrow(speakers_data_frame) + 1,] = c("OPERATOR","OPERATOR","OPERATOR","OPERATOR", 0,0,0,0)

      }else if(ncol(speakers_data_frame) == 1){

        speakers_one_block <- paste(speakers_collapsed, collapse = " ")
        speaker_seperated <- stringr::str_split_fixed(speakers_one_block, " \\*", n = stringr::str_count(speakers_one_block, " \\*") + 1)
        speakers_data_frame <- as.data.frame(t(speaker_seperated))
        colnames(speakers_data_frame)[1] <- "speaker"


        speakers_data_frame$speaker <- ifelse(grepl("\\*", stringr::str_sub(speakers_data_frame$speaker, 1, 1)),
                                              stringr::str_sub(speakers_data_frame$speaker, 3, nchar(speakers_data_frame$speaker)),
                                              speakers_data_frame$speaker)

        speakers_data_frame$role <- stringr::str_split_fixed(speakers_data_frame$speaker, " - ", 2)[,2]

        speakers_data_frame$speaker2 <- stringr::word(speakers_data_frame$speaker, 1,2, sep= " ")

        speakers_data_frame$speaker <- stringi::stri_replace_all_fixed(speakers_data_frame$speaker, speakers_data_frame$role, " ")
        speakers_data_frame$speaker <- stringi::stri_replace_all_fixed(speakers_data_frame$speaker, speakers_data_frame$speaker2, " ")
        names(speakers_data_frame)[names(speakers_data_frame) == "speaker"] <- "company"
        names(speakers_data_frame)[names(speakers_data_frame) == "speaker2"] <- "speaker"

        # executive indicator
        speakers_data_frame$host_CEO <- ifelse(grepl("CEO", speakers_data_frame$role), 1, 0)
        speakers_data_frame$host_CFO <- ifelse(grepl("CFO", speakers_data_frame$role), 1, 0)
        speakers_data_frame$host_Chairman <- ifelse(grepl("Chairman", speakers_data_frame$role) |
                                                      grepl("chairman", speakers_data_frame$role), 1, 0)

        # make speakers upper case
        speakers_data_frame$speaker <- toupper(speakers_data_frame$speaker)

        # add OPERATOR to the speakers list
        speakers_data_frame[nrow(speakers_data_frame) + 1,] = c("OPERATOR","OPERATOR", 0,0,0,"OPERATOR","OPERATOR", "OPERATOR")

      }

      # split script in presentation part
      presentation_text <- sub("Questions and Answers.*", "", text)

      # split text into vector of lines
      text_by_line <- strsplit(presentation_text, "\n")
      text_by_line <- text_by_line[[1]]

      # speaker patterns creation for grepl function
      speakers_patterns <- paste(speakers_data_frame$speaker, collapse = "|")

      # get line in which quotes start
      quote_start <- which(grepl(speakers_patterns, text_by_line, fixed = F))

      which(stringr::str_detect(text_by_line, speakers_patterns))

      # get line in which quotes end
      quote_end <- quote_start-1
      quote_end <- c(quote_end[-1], length(text_by_line))

      # message number of quotes
      message(paste("Number of quotes: ", length(quote_start), sep = " "))

      # iterate over quotes
      quotes <- c()
      for(d in 1:length(quote_start)){

        quotes[d] <- paste(text_by_line[quote_start[d]:quote_end[d]], collapse = " ")

      }

      # get speaker of quote index
      speaker_quotes_index <- sapply(speakers_data_frame$speaker, function(x) which(grepl(x, quotes)))

      quotes_data_frame_pres <- data.frame(year = NA, quarter = NA, date = NA, weekday = NA, section = NA,
                                           speaker_name = NA, speaker_role = NA, speaker_company = NA,
                                           host_company = NA, host_CEO = NA, host_CFO = NA, host_Chairman = NA,
                                           quote_index = NA,
                                           quote = NA)

      # allocate quotes to speakers
      # loop over speakers
      for (name in names(speaker_quotes_index)) {

        quotes_data_frame <- data.frame(year = NA, quarter = NA, date = NA, weekday = NA, section = NA,
                                        speaker_name = NA, speaker_role = NA, speaker_company = NA,
                                        host_company = NA, host_CEO = NA, host_CFO = NA, host_Chairman = NA,
                                        quote_index = NA, quote = NA)

        speaker_quote_count <- length(speaker_quotes_index[[name]])

        if(speaker_quote_count > 0){

          quotes_data_frame[1:speaker_quote_count,] <- NA

          quotes_data_frame$year <- rep(year, speaker_quote_count)
          quotes_data_frame$quarter <- rep(quarter, speaker_quote_count)
          quotes_data_frame$date <- rep(date, speaker_quote_count)
          quotes_data_frame$weekday <- rep(weekday, speaker_quote_count)
          quotes_data_frame$section <- rep("presentation", speaker_quote_count)
          quotes_data_frame$speaker_name <- rep(name, speaker_quote_count)
          quotes_data_frame$speaker_role <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$role[1], speaker_quote_count)
          quotes_data_frame$speaker_company <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$company[1], speaker_quote_count)
          quotes_data_frame$host_company <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_company[1], speaker_quote_count)
          quotes_data_frame$host_CEO <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_CEO[1], speaker_quote_count)
          quotes_data_frame$host_CFO <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_CFO[1], speaker_quote_count)
          quotes_data_frame$host_Chairman <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_Chairman[1], speaker_quote_count)

          # loop over quotes per speaker
          s = 1
          for(t in speaker_quotes_index[[name]]){

            quotes_data_frame[s, "quote_index"] <- t
            quotes_data_frame[s, "quote"] <- quotes[t]
            s = s + 1

          }

          # bind dataframes together
          quotes_data_frame_pres <- rbind(quotes_data_frame_pres, quotes_data_frame)

        }


      }


      q_and_a_text <- sub(".*Questions and Answers", "", text)


      # split text into vector of lines
      text_by_line <- strsplit(q_and_a_text, "\n")
      text_by_line <- text_by_line[[1]]

      # speaker patterns creation for grepl function
      speakers_patterns <- paste(speakers_data_frame$speaker, collapse = "|")

      # get line in which quotes start
      quote_start <- which(grepl(speakers_patterns, text_by_line, fixed = F))

      # get line in which quotes end
      quote_end <- quote_start-1
      quote_end <- c(quote_end[-1], length(text_by_line))

      # message number of quotes
      message(paste("Number of quotes: ", length(quote_start), sep = " "))

      # iterate over quotes
      quotes <- c()
      for(d in 1:length(quote_start)){

        quotes[d] <- paste(text_by_line[quote_start[d]:quote_end[d]], collapse = " ")

      }

      # get speaker of quote index
      speaker_quotes_index <- sapply(speakers_data_frame$speaker, function(x) which(grepl(x, quotes)))

      quotes_data_frame_q_and_a <- data.frame(year = NA, quarter = NA, date = NA, weekday = NA, section = NA,
                                              speaker_name = NA, speaker_role = NA, speaker_company = NA,
                                              host_company = NA, host_CEO = NA, host_CFO = NA, host_Chairman = NA,
                                              quote_index = NA,
                                              quote = NA)

      # allocate quotes to speakers
      # loop over speakers
      for (name in names(speaker_quotes_index)) {

        quotes_data_frame <- data.frame(year = NA, quarter = NA, date = NA, weekday = NA, section = NA,
                                        speaker_name = NA, speaker_role = NA, speaker_company = NA,
                                        host_company = NA, host_CEO = NA, host_CFO = NA, host_Chairman = NA,
                                        quote_index = NA,
                                        quote = NA)

        speaker_quote_count <- length(speaker_quotes_index[[name]])

        if(speaker_quote_count > 0){

          quotes_data_frame[1:speaker_quote_count,] <- NA

          quotes_data_frame$year <- rep(year, speaker_quote_count)
          quotes_data_frame$quarter <- rep(quarter, speaker_quote_count)
          quotes_data_frame$date <- rep(date, speaker_quote_count)
          quotes_data_frame$weekday <- rep(weekday, speaker_quote_count)
          quotes_data_frame$section <- rep("q_and_a", speaker_quote_count)
          quotes_data_frame$speaker_name <- rep(name, speaker_quote_count)
          quotes_data_frame$speaker_role <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$role[1], speaker_quote_count)
          quotes_data_frame$speaker_company <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$company[1], speaker_quote_count)
          quotes_data_frame$host_company <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_company[1], speaker_quote_count)
          quotes_data_frame$host_CEO <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_CEO[1], speaker_quote_count)
          quotes_data_frame$host_CFO <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_CFO[1], speaker_quote_count)
          quotes_data_frame$host_Chairman <- rep(speakers_data_frame[speakers_data_frame$speaker == name,]$host_Chairman[1], speaker_quote_count)


          # loop over quotes per speaker
          s = 1
          for(t in speaker_quotes_index[[name]]){

            quotes_data_frame[s, "quote_index"] <- t
            quotes_data_frame[s, "quote"] <- quotes[t]
            s = s + 1

          }

          # bind dataframes together
          quotes_data_frame_q_and_a <- rbind(quotes_data_frame_q_and_a, quotes_data_frame)

        }


      }

      # bind presentation and q_and_a data frame together
      quotes_data_frame_one_call <- rbind(quotes_data_frame_pres, quotes_data_frame_q_and_a)

      # remove rows with only NA values
      quotes_data_frame_one_call <- quotes_data_frame_one_call[stats::complete.cases(quotes_data_frame_one_call), ]

      ### remove names from the quote column
      quotes_data_frame_one_call$quote <- gsub(".*:","",quotes_data_frame_one_call$quote)

      # reformat data
      quotes_data_frame_one_call$date <- as.Date(quotes_data_frame_one_call$date, origin = "1970-01-01")

      # sort data frame
      quotes_data_frame_one_call <- quotes_data_frame_one_call %>%
        dplyr::arrange(.data$section, .data$quote_index)

    }

    # analyze sentiment of each quote
    if(sentiment){

      # get sentiment per quote
      sentiment_quotes_sentimentAnalysis <- SentimentAnalysis::analyzeSentiment(
        quotes_data_frame_one_call$quote)


      # bind sentiment to original dataframe
      quotes_data_frame_one_call <- cbind(quotes_data_frame_one_call, sentiment_quotes_sentimentAnalysis)


    }

    # analyze emotion of each quote
    if(emotion){

      ### get emotions per quotes
      nrc_data <- syuzhet::get_nrc_sentiment(quotes_data_frame_one_call$quote)
      quotes_data_frame_one_call <- cbind(quotes_data_frame_one_call, nrc_data)

    }

    # analyze regulatory focus
    if(regulatory_focus ){

      # create libraries
      promotion.focus <- paste(c("Accomplish", "Achieve", "Advancement", "Aspiration", "Aspire", "Attain", "Desire", "Earn", "Expand", "Gain", "Grow",
                                 "Hope", "Hoping", "Ideal", "Improve", "Increase", "Momentum", "Obtain", "Optimistic", "Progress", "Promoting", "Promotion",
                                 "Speed", "Swift", "Toward", "Velocity", "Wish"), collapse = "|")

      prevention.focus <- paste(c("Accuracy", "Afraid", "Careful", "Anxious", "Avoid", "Conservative", "Defend", "Duty", "Escape", "Escaping", "Evade", "Fail", "Fear",
                                  "Loss", "Obligation", "Ought", "Pain", "Prevent", "Protect", "Responsible", "Risk", "Safety", "Security", "Threat", "Vigilance"),
                                collapse = "|")

      # get word count per quote
      quotes_data_frame_one_call$word_count <- stringr::str_count(quotes_data_frame_one_call$quote, "\\w+")

      # count frequency words regulatory focus
      quotes_data_frame_one_call$promotion_focus <- stringr::str_count(quotes_data_frame_one_call$quote_withoutName,
                                                                       promotion.focus) / quotes_data_frame_one_call$word_count
      quotes_data_frame_one_call$prevention_focus <- stringr::str_count(quotes_data_frame_one_call$quote_withoutName,
                                                                        prevention.focus) / quotes_data_frame_one_call$word_count

    }

    # analyze laughter
    if(laughter){

      # (1) check if a CEO's quote contains "(laughter)"
      quotes_data_frame_one_call$laughter <- stringi::stri_count(quotes_data_frame_one_call$quote,
                                                                 regex = "laughter|laugh|laughs")

    }

    if(narcissism){

      for(i in 1:nrow(quotes_data_frame_one_call)){

        pronoun_counts <- qdap::pronoun_type(quotes_data_frame_one_call[i, "quote"])[["raw"]]

      }


      # cbind results of pronoun analysis to initial quote dataframe
      quotes_data_frame_one_call <- cbind(quotes_data_frame_one_call, pronoun_counts)

      # calculate ratio of first person singular to first person plural pronoun usage ratio
      # calculate pronoun usage
      quotes_data_frame_one_call$first_pronoun_singular_usage <- (quotes_data_frame_one_call$I + quotes_data_frame_one_call$me)
      quotes_data_frame_one_call$first_pronoun_plural_usage <-  (quotes_data_frame_one_call$we + quotes_data_frame_one_call$us)
      quotes_data_frame_one_call$first_pronoun_usage <- quotes_data_frame_one_call$first_pronoun_singular_usage / (quotes_data_frame_one_call$first_pronoun_singular_usage +
                                                                                                                     quotes_data_frame_one_call$first_pronoun_plural_usage)



    }

    return(quotes_data_frame_one_call)

  }
}

#' Earnings call segmenter (multiple files)
#'
#' Converts all 'FairDisclosure' earnings call transcripts obtained from 'NexisUni'
#' in a folder to an R data frame.
#' @param folder_path The name of the folder which the data are to be read
#' from. If it does not contain an absolute path, the file name is
#' relative to the current working directory.
#' @param sentiment Performs dictionary-based sentiment analysis
#' based on the \code{\link[SentimentAnalysis]{analyzeSentiment}}
#' function (default: FALSE)
#' @param emotion Performs dictionary-based emotion analysis based on the
#' @param regulatory_focus Calculates the number of words indicative for
#' promotion and prevention focus based on the
#' dictionary developed by \href{https://psycnet.apa.org/record/2015-41859-013}{Gamache et al., 2015}
#' (default: FALSE)
#' @param narcissism Counts the number of pronoun usage and calculates the ratio of first-person singular to
#' first-person plural pronouns. This measure is derived from \href{https://psycnet.apa.org/record/2015-06474-002}{Zhu & Chen, (2015}
#' (default: FALSE)
#' @param laughter Counts the number of times laughter was indicated in a quote. (default: FALSE)
#' @return An R data frame with each row representing one quote. The columns indicate the quarter, year, section
#' (presentation versus Q&A), the speaker's name, role, affiliation, and also three binary indicators on whether the
#' speaker is the host company's (1) CEO, (2) CFO, and/or (3) Chairman.
#' @examples
#' earnings_calls_df <- conference_call_segmenter_folder(
#' folder_path = system.file("inst",
#' "examples",
#' "earnings_calls",
#' package = "disclosuR"));
#' earnings_calls_df_sentiment <- conference_call_segmenter_folder(
#' folder_path = system.file("inst",
#' "examples",
#' "newswire",
#' sentiment = TRUE,
#' package = "disclosuR"));
#'
#' @export
#'
conference_call_segmenter_folder <- function(folder_path, sentiment = FALSE,
                                             emotion = FALSE,
                                             regulatory_focus = FALSE,
                                             laughter = FALSE,
                                             narcissism = FALSE){

  if(!dir.exists(folder_path)){

    warning("File does not exist")

  }else{

    message("Folder exists")

  }

  # message folder
  message(paste("Folder to analyze:", folder_path, sep = " "))

  # get all pdf in a folder
  pdf_files <- list.files(path = folder_path,
                          full.names = T,
                          pattern = "pdf$|PDF$")

  # apply the function to each PDF file using lapply and store the results in a list
  result_list <- lapply(pdf_files, conference_call_segmenter, sentiment = sentiment,
                        emotion = emotion,
                        regulatory_focus = regulatory_focus,
                        laughter = laughter,
                        narcissism = narcissism)

  # merge all the dataframes into one using the `dplyr` package
  final_result <- dplyr::bind_rows(result_list)

  # return final data frame
  final_result$date <- as.Date(final_result$date)
  return(final_result)

}

#' Newswire segmenter
#'
#' Takes a PDF document containing a 'newswire' document obtained from 'NexisUni' and transforms it into
#' an R data frame consisting of one row
#' @param file The name of the PDF file which the data are to be read
#' from. If it does not contain an absolute path, the file name is
#' relative to the current working directory, getwd().
#' @param sentiment Performs dictionary-based sentiment analysis
#' based on the \code{\link[SentimentAnalysis]{analyzeSentiment}}
#' function (default: FALSE)
#' @param emotion Performs dictionary-based emotion analysis based on the
#' \code{\link[syuzhet]{get_nrc_sentiment}} function (default: FALSE)
#' @param regulatory_focus Calculates the number of words indicative for
#' promotion and prevention focus based on the
#' dictionary developed by \href{https://psycnet.apa.org/record/2015-41859-013}{Gamache et al., 2015}
#' (default: FALSE)
#' @param narcissism Counts the number of pronoun usage and calculates the ratio of first-person singular to
#' first-person plural pronouns. This measure is derived from \href{https://psycnet.apa.org/record/2015-06474-002}{Zhu & Chen, (2015}
#' (default: FALSE)
#' @param laughter Counts the number of times laughter was indicated in a quote. (default: FALSE)
#' @param text_clustering Applies a document categorization using a dictionary developed based on the framework developed by \href{https://escholarship.org/uc/item/026179rh}{Graffin et al., 2016}. (default: FALSE)
#' @return An R data frame with each row representing one 'newswire' article. The columns indicate the title, text,
#' 'newswire', date, and weekday.
#' @examples
#' newswire_df <- newswire_segmenter(
#' file = system.file("inst",
#' "examples",
#' "newswire", "newswire_example_01.pdf",
#' package = "disclosuR"));
#' newswire_df_sentiment <- newswire_segmenter(
#' file = system.file("inst",
#' "examples",
#' "newswire", "newswire_example_01.pdf",
#' sentiment = TRUE,
#' package = "disclosuR"));
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom stringr str_match
#' @importFrom stringr str_split
#' @importFrom stringr str_count
#' @importFrom tm removeWords
#' @importFrom tm stopwords
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
newswire_segmenter <- function(file,
                               sentiment = FALSE,
                               emotion = FALSE,
                               regulatory_focus = FALSE,
                               laughter = FALSE,
                               narcissism = FALSE,
                               text_clustering = FALSE){

  if(!file.exists(file)){

    warning("File does not exist")

  }else{

    message(file)
    # read pdf as text
    text <- paste(pdftools::pdf_text(file), sep = " ", collapse = " ")


    # reformat text
    text <- stringr::str_replace_all(text, "[\r\n]" , " ")
    text <- stringr::str_replace_all(text, "[\r\n]" , " ")
    text <- stringr::str_squish(text)

    # get newswire
    newswires <- c("Canada NewsWire", "PR Newswire", "ENP Newswire", "States News Service", "Marketwire",
                   "Plus Company Updates(PCU)", "Targeted News Service", "GlobeNewswire", "GlobeNewswire",
                   "MediaNet Press Release Wire", "US Fed News", "SocialBizWire", "M2 PressWIRE",
                   "Marketwire (Canada)", "Marketwired", "Market News Publishing", "dpa-AFX International ProFeed",
                   "Business Wire", "Web newswire", "Associated Press State & Local", "Thai News Service",
                   "Impact News Service", "Global English",
                   "MENA English (Middle East and North Africa Financial Network)", "Thomson Reuters ONE",
                   "Hugin - English", "London Stock Exchange Aggregated Regulatory News Service (ARNS)",
                   "Internet Business News", "M2 EquityBites", "Associated Press Financial Wire",
                   "Associated Press International", "The Associated Press", "Canadian Press",
                   "ACCESSWIRE", "AAP Newsfeed", "US Official News", "FinancialWire", "DC Daybook - Policy & News Events",
                   "iCrowdNewswire (English)", "PR Newswire Europe", "Premium Official News", "SDA - Basisdienst Deutsch",
                   "AWP OTS (Original text service) - English", "Original text service (ots)", "MarketLine NewsWire (Formerly Datamonitor)",
                   "MacReport/eTeligis", "SKRIN Market & Corporate News")

    # get newswire
    for(l in newswires){

      if(grepl(l, text)){

        newswire <- l
        break

      }
    }

    # define weekdays character vector
    week_days <- paste(c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), collapse = "|")

    # get title
    title <- strsplit(text, newswire)[[1]][1]

    # get end of date and date
    date_end <- "Copyright"
    date <- stringr::str_match(text, paste(newswire, "\\s*(.*?)\\s*", date_end, sep = ""))[[2]]

    # convert data to real date
    date <- stringr::str_split(date, pattern = week_days)[[1]][1]
    date <- as.character(date)
    date <- as.Date(date, format = "%B %d, %Y")

    # get weekday
    weekday <- weekdays(date)

    # create data frame
    press_data_temp <- as.data.frame(cbind(title, newswire, date, text, weekday))


    # analyze sentiment of each quote
    if(sentiment){

      # get sentiment per quote
      sentiment_quotes_sentimentAnalysis <- SentimentAnalysis::analyzeSentiment(
        press_data_temp$text)

      # bind sentiment to original dataframe
      press_data_temp <- cbind(press_data_temp, sentiment_quotes_sentimentAnalysis)


    }

    # analyze emotion of each quote
    if(emotion){

      ### get emotions per quotes
      nrc_data <- syuzhet::get_nrc_sentiment(press_data_temp$text)
      press_data_temp <- cbind(press_data_temp, nrc_data)

    }

    # analyze regulatory focus
    if(regulatory_focus ){

      # create libraries
      promotion.focus <- paste(c("Accomplish", "Achieve", "Advancement", "Aspiration", "Aspire", "Attain", "Desire", "Earn", "Expand", "Gain", "Grow",
                                 "Hope", "Hoping", "Ideal", "Improve", "Increase", "Momentum", "Obtain", "Optimistic", "Progress", "Promoting", "Promotion",
                                 "Speed", "Swift", "Toward", "Velocity", "Wish"), collapse = "|")

      prevention.focus <- paste(c("Accuracy", "Afraid", "Careful", "Anxious", "Avoid", "Conservative", "Defend", "Duty", "Escape", "Escaping", "Evade", "Fail", "Fear",
                                  "Loss", "Obligation", "Ought", "Pain", "Prevent", "Protect", "Responsible", "Risk", "Safety", "Security", "Threat", "Vigilance"),
                                collapse = "|")

      # get word count per quote
      press_data_temp$word_count <- stringr::str_count(press_data_temp$text, "\\w+")

      # count frequency words regulatory focus
      press_data_temp$promotion_focus <- stringr::str_count(press_data_temp$text,
                                                            promotion.focus) / press_data_temp$word_count
      press_data_temp$prevention_focus <- stringr::str_count(press_data_temp$text,
                                                             prevention.focus) / press_data_temp$word_count

    }

    # analyze laughter
    if(laughter){

      # (1) check if a CEO's quote contains "(laughter)"
      press_data_temp$laughter <- stringi::stri_count(press_data_temp$text,
                                                      regex = "laughter|laugh|laughs")

    }

    if(narcissism){

      for(i in 1:nrow(press_data_temp)){

        pronoun_counts <- qdap::pronoun_type(press_data_temp[i, "text"])[["raw"]]

      }

      # cbind results of pronoun analysis to initial quote dataframe
      press_data_temp <- cbind(press_data_temp, pronoun_counts)

      # calculate ratio of first person singular to first person plural pronoun usage ratio
      # calculate pronoun usage
      press_data_temp$first_pronoun_singular_usage <- (press_data_temp$I + press_data_temp$me)
      press_data_temp$first_pronoun_plural_usage <-  (press_data_temp$we + press_data_temp$us)
      press_data_temp$first_pronoun_usage <- press_data_temp$first_pronoun_singular_usage / (press_data_temp$first_pronoun_singular_usage +
                                                                                               press_data_temp$first_pronoun_plural_usage)

    }


    if(text_clustering){

      # Define function for text pre-processing
      preprocess_text <- function(text) {
        # Convert text to lowercase
        text <- tolower(text)
        # Remove punctuation and numbers
        text <- gsub("[^[:alpha:]]", " ", text)
        # Remove extra whitespace
        text <- gsub("\\s+", " ", text)
        # Remove stop words
        text <- removeWords(text, stopwords("english"))
        # remove all words shorter than three letters
        text <- gsub("\\b\\w{1,2}\\b", "", text)
        # stem words
        text <- SnowballC::wordStem(text)
        # Return pre-processed text
        return(text)
      }

      # apply pre-processing function to title column
      press_data_temp$preprocessed_title <- preprocess_text(press_data_temp$title)

      # define category keywords
      keywords <- data.frame(Category = c("Earnings releases",
                                          "Earnings guidance",
                                          "Change in dividend rate",
                                          "New product",
                                          "Customer win",
                                          "Social good (e.g., donation, sponsorship),  training, professional development",
                                          "Received award from third party",
                                          "Buyback or split stock",
                                          "Results of a sponsored study",
                                          "New executive or director",
                                          "Divestiture or plant closing",
                                          "Settlement of litigation or other legal dispute",
                                          "Executive retirement",
                                          "Change of stock exchange listing",
                                          "Debt issuance",
                                          "Other acquisition",
                                          "Completion of another acquisition",
                                          "Recall or safety issue",
                                          "Partnership announcements",
                                          "Other"),
                             Keywords = c("earning release|earning announcement|earning statement|earning|revenue increase|operating income|operating margin|annual revenues|earning per share|comparable cash growth|index increase|index decrease|report sale result|release result|report result|announces revenue|announces revenue of million|over prior year|worldwide revenue growth|reports strong results|record growth|record revenue",
                                          "earnings guidance|guidance|earnings per diluted share|updates earnings per share guidance|currently expects its earnings per share|guidance ranges previously announced|preliminairy result|announces preliminairy revenue|affirms guidance|reaffirms guidance|update guidance",
                                          "dividend rate|dividend increases|divided decreases|dividend raise|dividend decline|per share|dividend change|dividend adjusted|increases dividend percent|decreases dividend percent|declares quarterly dividend|raising quarterly dividend|quarterly cash distribution|declares dividend|dividend declaration|announces quarterly dividend|raises quaterly dividend|declares consecutive quaterly dividend",
                                          "new product|other product|product development|launches|develop|implement|introduces|new enhancements|helps design and launch|to improve experience|chooses to create|select to deliver|new offering|deliver new|new release|new version|upgraded to|unveils enhanced|advanced|expand existing array|further enhanced|announced the launch|a new line of|new system|to develop new|received upgrades|announces launch|now available|delivers the|first to open|unveil new|introduces new product|introducing product|roll out first|integrated solutions|adds new",
                                          "customer win|new customer|customer increase|customer raise|sale increase|sale raise|loyalty|contract to|selected by|chosen to help|contract value|new order|new contract to deploy|selected to help meet|contract with|contract to help",
                                          "social good|donation|sponsorship|charity|charities|climate|CSR|talent|awards to help|foundation|women|footprint|maternity leave|net zero|corporate citizenship|training|professional development|lgbt|will present conference|fundraiser|donates|present at the conference|to speak at forum|will make a presentation|present at upcoming conferences|attend conference|participate conference|to sponsor|to speak at conference|to present at conference|summun|fundraising event",
                                          "received|win|achieve|winner|named|positioned as a leader|ranked|recognized|achieves leader position|names leader|of the year|honored for|awards",
                                          "buyback stock|buyback share|stock repurchase|share repurchase|split stock|split share|repurchase outstanding common stock",
                                          "study|survey|studies|sponsor result|research|sponsor|result study|research finds|argues|study shows|according to|report finds|research sponsored by|survey shows|research indicates|new report highlights|study identifies ways|the findings",
                                          "new executive|new director|new ceo|executive change|director change|ceo change|appoints as|names|hire|joins|steps down|board of directors|joins board|appointed to|is appointed|appoints officer|appoints new executive director|announces new leadership appointments|assumes ceo role|chairman|leadership change",
                                          "divestiture|dispose asset|dispose stock|stock sell|stock exchange|shutdown|temporal shutdown|permanent shutdown|plant closing|completes the sale|completes the transfer",
                                          "litigation|settlement litigation|lawsuit|policy|settle lawsuit|dispute|settle dispute|infringement|injunction|federal contract|add new patent|fda",
                                          "retirement|ceo retire|executive retire|director retire|will retire",
                                          "stock change|stock switch|stock transfer|stock listing|insurers invest|investors shift away",
                                          "debt issuance|government cost|loan cost|bond cost|debt issuance fee",
                                          "acquire|to acquire|announce acquisition|intent to acquire|agreement to acquire",
                                          "acquisition complete|acquisition finish|close acquisition|terms of the transaction|acquisition enhences|it has completed acquisition|it has completed the integration|has acquired the assets|has completed the sale|announced completion of acquisition|completes acquisition of|completes sale of|completes combination of",
                                          "recall|safety issue|threats|prevention|concerns|warning letter",
                                          "forms alliance|collaborates with to extend|join forces|expand relationship|partners with|collaborates with|teams with|sign agreement|collaborate to|expand capabilities|joining together to|announced a partnership|bringing together organizations|has collaborated with|join forces to build|announce a strategic partnership|new partnership|partner together to",
                                          "conference call|to discuss quarter results|cfo remarks|board declares|host conference call for|webcast earnings conference call|posts new presentation|annual meeting stockholders|executives to present|announces conference call|annual meeting shareholders|to participate at investor|investor conference|investor briefing|host annual meeting|announces quarter earnings webcast|announces guidance|schedules conference call|new office|opened a new|co locate|to host conference call|will release|to webcast"))

      # initialize category columns with zeroes
      for (category in unique(keywords$Category)) {
        press_data_temp[[category]] <- 0
      }

      # loop through each row in press_data_temp and update the counts for each category
      for (i in 1:nrow(press_data_temp)) {
        # initialize a vector to store the counts for each category
        counts <- rep(0, nrow(keywords))
        for (j in 1:nrow(keywords)) {
          # get the category and keywords from df2
          category <- keywords$Category[j]
          keywords_list <- unlist(strsplit(keywords$Keywords[j], "\\|"))
          # count the number of matches in the text column of press_data_temp
          count <- sum(stringr::str_count(press_data_temp$preprocessed_title[i], stringr::regex(keywords_list, ignore_case = TRUE)))
          # store the count for this category
          counts[j] <- count
          # update the category column in press_data_temp
          press_data_temp[i, category] <- count
        }
      }


      # add the most frequent column name to a new column
      # Create new column to store column names with highest values
      press_data_temp$category_Graffin <- apply(press_data_temp[, which(names(press_data_temp) == "preprocessed_title"):ncol(press_data_temp)], 1, function(row) {
          # Check if all values in the row are zero
          if(all(row == 0)){
            return("Others")
          }

          # Get column index with maximum value in the row
          max_col_idx <- which.max(row)

          # Get column names with maximum value in the row
          max_cols <- names(row)[row == max(row)]

          # If multiple columns have equally high numbers, paste them together with "OR"
          if (length(max_cols) > 1) {
            paste(max_cols, collapse = " OR ")
          } else {
            max_cols
          }
        })

    }

    # assign valence to categoru according to Graffin
    # Create a vector of terms
    terms_positive <- paste(c("Change in dividend rate",
                         "New product",
                         "Customer win",
                         "Social good (e.g., donation, sponsorship),  training, professional development",
                         "Received award from third party",
                         "Buyback or split stock",
                         "Results of a sponsored study",
                         "Partnership announcements"), collapse = "|")

    terms_negative <- paste(c("Other acquisition",
                        "Completion of another acquisition",
                        "Recall or safety issue"), collapse = "|")

    terms_neutral <- paste(c("New executive or director",
                       "Divestiture or plant closing",
                       "Settlement of litigation or other legal dispute",
                       "Executive retirement",
                       "Change of stock exchange listing",
                       "Debt issuance",
                       "Others"), collapse = "|")

    terms_ambiguous <- paste(c("Earnings releases",
                         "Earnings guidance"), collapse =  "|")

    # Use grepl() to check if any of the terms are found in category_Graffin
    press_data_temp <- press_data_temp %>%
      dplyr::mutate(valence_category = ifelse(
        grepl(terms_positive, .data$category_Graffin), "positive",
        ifelse(grepl(terms_negative, .data$category_Graffin), "negative",
               ifelse(grepl(terms_neutral, .data$category_Graffin), "neutral",
                      ifelse(
                        grepl(terms_ambiguous, .data$category_Graffin) & .data$SentimentHE > 0.001, "positive",
                        ifelse(
                          grepl(terms_ambiguous, .data$category_Graffin) & .data$SentimentHE < -0.001, "negative",
                          ifelse(
                            grepl(terms_ambiguous, .data$category_Graffin) & .data$SentimentHE >= -0.001 & .data$SentimentHE <= 0.001, "neutral", NA
                          )
                        )
                      )
               )
        )
      ))

    # return data frame
    return(press_data_temp)

  }

}


#' Newswire segmenter (multiple files)
#'
#' Takes all PDF documents in a folder containing 'newswire' documents obtained from 'NexisUni' and transforms them into
#' an R data frame consisting of one row per document.
#' @param folder_path The path to the folder in which the 'newswire' PDFs reside.
#' If it does not contain an absolute path, the folder name is
#' relative to the current working directory, getwd().
#' @param sentiment Performs dictionary-based sentiment analysis
#' based on the \code{\link[SentimentAnalysis]{analyzeSentiment}}
#' function (default: FALSE)
#' @param emotion Performs dictionary-based emotion analysis based on the
#' \code{\link[syuzhet]{get_nrc_sentiment}} function (default: FALSE)
#' @param regulatory_focus Calculates the number of words indicative for
#' promotion and prevention focus based on the
#' dictionary developed by \href{https://psycnet.apa.org/record/2015-41859-013}{Gamache et al., 2015}
#' (default: FALSE)
#' @param narcissism Counts the number of pronoun usage and calculates the ratio of first-person singular to
#' first-person plural pronouns. This measure is derived from \href{https://psycnet.apa.org/record/2015-06474-002}{Zhu & Chen, (2015}
#' (default: FALSE)
#' @param laughter Counts the number of times laughter was indicated in a quote. (default: FALSE)
#' @param text_clustering Applies a document categorization using a dictionary developed based on the framework developed by \href{https://escholarship.org/uc/item/026179rh}{Graffin et al., 2016}. (default: FALSE)
#' @return An R data frame with each row representing one 'newswire' article. The columns indicate the title, text,
#' 'newswire', date, and weekday. (default: FALSE)
#' @return An R data frame with each row representing one 'newswire' article. The columns indicate the title, text,
#' 'newswire', date, and weekday. Depending on the additional arguments, the output data can also
#' contain sentiment, emotion, regulatory focus, laughter, narcissism and text cluster based on the Graffin et al.
#' categories.
#' @examples
#' newswire_df <- newswire_segmenter_folder(
#' folder_path = system.file("inst",
#' "examples",
#' "newswire",
#' package = "disclosuR"));
#' newswire_df_sentiment <- newswire_segmenter_folder(
#' folder_path = system.file("inst",
#' "examples",
#' "newswire",
#' package = "dislosuR"), sentiment = TRUE);
#'
#' @export
#' @importFrom zoo rollmean
#' @importFrom dplyr %>%
#'
#'
newswire_segmenter_folder <- function(folder_path,
                                      sentiment = FALSE,
                                      emotion = FALSE,
                                      regulatory_focus = FALSE,
                                      laughter = FALSE,
                                      narcissism = FALSE,
                                      text_clustering = FALSE){

  # message folder
  message(paste("Folder to analyze:", folder_path, sep = " "))

  # get all pdf in a folder
  pdf_files <- list.files(path = folder_path,
                          full.names = T,
                          pattern = "pdf$|PDF$")

  # apply the function to each PDF file using lapply and store the results in a list
  result_list <- lapply(pdf_files, newswire_segmenter,
                        sentiment = sentiment,
                        emotion = emotion,
                        regulatory_focus = regulatory_focus,
                        laughter = laughter,
                        narcissism = narcissism,
                        text_clustering = text_clustering)

  # merge all the dataframes into one using the `dplyr` package
  final_result <- dplyr::bind_rows(result_list)

  # return final data frame
  return(final_result)

  }

#' Impression offsetting
#'
#' Takes an event data set containing of dates and CUSIPs which have to correspond
#' to a press data frame compiled by the function \code{\link{newswire_segmenter_folder}}.
#' @param event_data An R data that contains two columns which have to be labeled "date_announced" and "CUSIP". The date_announced
#' column contains the dates of the events for which impression offsetting is calculated. The CUSIP column contains the
#' 8-digit CUSIP of the companies for which impression offsetting is calculated.
#' @param press_data_categorized An R data frame with each row representing one 'newswire' article. The columns indicate the title, text,
#' 'newswire', date, and weekday. It should be the outcome of \code{\link[disclosuR]{newswire_segmenter}} in which both
#' the argument sentiment and text_clustering have been set to TRUE.
#' @return An R data frame which contains the column of the event_data plus three columns for the baseline announcements
#' (positive, neutral, and negative) and three columns for the impression offsetting announcements (positive, neutral, and negative).
#' @examples
#' \dontrun{
#' impression_offsetting(event_data, press_data)
#' }
#' @export
#' @importFrom rlang .data
#'
# create impression offsetting data frame
impression_offsetting <- function(event_data, press_data_categorized){

  # define impression offsetting dataframe
  impression_offsetting <-
    data.frame(
      ID = NA,
      IO = NA)

  # select only relevant columns from press data
  press_data <- press_data_categorized[, c("date", "cusip", "title", "valence_category")]
  press_data <- press_data[press_data$valence_category == "positive", ]
  press_data$date <- as.Date(press_data$date)
  press_data$valence_category <- ifelse(press_data$valence_category == "positive", 1, 0)

  # loop over deals and select impression offsetting press statements
  for (i in 1:nrow(event_data)) {
    temp_cusip <- substr(event_data[i, "cusip"], start = 1, stop = 8)
    temp_date <- as.Date(event_data[i, "date_announced"])
    temp_date_vector <- c(temp_date - 1, temp_date, temp_date + 1)
    temp_ID <- event_data[i, "ID"]

    # filter temp data
    temp_data <-
      press_data[press_data$cusip == temp_cusip &
                   press_data$date %in% temp_date_vector,]

    # add variables to offsetting data
    impression_offsetting[i, "ID"] <- temp_ID
    impression_offsetting[i, "IO"] <- nrow(temp_data)


    message(paste("IO: CUSIP:",
                temp_cusip, "Date:",
                temp_date, "No of press:",
                nrow(temp_data), sep = " "))
  }

  # define impression offsetting data frame for baseline
  impression_offsetting_baseline <-
    data.frame(
      ID = NA,
      baseline_positivity = NA)

  # loop over deals and select impression offsetting press statements
  for (i in 1:nrow(event_data)) {
    temp_cusip <- substr(event_data[i, "cusip"], start = 1, stop = 8)
    temp_date <- as.Date(event_data[i, "date_announced"])
    temp_date_vector <- seq(temp_date - 121, temp_date -30, 1)
    temp_ID <- event_data[i, "ID"]

    # filter temp data
    temp_data <-
      press_data[press_data$cusip == temp_cusip &
                   press_data$date %in% temp_date_vector,]



    # add variables to offsetting data
    impression_offsetting_baseline[i, "ID"] <- temp_ID

    # Assume the start date is the minimum and end date is the maximum in your data
    start_date <- temp_date - 121
    end_date <- temp_date - 30

    # Create a dataframe with all combinations of ID and date
    complete_df <- expand.grid(ID = unique(temp_ID), date = seq(start_date, end_date, by = "day"))

    # Merge the complete dataframe with the original one
    df_complete <- merge(complete_df, temp_data, all.x = TRUE)

    # Replace NA values in positive_announcement with 0
    df_complete$valence_category[is.na(df_complete$valence_category)] <- 0

    # Calculate the three-day rolling average of positive announcements for each firm
    df_complete <- df_complete %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(three_day_avg = rollmean(.data$valence_category, 3, fill = NA, align = "right"))

    # Calculate the baseline positive announcements as the average three-day count in the three-month period
    baseline_positive_announcements <- df_complete %>%
      dplyr::summarise(baseline_positive_announcements = mean(.data$three_day_avg, na.rm = TRUE))
    impression_offsetting_baseline[i, "baseline_positivity"] <- baseline_positive_announcements

    message(paste("Baseline: CUSIP:",
                temp_cusip, "Date:",
                temp_date, "No of press:",
                nrow(temp_data), sep = " "))
  }

  # merge IO and baseline data
  combined_df <- merge(impression_offsetting,
                       impression_offsetting_baseline,
                       by = "ID",
                       all = T)

  # return data frame
  return(combined_df)
}

