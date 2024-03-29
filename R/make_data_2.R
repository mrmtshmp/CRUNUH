#' Make tidy data
#'
#' @importFrom readxl read_excel
#' @import dplyr
#' @import plyr
#' @import tidyr
#' @import MASS
#'
#' @param dir.data Character
#' @param fn.data Character
#' @param fn.record Charater
#' @param dir.output Character
#' @param fn.output Character
#' @param n_of_feedback Numeric
#' @param .guess_max Numeric
#'
#'
#'
#' @export

make_date_data.v2 <- function(
  dir.data, fn.data, fn.record,
  dir.output, fn.output,
  n_of_feedback = 13,
  .guess_max=200
  ){

# dir.data   <- "./testdata"
# fn.data    <- "test_211007.xlsx"
# fn.record  <- "Report_211006.xlsx"
# dir.output <- "./testdata"
# fn.output  <- "output_test_211007.csv"
# n_of_feedback <- 13
# .guess_max<-200
# test<- make_date_data.v2(
#   dir.data,
#   fn.data,
#   fn.record=fn.record,
#   dir.data,
#   fn.output = fn.output
#   )

  data_raw <- read_excel(
    path = sprintf("%s/%s",dir.data,fn.data),
    sheet= 1,
    skip = 0,
    col_names = TRUE,
    guess_max = .guess_max,
    col_types = "text"
    ) %>%
    data.frame()

  data_record <- read_excel(
    path = sprintf("%s/%s",dir.data,fn.record),
    sheet= 1,
    skip = 0,
    na = ".",
    col_types = c("text","text","text","date","numeric","date","date","date","date","date","date","text","date","text","text","text"),
    col_names = TRUE,
    guess_max = .guess_max
    ) %>%
    dplyr::select(
      ID, Title, var.3,
      starts_with("Comment_")
      ) %>%
    data.frame()

  data_record <- data_record[
    # !duplicated(
    #   data_record[,c("ID","Title","var.3", "Comment_1", "Comment_2")]
    #   ) ,
    !is.na(data_record$Comment_1),
    ]

  print(data_record%>%filter(ID=="8_28"))
  print(data_raw[data_raw[,1]=="10-8",])

  vec.make <- function(v, itt){
    out <- c()
    for(i in 1:itt) {
      vec <- sprintf("%s_%s", v, i)
      if(i>1){
        out <- append(out, vec)
        }else out <- vec
      }
    return(
      out
      )
    }
  vec.make(c("Rec_Unit", "Due_Date", "Sent_back"), 2)
  vec.colnames <- c(
    "ID", "Title", "main", "Rec_Somu",
    vec.make(c("Rec_Unit",	"Due_Date",	"Sent_back"), n_of_feedback)
    )
  colnames(data_raw) <- vec.colnames
  data <- data_raw %>%
    gather(var, val, -ID, -Title, -main, -Rec_Somu) %>%
    dplyr::filter(
      !is.na(val) # & !is.na(Rec_Somu)
    ) %>%
    mutate(
      ID    = gsub("^[^0-9]{0,}([0-9]{1,})-([0-9]{1,}).*", "\\1_\\2", ID),
      var.2 = gsub(".[0-9]{1,}","", var),
      var.3 = gsub("[^0-9]{1,}","", var)
      ) %>%
    mutate(
      date = gsub("[^0-9]{1,}","", val),
      flg = gsub("[0-9]{1,}","", val)
      ) %>%
    dplyr::select(-var, -val) %>%
    dplyr::distinct(ID, Title, var.2, var.3,.keep_all = TRUE) %>%
    ddply(
      .(var.3),
      function(D){
        df.out <- D %>% spread(var.2, date)
      }
    ) %>%
    mutate(
      Rec_Unit.2 = try(
        as.Date(
          as.numeric(Rec_Unit),
          origin = "1899-12-30"
        )
      ),
      Due_date.2 = try(
        as.Date(
          as.numeric(Due_Date),
          origin = "1899-12-30"
        )
      ),
      Sent_back.2 = try(
        as.Date(
          as.numeric(Sent_back),
          origin = "1899-12-30"
        )
      )
    )

  df.commented <- data %>%
    filter(flg!="") %>%
    dplyr::select(ID, Title, var.3, flg)

  output <- data %>%
    # filter(flg=="") %>%
    dplyr::select(-flg) %>%
    left_join(df.commented) %>%
    left_join(
      data_record[!duplicated(data_record),] %>%
        dplyr::select(
          ID, Title, var.3, starts_with("Comment_")
          )%>%
        mutate(
          ID  = gsub(
            "[^0-9]{0,}([0-9]{1,})-([0-9]{1,}).*",
            "\\1_\\2",
            ID
            ),
          var.3= as.character(var.3),
          Comment_1=as.Date(Comment_1)
          ),
      by=c("ID", "Title", "var.3")
    ) %>%
    arrange(Rec_Unit.2)

  output <-
    output[
      !duplicated(
        output[,
          c("ID","Title","var.3")
          ]
        ),
      ]

  output.test <-
    structure(
      output,
      class="data.frame",
      .typeOf=c(
        "character","character","character",
        "character","numeric","Date",
        "Date","Date","Date",
        "Date","Date","character",
        "Date","character","character",
        "character"
        )
      )


  readr::write_excel_csv(
    output,na = ".",
    file = sprintf(
      "%s/%s",
      dir.output,
      fn.output
    )#,
    # fileEncoding = "CP932",
    # na = "."
  )
  write.csv(
    df.commented,
    file = sprintf(
      "%s/%s",
      dir.output,
      "test_df.commented.csv"
    ),
    fileEncoding = "CP932",
    na = "."
    )
  write.csv(
    data_record,
    file = sprintf(
      "%s/%s",
      dir.output,
      "test_data_record.csv"
    ),
    fileEncoding = "CP932",
    na = "."
    )

  return(list(output, data_raw))
}

