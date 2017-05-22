library("rtweet")
billmeet <- search_tweets(q = "#BillMeetScienceTwitter", n = 18000, type = "recent")
billmeet <- unique(billmeet)

billmeet <- dplyr::filter(billmeet, lang == "en")
billmeet <- dplyr::filter(billmeet, is_retweet == FALSE)

output <- monkeylearn::monkeylearn_classify(request = billmeet$text, key= "ee9f5e9858e6bf4221abc15fbfa0110549c5909f",
                                            classifier_id = "cl_5icAVzKR")
str(output)

billmeet <- dplyr::mutate(billmeet, text_md5 = vapply(X = text,
                                                      FUN = digest::digest,
                                                      FUN.VALUE = character(1),
                                                      USE.NAMES = FALSE,
                                                      algo = "md5"))
billmeet <- dplyr::select(billmeet, text, text_md5)
output <- dplyr::left_join(output, billmeet, by = "text_md5")

output <- dplyr::filter(output, probability > 0.5)


dplyr::group_by(output) %>%
  dplyr::summarise(nlabels = n()) %>%
  dplyr::group_by(nlabels) %>%
  dplyr::summarise(n_tweets = n()) %>%
  knitr::kable()


library("ggplot2")
library("viridis")

label_counts <- output %>% 
  dplyr::filter(label != "Internet") %>%
  dplyr::group_by(label) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n))

label_counts <- label_counts %>%
  dplyr::mutate(label = ifelse(n < 5, "others", label)) %>%
  dplyr::group_by(label) %>%
  dplyr::summarize(n = sum(n)) %>%
  dplyr::arrange(desc(n))

label_counts <- dplyr::mutate(label_counts,
                              label = factor(label,
                                             ordered = TRUE,
                                             levels = unique(label)))

ggplot(label_counts) +
  geom_bar(aes(label, n, fill = label), stat = "identity")+
  scale_fill_viridis(discrete = TRUE, option = "plasma")+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 1),
        text = element_text(size=25),
        legend.position = "none")


####TRUMP

billmeet <- search_tweets(q = "@POTUS", n = 1000, type = "recent")
billmeet <- unique(billmeet)

billmeet <- dplyr::filter(billmeet, lang == "en")
billmeet <- dplyr::filter(billmeet, is_retweet == FALSE)

output <- monkeylearn::monkeylearn_classify(request = billmeet$text, key= "ee9f5e9858e6bf4221abc15fbfa0110549c5909f",
                                            classifier_id = "cl_qkjxv9Ly")
str(output)

billmeet <- dplyr::mutate(billmeet, text_md5 = vapply(X = text,
                                                      FUN = digest::digest,
                                                      FUN.VALUE = character(1),
                                                      USE.NAMES = FALSE,
                                                      algo = "md5"))
billmeet <- dplyr::select(billmeet, text, text_md5)
output <- dplyr::left_join(output, billmeet, by = "text_md5")

output <- dplyr::filter(output, probability > 0.5)
