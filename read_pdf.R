library(pdftools)
# indices = 0:366
# indices = indices[!indices == 239]
# indices = indices[!indices == 241]
# filenames_evidence_scopus = paste0("/Users/chenyuyan/PDF/evidence_scopus/",indices, ".pdf")
# filenames_evidence_wos =  paste0("/Users/chenyuyan/PDF/evidence_wos/",0:18, ".pdf")


fn_all = paste0("/Users/chenyuyan/Downloads/gr_canada/gr_canada", 1:36, ".pdf")
# fn_all = c(filenames_evidence_scopus, filenames_evidence_wos)

#indices = 0:32 
#indices = indices[!indices == 27]
#indices = indices[!indices == 28]
#indices = indices[!indices == 30]

#fn_review = paste0('/Users/chenyuyan/PDF/review/', 0:21, '.pdf')
#fn_review_scopus = paste0('/Users/chenyuyan/PDF/review_scopus/', indices, '.pdf')
#fn_review_wos = paste0('/Users/chenyuyan/PDF/review_wos/', 0:5, '.pdf')

#fn_all_review = c(fn_review, fn_review_scopus)
#fn_all_review = c(fn_all_review, fn_review_wos)
i = 0
all_text = c()
for (f in fn_all){
  print(f)
  txt <- tryCatch(pdftools::pdf_text(f))
  all_text = c(all_text, toString(txt))
}

text.data = data.frame(1:36, all_text)
write.csv(text.data, "gr_canada.csv", row.names = FALSE)

#install.packages("superml")
#library(superml)
#cfv <- CountVectorizer$new(max_features = 10, remove_stopwords = TRUE, ngram_range = c(1, 2))
#cf_mat <- cfv$fit_transform(all_text)
