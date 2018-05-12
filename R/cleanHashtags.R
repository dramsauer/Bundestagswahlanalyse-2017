cleanHashtags<-function(hashtag){
require(stringr)
require(stringi)
          
  hashtag<-str_replace_all(hashtag,pattern="https",replacement = "")
  hashtag<-str_replace_all(hashtag,pattern="#",replacement="")
  hashtag<-stri_replace_all_fixed(hashtag, 
                            c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
                            c("ae","oe","ue","Ae","Oe","Ue"), 
                            vectorize_all = FALSE)
  hashtag<-stri_trans_general(hashtag, "latin-ascii")
  hashtag<-str_to_lower(hashtag)
  return(hashtag)
}