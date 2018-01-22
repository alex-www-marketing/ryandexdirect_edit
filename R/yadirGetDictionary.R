yadirGetDictionary <- function(DictionaryName = "GeoRegions", Language = "ru", login = NULL, token = NULL){
  #–ü—Ä–æ–≤–µ—Ä–∫–∞ –Ω–∞–ª–∏—á–∏—è –ª–æ–≥–∏–Ω–∞ –∏ —Ç–æ–∫–µ–Ω–∞
  if(is.null(login)|is.null(token)) {
    stop("You must enter login and API token!")
  }
  
  #–ü—Ä–æ–≤–µ—Ä–∫–∞ –≤–µ—Ä–Ω–æ –ª–∏ —É–∫–∞–∑–∞–Ω–æ –Ω–∞–∑–≤–∞–Ω–∏–µ —Å–ø—Ä–∞–≤–æ—á–Ω–∏–∫–∞
  if(!DictionaryName %in% c("Currencies",
                            "MetroStations",
                            "GeoRegions",
                            "TimeZones",
                            "Constants",
                            "AdCategories",
                            "OperationSystemVersions",
                            "ProductivityAssertions",
                            "SupplySidePlatforms",
                            "Interests")){
    stop("Error in DictionaryName, select one of Currencies, MetroStations, GeoRegions, TimeZones, Constants, AdCategories, OperationSystemVersions, ProductivityAssertions, SupplySidePlatforms, Interests")
  }

#check stringAsFactor
factor_change <- FALSE

#change string is factor if TRUE
if(getOption("stringsAsFactors")){
  options(stringsAsFactors = F)
  factor_change <- TRUE
}
  
  queryBody <- paste0("{
                      \"method\": \"get\",
                      \"params\": {
                      \"DictionaryNames\": [ \"",DictionaryName,"\" ]
}
}")
  
  #–û—Ç–ø—Ä–∞–≤–∫–∞ –∑–∞–ø—Ä–æ—Å–∞ –Ω–∞ —Å–µ—Ä–≤–µ—Ä
  answer <- POST("https://api.direct.yandex.com/json/v5/dictionaries", body = queryBody, add_headers(Authorization = paste0("Bearer ",token), 'Accept-Language' = Language, "Client-Login" = login[1]))
  #–ü—Ä–æ–≤–µ—Ä–∫–∞ —Ä–µ–∑—É–ª—å—Ç–∞—Ç–∞ –Ω–∞ –æ—à–∏–±–∫–∏
  stop_for_status(answer)
  
  dataRaw <- content(answer, "parsed", "application/json")
  
  if(length(dataRaw$error) > 0){
    stop(paste0(dataRaw$error$error_string, " - ", dataRaw$error$error_detail))
  }
  
  #–ü—Ä–µ–æ–±—Ä–∞–∑—É–µ–º –æ—Ç–≤–µ—Ç –≤ data frame
  
  #–ü–∞—Ä—Å–∏–Ω–≥ —Å–ø—Ä–∞–≤–æ—á–Ω–∏–∫–∞ —Ä–µ–≥–∏–æ–Ω–æ–≤
  if(DictionaryName == "GeoRegions"){
  dictionary_df <- data.frame()

  for(dr in 1:length(dataRaw$result[[1]])){
    dictionary_df_temp <- data.frame(GeoRegionId = dataRaw$result[[1]][[dr]]$GeoRegionId,
                                     ParentId = ifelse(is.null(dataRaw$result[[1]][[dr]]$ParentId),NA , dataRaw$result[[1]][[dr]]$ParentId),
                                     GeoRegionType = dataRaw$result[[1]][[dr]]$GeoRegionType,
                                     GeoRegionName = dataRaw$result[[1]][[dr]]$GeoRegionName)
    dictionary_df <- rbind(dictionary_df, dictionary_df_temp)
    
  }}

  #–ü–∞—Ä—Å–∏–Ω–≥ —Å–ø—Ä–∞–≤–æ—á–Ω–∏–∫–∞ –≤–∞–ª—é—Ç
  if(DictionaryName == "Currencies"){
    dictionary_df <- data.frame()
  for(dr in 1:length(dataRaw$result[[1]])){
    dictionary_df_temp <- data.frame(Cur = dataRaw$result[[1]][[dr]]$Currency, as.data.frame(do.call(rbind.data.frame, dataRaw$result[[1]][[dr]]$Properties), row.names = NULL, stringsAsFactors = F))
    dictionary_df <- rbind(dictionary_df, dictionary_df_temp)
  }
    dictionary_df_cur <- data.frame()
    #–ü—Ä–µ–æ–±—Ä–∞–∑—É–µ–º —Å–ø—Ä–∞–≤–æ—á–Ω–∏–∫ –≤–∞–ª—é—Ç
    for(curlist in unique(dictionary_df$Cur)){
      dictionary_df_temp <- data.frame(Cur = curlist,
                                       FullName = dictionary_df[dictionary_df$Cur == curlist & dictionary_df$Name == "FullName",3],
                                       Rate = dictionary_df[dictionary_df$Cur == curlist & dictionary_df$Name == "Rate",3],
                                       RateWithVAT = dictionary_df[dictionary_df$Cur == curlist & dictionary_df$Name == "RateWithVAT",3])
      dictionary_df_cur <- rbind(dictionary_df_cur,dictionary_df_temp)
    }
    dictionary_df <- dictionary_df_cur
  }
  
  #–ü–∞—Ä—Å–∏–Ω–≥ —Å–ø—Ä–∞–≤–æ—á–Ω–∏–∫–∞ Interests
  if(DictionaryName == "Interests"){
    dictionary_df <- data.frame()
    for(dr in 1:length(dataRaw$result[[1]])){
      dictionary_df_temp <- data.frame(Name = dataRaw$result[[1]][[dr]]$Name,
                                       ParentId = ifelse(is.null(dataRaw$result[[1]][[dr]]$ParentId),NA , dataRaw$result[[1]][[dr]]$ParentId),
                                       InterestId = dataRaw$result[[1]][[dr]]$InterestId,
                                       IsTargetable = dataRaw$result[[1]][[dr]]$IsTargetable)
      dictionary_df <- rbind(dictionary_df, dictionary_df_temp)
    }
  }
  
  #–ü–∞—Ä—Å–∏–Ω–≥ –æ—Å—Ç–∞–ª—å–Ω—ã—Ö —Å–ø—Ä–∞–≤–æ—á–Ω–∏–∫–æ–≤ —Å–æ —Å—Ç–∞–Ω–¥–∞—Ä—Ç–Ω–æ–π —Å—Ç—Ä—É–∫—Ç—É—Ä–æ–π
  if(! DictionaryName %in% c("Currencies","GeoRegions","Interests")){
    dictionary_df <- do.call(rbind.data.frame, dataRaw$result[[1]])
    }
  
  #back string as factor value
  if(factor_change){
  options(stringsAsFactors = T)
  }
  #–í—ã–≤–æ–¥–∏–º –∏–Ω—Ñ–æ—Ä–º–∞—Ü–∏—é –æ —Ä–∞–±–æ—Ç–µ –∑–∞–ø—Ä–æ—Å–∞ –∏ –æ –∫–æ–ª–∏—á–µ—Å—Ç–≤–µ –±–∞–ª–ª–æ–≤
   packageStartupMessage("—Ô‡‚Ó˜ÌËÍ ÛÒÔÂ¯ÌÓ Á‡„ÛÊÂÌ!", appendLF = T)
   packageStartupMessage(paste0("¡‡ÎÎ˚ ÒÔËÒ‡Ì˚ Ò : " ,answer$headers$`units-used-login`), appendLF = T)
   packageStartupMessage(paste0(" -‚Ó ·‡ÎÎÓ‚ ËÁ‡ÒıÓ‰Ó‚‡Ì˚ı ÔË ‚˚ÔÓÎÌÂÌËË Á‡ÔÓÒ‡: " ,strsplit(answer$headers$units, "/")[[1]][1]), appendLF = T)
   packageStartupMessage(paste0("ƒÓÒÚÛÔÌ˚È ÓÒÚ‡ÚÓÍ ÒÛÚÓ˜ÌÓ„Ó ÎËÏËÚ‡ ·‡ÎÎÓ‚: " ,strsplit(answer$headers$units, "/")[[1]][2]), appendLF = T)
   packageStartupMessage(paste0("—ÛÚÓ˜Ì˚È ÎËÏËÚ ·‡ÎÎÓ‚: " ,strsplit(answer$headers$units, "/")[[1]][3]), appendLF = T)
   packageStartupMessage(paste0("”ÌËÍ‡Î¸Ì˚È Ë‰ÂÌÚËÙËÍ‡ÚÓ Á‡ÔÓÒ‡ ÍÓÚÓ˚È ÌÂÓ·ıÓ‰ËÏÓ ÛÍ‡Á˚‚‡Ú¸ ÔË Ó·‡˘ÂÌËË ‚ ÒÎÛÊ·Û ÔÓ‰‰ÂÊÍË: ",answer$headers$requestid), appendLF = T)
  
  #–í–æ–∑–≤—Ä–∞—â–∞–µ–º —Ä–µ–∑—É–ª—å—Ç–∞—Ç –≤ –≤–∏–¥–µ Data Frame
  return(dictionary_df)
}

