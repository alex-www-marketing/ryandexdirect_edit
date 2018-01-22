yadirGetToken <-
function(){
  browseURL("https://oauth.yandex.ru/authorize?response_type=token&client_id=c441fa0194f04f4ea2c238c0c2c40ec9")
  token <- readline(prompt = "Enter your token: ")
  return(token)
}
