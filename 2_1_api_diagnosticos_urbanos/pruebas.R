  
library(dplyr)
library(stringr)
library(seewave)
library(tuneR)

pr <- readRDS("..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")
audio <- readWave("..\\data/01_raw/Prueba2.m4a")



a0_pobreza = data.frame(
  barrio = sample(letters[1:4]),,
  pp_de_pobres = sample()
)


play(audio)
plot(audio)
spectro(audio)


i0 <- openai::create_chat_completion(
  model = "gpt-3.5-turbo",
    #"gpt-4o-mini",
  messages = list(
    list("role" = "system",
         "content" = "responde normal"),
    list("role" = "user",
         "content" = "Estoy usando la libreria de openai::create_chat_completion()
         para comunicarme contigo, pero no se como usarte para que no te olvides
         de mis comentarios anteriores y así generar textos más largos pero
         fluidos")),
  openai_api_key =ai_key)


i1 <- openai::create_transcription(
  file = "..\\data/01_raw/Prueba2.m4a", 
  model = "whisper-1", 
  openai_api_key = ai_key)



i1$text
i0$choices$message.content %>% 
  stringr::str_view(html = T)
  




