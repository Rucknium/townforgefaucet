source("ui.R", local = TRUE)
source("server.R", local = TRUE)
# local = TRUE due to https://shiny.rstudio.com/articles/scoping.html

# csv.files <- c("undispensed-invitations.csv", "dispensed-invitations.csv", "recipient-ip-addresses.csv")
# for (i in csv.files) {
#   if ( ! any(list.files(recursive = TRUE) == paste0("data/", i))) {
#     file.copy(paste0("data/templates/", i), paste0("data/", i))
#   }
# }
# 
# Sys.setenv(TZ = "UTC")
# thematic::thematic_shiny(font = "auto")
# ^ This is crucial to make the dark mode work for the plots

shiny::shinyApp(
  ui = uiFaucet,
  server = serverFaucet
)
