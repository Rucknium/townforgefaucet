

uiFaucet <- function(req) {
  # Structure it like function(req) because doing https://github.com/rstudio/shiny/issues/141#issuecomment-854707910
  shiny::navbarPage("Townforge Invitation Faucet", #  paste("TownforgeR", gsub("`|´", "", packageVersion("TownforgeR"))),
    #collapsible = TRUE,
    theme = bslib::bs_theme(bootswatch = "cosmo", bg = "black", fg = "white", primary = "purple"),
    #"----", # "If the string is a set of dashes like "----" a horizontal separator will be displayed in the menu.
    shiny::tabPanel(shiny::HTML("<u>Receive invitations</u>"),
      shiny::fluidRow(
        shiny::column(12,
          shiny::h5(tags$span(style = "color:red", "NOTICE: All invitations are for the Townforge testnet")),
          shiny::plotOutput("passage_image", width = "600px", height = "150px"),
          shiny::textAreaInput("user_passage_input", "Type the passage displayed above", width = "100%", height = "200px"),
          shiny::HTML("Note: Input verification is insensitive to case, punctuation, and spacing."),
          shiny::br(),
          shiny::actionButton("submit_passage", "Submit"),
          shiny::br(),
          shiny::HTML("Invitation code consists of all five lines:"),
          shiny::br(),
          shiny::verbatimTextOutput("passage_verification_display"),
          shiny::br(),
          shiny::checkboxInput("dark_mode", "Dark mode", value = TRUE),
          shiny::br(),
          shiny::HTML("Verification text licensed by Perseus Digital Library under a Creative Commons Attribution-ShareAlike 3.0 United States License."),
          shiny::br(),
          shiny::HTML("Privacy policy: If you receive an invitation code, your IP address will be sent to https://ipinfo.io and logged for 24 hours to prevent overuse of the service."),
          # shiny::div(style = "display: none;",
          #   shiny::textInput("remote_addr", "remote_addr",
          #     if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
          #       req[["HTTP_X_FORWARDED_FOR"]]
          #     else
          #       req[["REMOTE_ADDR"]]
          #   )
          # ), 
          # To get IP address. See https://github.com/rstudio/shiny/issues/141#issuecomment-854707910
          
          tags$head(
            tags$script(src = "getIP.js")
          )
          # See https://stackoverflow.com/questions/43888099/get-user-ip-in-shiny
          # NOTE: This service has a rate limit of 1k requests per day: https://ipinfo.io/missingauth
        )
      )
    ),
    shiny::tabPanel(shiny::HTML("<u>Contribute invitations</u>"),
      shiny::fluidRow(
        shiny::column(12,
          # shiny::h4(tags$span(style = "color:red", "NOTICE: Invitation codes are not yet checked for validity, so please ensure your submissions are valid.")),
          shiny::textAreaInput("user_invitation_input", "Submit five-line invitation code", width = "100%", height = "200px"),
          shiny::actionButton("submit_invitation", "Submit"),
          shiny::verbatimTextOutput("invitation_submission_display"),
        )
      )
    )
  )
}





