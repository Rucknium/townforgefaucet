


serverFaucet <- function(input, output, session) {
  
  Sys.setenv(TZ = "UTC")
  thematic::thematic_shiny(font = "auto")
             
  beowulf.txt <- xml2::read_xml("data/Perseus_text_2003.01.0003.xml")
  beowulf.txt <- xml2::xml_text(xml2::xml_find_all(beowulf.txt, ".//l"))
  
  passage.num.lines <- 6
  
  csv.files <- c("undispensed-invitations.csv", "dispensed-invitations.csv", "recipient-ip-addresses.csv")
  for (i in csv.files) {
    if ( ! any(list.files(recursive = TRUE) == paste0("data/", i))) {
      file.copy(paste0("data/templates/", i), paste0("data/", i))
    }
  }
  
  
  
  light <- bslib::bs_theme(bootswatch = "cosmo")
  dark <- bslib::bs_theme(bootswatch = "cosmo", bg = "black", fg = "white", primary = "purple")
  session$setCurrentTheme(dark)
  shiny::observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) {dark} else {light}
  ))
  # https://rstudio.github.io/bslib/articles/bslib.html#dynamic-theming
  
  passage.start <- sample(length(beowulf.txt) - (passage.num.lines - 1), 1)
  # passage.start <- which.max(nchar(beowulf.txt))
  # Test for text clipping
  
  passage <- beowulf.txt[passage.start:(passage.start + (passage.num.lines - 1))]
  
  session.vars <- shiny::reactiveValues(already.dispensed = FALSE)
  
  # output$passage_verification_display <- renderText( {list.files(recursive = TRUE)})
  # output$passage_verification_display <- renderText( {"TEST"})
  
  IP <- reactive({ input$getIP })
  
  output$passage_verification_display <- renderText( {
    if (length(IP()$ip) == 0) {
      "WARNING: Failed to obtain IP address. Requesting an invitation code will fail. The most likely cause is using TOR. VPNs should work."
    } else {
      ""
    } })
  
  
  output$passage_image <- shiny::renderPlot({
    
    observe(input$dark_mode)
    
    par(mar = rep(0, 4))
    plot(0, 0, type = "n", ylim = c((-1) * (passage.num.lines + 1), 0), xlim = c(0, 10),
      axes = FALSE, ylab = "", xlab = "")
    text(x = rep(0, passage.num.lines), y = (-1) * (1:passage.num.lines), 
      labels = passage, pos = 4, family = "mono", cex = 1.5)
    
  })
  
  shiny::observeEvent(input$submit_passage, {
    shiny::withProgress(message = "Submission in progress...", {
      
      Sys.sleep(5)
      
      if (isolate(input$submit_passage) > 5) {
        output$passage_verification_display <- renderText( {"Too many attempts."})
        return()
      }
      
      if (isolate(session.vars$already.dispensed)) {
        output$passage_verification_display <- renderText( {"Already dispensed an invitation code this session."})
        return()
      }
      
      if (tolower(gsub("[^[:alpha:]]", "", paste0(passage, collapse = ""))) == 
          tolower(gsub("[^[:alpha:]]", "", isolate(input$user_passage_input))) ) {
        # TODO: maybe instead of exact comparison, do a comparison of edit distance
        
        ip.addresses.df <- read.csv("data/recipient-ip-addresses.csv", stringsAsFactors = FALSE,
          colClasses = c("character", class(Sys.time())[1] ))
        write.csv(unique(ip.addresses.df[(ip.addresses.df$time.dispensed + 60 * 60 * 24) > Sys.time(), , drop = FALSE]), 
          "data/recipient-ip-addresses.csv", row.names = FALSE)
        # Purge IP addresses if they have been around for more than 24 hours
        # TODO: This is not a cron job, so only runs when someone triggers a submission. Maybe turn it into a cron job.
        # TODO: Specify colClasses for other read.csv() uses
        
        user.ip.address <- isolate(IP()$ip)
        
        if (length(user.ip.address) == 0) { 
          output$passage_verification_display <- renderText( {
            "Failed to obtain IP address. IP addresses are logged to prevent overuse of the service. The most likely cause is using TOR. VPNs should work."})
          return()
        }
        
        most.recent.time.dispensed <- max(ip.addresses.df$time.dispensed[ip.addresses.df$ip.address == user.ip.address])
        
        if ( is.finite(most.recent.time.dispensed) && (most.recent.time.dispensed + 60 * 60 * 24) > Sys.time() ) {
          output$passage_verification_display <- renderText( {
            wait.time <- most.recent.time.dispensed + 60 * 60 * 24 - Sys.time()
            paste0("Only one invitation code per 24 hours per user.\nWait for ", 
              round(wait.time, 1), " more ", attr(wait.time, "units"), " before trying again." )
          })
          return()
        }
        
        inv.codes.df <- read.csv("data/undispensed-invitations.csv", stringsAsFactors = FALSE)
        
        if (nrow(inv.codes.df) == 0) {
          output$passage_verification_display <- renderText( {"Invitation pool currently empty. Try again in a few hours or days."})
          return()
        }
        
        inv.codes.df <- inv.codes.df[sample(nrow(inv.codes.df)), , drop = FALSE]
        inv.code.to.display.index <- sample(nrow(inv.codes.df), 1)
        inv.code.to.display.txt <- inv.codes.df[inv.code.to.display.index, "invitation.code"]
        
        dispensed.inv.codes.df <- read.csv("data/undispensed-invitations.csv", stringsAsFactors = FALSE)
        write.csv(unique(rbind(dispensed.inv.codes.df, inv.codes.df[inv.code.to.display.index, , drop = FALSE])), 
          "data/dispensed-invitations.csv", row.names = FALSE)
        
        inv.codes.df <- inv.codes.df[ (-1) * inv.code.to.display.index, , drop = FALSE]
        write.csv(unique(inv.codes.df), "data/undispensed-invitations.csv", row.names = FALSE)
        
        ip.addresses.df <- read.csv("data/recipient-ip-addresses.csv", stringsAsFactors = FALSE,
          colClasses = c("character", class(Sys.time())[1] ))
        ip.addresses.df <- rbind(ip.addresses.df, 
          data.frame(ip.address = user.ip.address, time.dispensed = Sys.time(), stringsAsFactors = FALSE) )
        write.csv(unique(ip.addresses.df), "data/recipient-ip-addresses.csv", row.names = FALSE)
        
        session.vars$already.dispensed <- TRUE
        
        output$passage_verification_display <- renderText( {
          gsub("|", "\n", inv.code.to.display.txt, fixed = TRUE)
        })
        
      } else {
        output$passage_verification_display <- renderText( {"Input does not match text."})
      }
    })
  })
  
  shiny::observeEvent(input$submit_invitation, {
    shiny::withProgress(message = "Submission in progress...", {
      
      Sys.sleep(5)
      
      invitation.input.txt <- isolate(input$user_invitation_input)
      invitation.input.txt <- gsub("(^[^[:alnum:]]+)|([^[:alnum:]]+$)", "", invitation.input.txt)
      # Trim whitespace before and after
      invitation.input.txt <- gsub("[^[:alnum:]]*\n[^[:alnum:]]*", "\n", invitation.input.txt)
      # Sanitize any issue with paragraph returns

      
      if (TRUE) { # add validation of invitation.input.txt and check if it has been used
        
        invitation.input.txt <- gsub("[^[:alnum:]]*\n[^[:alnum:]]*", "|", invitation.input.txt)
        
        inv.codes.df <- read.csv("data/undispensed-invitations.csv", stringsAsFactors = FALSE)
        inv.codes.df <- rbind(inv.codes.df, data.frame(invitation.code = invitation.input.txt, stringsAsFactors = FALSE) )
        write.csv(unique(inv.codes.df), "data/undispensed-invitations.csv", row.names = FALSE)
        
        output$invitation_submission_display <- shiny::renderText( {
          "Invitation successfully submitted to pool."
        })
        
      } else {
        output$invitation_submission_display <- shiny::renderText( {"Invitation is not valid."})
      }
    })
  })
  
}
