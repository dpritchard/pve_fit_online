library(shiny)

# Define server logic
shinyServer(function(input, output) {
    make_plot <- function(dat, newx, guess, mod_pred=NULL){
        par(mar=c(5, 5, 0, 10))
        plot(dat$E, dat$P, pch=19, axes=F, xlab = "Irradiance (E)", ylab = "Photosynthesis (P)")
        abline(h=0, lty=3, col="grey50")
        box()
        axis(1, lwd=0, lwd.ticks = 1, las=1)
        axis(2, lwd=0, lwd.ticks = 1, las=1)
        lines(newx, guess, lty=2, col="blue4")
        leg <- c("Measured", "Inital Guess")
        legcols <- c(1, "blue4")
        legpch <- c(19, NA)
        leglty <- c(NA, 2)
        
        if(!all(is.null(mod_pred))){
            lines(newx, mod_pred, lty=1, col="firebrick4")
            leg <- c(leg, "Optimised Model")
            legcols <- c(legcols, "firebrick4")
            leglty <- c(leglty, 1)
            legpch <- c(legpch, NA)
        }
        
        legend(max(dat$E)*1.05, max(dat$P), legend = leg,
               pch=legpch, lty=leglty, col=legcols, xpd=TRUE, bty="n")
    }
    
    tanh_fit <- function(model_coefs, newx){
        
        Pmax <- model_coefs$Pmax
        alpha <- model_coefs$alpha
        Rd <- model_coefs$Rd
        out <- Pmax*tanh(alpha*newx/Pmax)+Rd
        return(out)
    }
    
    linear_fit <- function(model_coefs, newx){
        slope <- model_coefs$slope
        Rd <- model_coefs$Rd
        out <- slope*newx+Rd
        return(out)
    }
    
    get_dat <- function(raw_text, has_header) {
        if(nchar(raw_text) > 0){
            con <- textConnection(raw_text)
            dat <- read.table(con, sep = "\t", header=has_header)
            close(con)
            #print(str(dat))
            names(dat) <- c("E", "P")
            return(dat)
        }
    }
    
    fit_model <- function(mod_form, start, dat){
        fit <- try(nls(mod_form, start=start, data=dat, 
                       nls.control(minFactor=1/100000, maxiter=500)), 
                   silent=T)
    }
    
    output$coef_guess_ui <- renderUI({
        if (is.null(input$model_type)) { return() }
        # Depending on input$model_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$model_type,
               "tanh" = list(sliderInput("alpha", "alpha:", min = 0, max = 10, value = 1, step = 0.01),
                             sliderInput("Pmax", "Pmax:", min = 0, max = 200, value = 100),
                             sliderInput("Rd", "Rd:", min = -50, max = 0, value = 0)),
               "linear" = list(sliderInput("slope", "slope:", min = 0, max = 10, value = 1, step = 0.01),
                               sliderInput("Rd", "Rd:", min = -50, max = 0, value = 0))
        )
    })
    
    output$model_formula <- renderUI({
        if (is.null(input$model_type)) { return() }
        # Depending on input$model_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$model_type,
               "tanh" = withMathJax(helpText("$$P = P_{max} \\times tanh(alpha\\times\\frac{E}{P_{max}})+R_d$$")),
               "linear" = withMathJax(helpText("$$P = slope \\times E +R_d$$"))
        )
    })
    
    output$model_notes <- renderUI({
        if (is.null(input$model_type)) { return() }
        # Depending on input$model_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$model_type,
               "tanh" = helpText("Notes on the tanh model"),
               "linear" = helpText("Notes on the linear model")
        )
    })
    
    output$dat_plot <- renderPlot({
        dat <- get_dat(input$raw_dat, input$has_header)
        # if(!is.null(dat)){
        #    if(!is.numeric(dat$E) | !is.numeric(dat$P)){
        #        stop("E and P must be numbers. Are you sure your data don't have a header row?")
        #    }
        # }
        if(is.data.frame(dat)){
            model_func <- switch(input$model_type,
                                 "tanh" = tanh_fit,
                                 "linear" = linear_fit
            )
            model_coefs <- switch(input$model_type,
                                  "tanh" = list("Pmax" = input$Pmax, "alpha" = input$alpha, "Rd" = input$Rd),
                                  "linear" = list("slope" = input$slope, "Rd" = input$Rd)
            )
            newx <- seq(from=min(dat$E), to=max(dat$E), length.out = 100)
            guess <- model_func(model_coefs, newx)
            mod_pred <- NULL
            if(input$optimise){
                mod_form <- switch(input$model_type,
                        "tanh" = formula(P~Pmax*tanh(alpha*E/Pmax)+Rd),
                        "linear" = formula(P~slope*E+Rd)
                )
                fit <- fit_model(mod_form, start=model_coefs, dat=dat)
                if(inherits(fit, "try-error") & input$optimise){ 
                    outmsg <- paste0("Sorry, but the fit failed.<br>", 
                                     "The error was: <code>", attr(fit, "condition")$message, "</code><br>",
                                     "It might be possible to fit the curve with some different starting values. ",
                                     "However, sometimes it is not possible to fit models to some PvE data. ",
                                     "Sometimes, the only solution is to choose a different (often simpler) model, or collect more data (or pool several datasets).")
                    output$fit_print_caption <- renderText("")
                    output$fit_print <- renderText(outmsg)
                } else if(input$optimise) {
                    optim_coefs <- as.list(coefficients(fit))
                    mod_pred <- model_func(optim_coefs, newx)
                    outtab <- t(summary(fit)$coefficients[,1:2])
                    output$fit_print <- renderTable(outtab)
                } else {
                    output$fit_print_caption <- ""
                    output$fit_print <- ""
                }
            }
            make_plot(dat, newx, guess, mod_pred)
        }
    })
    
    output$dat_print <- renderTable({ if(nchar(input$raw_dat)){ t(get_dat()$dat) }})
})