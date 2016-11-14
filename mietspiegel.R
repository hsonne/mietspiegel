if (FALSE)
{
  file.pdf <- preparePdf()
  plot_mieten(spanne = c(4.40, 5.62, 7.52), 
              "Mietspiegelfeld 'G1'\nkein Sondermerkmal")
  
  plot_mieten(spanne = c(4.40, 5.62, 7.52), 
              "Mietspiegelfeld 'G1'\nSondermerkmal 'Bad'", 
              offset = 0.34)
  
  plot_mieten(spanne = c(4.40, 5.62, 7.52), 
              "Mietspiegelfeld 'G1'\nSondermerkmale 'Bad', 'Parkett'", 
              offset = 0.34 + 0.56)
  finishAndShowPdf(file.pdf)
}

# plot_mieten ------------------------------------------------------------------
plot_mieten <- function(spanne, main, offset = 0)
{
  steps <- lapply(c(neg = 3, pos = 1), function(exclude) diff(spanne[-exclude])/5)
  
  plot(
    NA, 
    xlim = c(0, 6), 
    ylim = spanne[-2] + offset, 
    main = main,
    xlab = "Merkmalsgruppe", 
    ylab = "Nettokaltmiete in EUR/m2",
    las = 1
  )
  
  h <- numeric()
  
  for (i in 0:4) {
    
    ystart <- spanne[2] + offset
    
    yp <- ystart - i * steps$neg
    yn <- ystart + i * steps$pos
    
    x0 <- i:4
    x1 <- (i+1):5
    
    n0 <- 0:(4-i)
    n1 <- 1:(5-i)
    
    y0p <- yp + n0 * steps$pos
    y1p <- yp + n1 * steps$pos
    
    y0n <- yn - n0 * steps$neg
    y1n <- yn - n1 * steps$neg
    
    arrows(x0 = x0, y0 = y0p, x1 = x1, y1 = y1p, angle = 15)
    arrows(x0 = x0, y0 = y0n, x1 = x1, y1 = y1n, angle = 15)
    
    h <- unique(c(h, y0p, y1p, y0n, y1n))
  }
  
  h <- sort(h)
  abline(h = h, lty = 3)
  x <- rep(6, length(h))
  x[isEvenNumber(seq_along(x))] <- 5.5
  text(x, h, adj = 1, sprintf("%0.2f", h))
}
  
