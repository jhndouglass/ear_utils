csf_rect <- function(line.loc, labels, Title, det, limit = NA,
                     useLimit = FALSE, ...){
        require(plotrix)
        if (class(Title) != "character") {stop("Title must be character")}
        if (useLimit == FALSE) {limit <- NA}
        if (useLimit == TRUE && is.na(limit)) {limit <- 0}
        if (!det %in% c("Ammoniacal Nitrogen", "Orthophosphate",
                        "Total Phosphorus", "TON", "Suspended Solids")){
                stop("invalid 'det' string")
        }
        col <- switch(det, "Ammoniacal Nitrogen" = "#bd0026",
                      "Orthophosphate" = "#253494",
                      "Total Phosphorus" = "#006837",
                      "TON" = "#993404", "Suspended Solids" = "#7a0177")
        if (length(line.loc) != length(labels)) {
                stop("'lines' and 'labels' must be vectors of equal length")
        }
        if (class(line.loc) != "numeric" && class(labels) != "character"){
                stop("'lines' must of class 'numeric' and 'labels' of class
                     'character'")
        }
        #base plot
        bounds <- list(x1 = 0.1, x2 = 0.3, y1 = 0, y2 = 1)
        RectPlot(Title, col, bounds, ...)
        # plot details
        if (useLimit == TRUE){
                line.loc <- as.numeric(c(line.loc, limit))
                labels <- c(labels, "User limit")
        }
        high <- max(line.loc, na.rm = TRUE)
        scale <- 0.95
        plot.loc <- (line.loc - 0)/(high - 0) * scale
        for (i in 1:length(plot.loc)){
                lines(x = c(bounds[[1]], bounds[[2]]),
                      y = c(plot.loc[i], plot.loc[i]),
                      lwd = 1, col = "black")
        }
        lab.disp <- paste(round(line.loc, 3), labels)
        spread.labels.simple(x = rep((bounds[[2]]), length(plot.loc)),
                             y = as.numeric(plot.loc), labels = lab.disp)
}

RectPlot <- function(Title, col, bounds, ...) {
        plot.new()
        title(main = Title)
        x1 <- bounds[[1]]
        x2 <- bounds[[2]]
        y1 <- bounds[[3]]
        y2 <- bounds[[4]]
        gradient.rect(x1, y1, x2, y2, col = smoothColors("gray95", 999, col),
                      border = NA, gradient = "y")
        text(y = y1 + 0.02, x = x1 - 0.05,
             labels = expression(paste("0 mgl" ^-1)), ...)
        text(y = 0.5, x = x1 - 0.1,
             labels = expression(paste("Increasing concentration",
                                       "  (mgl" ^-1, ")")), srt = 90, ...)
        arrows(y0 = 0.2, x0 = x1 - 0.02, y1 = 0.8, length = 0.1, lwd = 1.5, ...)
}

spread.labels.simple <- function (x, y, labels = NULL, linecol = par("fg"),
                                  srt = 0, ...) {
        if (missing(x))
                stop("Usage: spread.labels(x,y,labels,...)")
        nx <- length(x)
        ny <- length(y)

        sort.index <- sort.list(y)
        x <- x[sort.index]
        y <- y[sort.index]
        newy <- seq(y[1], y[ny], length = length(labels))

        offsets <- rep(0.15, nx)

        segments(x + offsets, newy, x, y, col = "black", lwd = 1, lty = 3)
        text(x + offsets, newy, labels[sort.index], srt = srt,
             pos = 4, ...)

}