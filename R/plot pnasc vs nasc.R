#' plot_pnasc_vs_nasc
#'@description
#' creates a plot showing relationship between pnasc and log(nasc)
#'
#'
#' @return
#' stores a pdf file "pnascvsnasc.pdf" in graphics folder
#'
#'
#'
#' @export
#'
#' @examples
#'
plot_pnasc_vs_nasc <- function() {
thedata <- load_zoop_data()

# check to see if graphics directory exists in current directory
cur.dir <- dir()
if (!"graphics" %in% cur.dir) dir.create("graphics")

plotfilename <- "graphics/pnascvsnasc.pdf"
pdf(file = plotfilename, height = 5, width = 5, useDingbats = F)

with(thedata, plot(x = log(Sa),
                   y = pnasc,
                   pch = 21,
                   type = "p",
                   bg = "black",
                   xlab = "log (NASC)",
                   ylab = expression("p"["NASC"]),
                  las = 1)
)
abline(h = 0.85, lty = "dashed")
dev.off()
}
