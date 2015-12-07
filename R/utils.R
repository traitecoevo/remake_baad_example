
## Sets up an empty plotting window
blank_plot <- function(xlim = c(0.02, 1.3), ylim = c(0.04, 15), use.box = TRUE,
  ...) {
  plot.new()
  plot.window(xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", ann = FALSE,
    ...)
  if (use.box)
    box()
}

## Make colours semitransparent:
make_transparent <- function(col, opacity = 0.5) {
  if (length(opacity) > 1 && any(is.na(opacity))) {
    n <- max(length(col), length(opacity))
    opacity <- rep(opacity, length.out = n)
    col <- rep(col, length.out = n)
    ok <- !is.na(opacity)
    ret <- rep(NA, length(col))
    ret[ok] <- Recall(col[ok], opacity[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1, ], tmp[2, ], tmp[3, ], alpha = opacity)
  }
}

## Position label at a fractional x/y position on a plot
label <- function(px, py, lab, ..., adj = c(0, 1)) {
  usr <- par("usr")
  text(usr[1] + px * (usr[2] - usr[1]), usr[3] + py * (usr[4] - usr[3]), lab,
    adj = adj, ...)
}
