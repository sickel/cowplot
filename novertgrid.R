library(ggplot2)
guide_grid_orig <- guide_grid
# Create the replacement function
guide_grid_no_vline <- function(theme, x.minor, x.major, y.minor, y.major) {
  ggname("grill", grobTree(
    theme_render(theme, "panel.background"),
    theme_render(
      theme, "panel.grid.minor", name = "y",
      x = rep(0:1, length(y.minor)), y = rep(y.minor, each=2),
      id.lengths = rep(2, length(y.minor))
    ),
    theme_render(
      theme, "panel.grid.major", name = "y",
      x = rep(0:1, length(y.major)), y = rep(y.major, each=2),
      id.lengths = rep(2, length(y.major))
    )
  ))
}

# Assign the function inside ggplot2
assignInNamespace("guide_grid", guide_grid_no_vline, pos="package:ggplot2")



# http://www.r-bloggers.com/r-good-practice-%E2%80%93-adding-footnotes-to-graphics/
makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"npc") - unit(2, "mm"),
             y= unit(2, "mm"),
             just=c("right", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

makeLabel <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= 1, color= 'black')
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(5, "mm"),
             y= unit(1,"npc") - unit(2, "mm"),
             just=c("left", "top"),
             gp=gpar(cex= size, col=color))
   popViewport()
}