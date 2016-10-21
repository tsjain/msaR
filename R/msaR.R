#' msaR
#'
#' Dynamic Multiple Sequence Alignments in R and Shiny
#'
#' @import htmlwidgets
#'
#' @export
msaR <- function(msa, menu=T, config=NULL, features=NULL, width = NULL, height = NULL) {
  # if no config options are given, init standard config
  if(is.null(config)){
    config <- list(
      vis=list(
        conserv=TRUE,
        overviewbox=TRUE,
        seqlogo=FALSE,
        sequences=TRUE,
        markers=TRUE,
        metacell=FALSE,
        gapHeader=FALSE,
        leftHeader=TRUE,
        # about the labels
        labels=TRUE,
        labelName=TRUE,
        labelId=TRUE,
        labelPartition=FALSE,
        labelCheckbox=FALSE,
        # meta stuff
        metaGaps=TRUE,
        metaIdentity=TRUE,
        metaLinks=TRUE
      ),
      conf=list(
        dropImport=TRUE,
        registerMouseHover=FALSE,
        registerMouseClicks=TRUE,
        eventBus=TRUE,
        alphabetSize= 20,
        dropImport=FALSE,
        debug=FALSE,
        hasRef=FALSE,
        manualRendering=FALSE
      ),
      colorscheme=list(
        scheme="taylor",
        colorBackground=TRUE,
        showLowerCase=TRUE,
        opacity=0.6
      ),
      zoomer=list(
        menuFontsize='12px',
        autoResize=TRUE,
        alignmentWidth="auto",
        alignmentHeight=225,
        columnWidth=15,
        rowHeight=15,
        textVisible=TRUE,
        labelIdLength=30,
        labelNameLength=100,
        labelPartLength=15,
        labelCheckLength=15,
        labelFontsize=13,
        labelLineHeight="13px",
        # marker
        markerFontsize="10px",
        stepSize=1,
        markerStepSize=2,
        markerHeight=20,
        #canvas
        residueFont="13", #in px
        canvasEventScale=1,
        # overview box
        boxRectHeight=2,
        boxRectWidth=2,
        overviewboxPaddingTop=10,
        # meta cell
        metaGapWidth=35,
        metaIdentWidth=40,
        metaLinksWidth=25
      ),
      menu=list(
        menuFontsize="14px",
        menuItemFontsize="14px",
        menuItemLineHeight="14px",
        menuMarginLeft="3px",
        menuPadding="3px 4px 3px 4px"
      )
    )
  }

  
  # forward options using x
  x <- list(
    alignment=as.fasta(msa),
    config=config,
    menu=menu,
    features=features
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'msaR',
    x,
    width = width,
    height = height,
    package = 'msaR'
  )
}


#' Widget output function for use in Shiny
#'
#' @export
msaROutput <- function(outputId, width = '100%', height = '300px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'msaR', width, height, package = 'msaR')
}

#' Widget render function for use in Shiny
#'
#' @export
renderMsaR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, msaROutput, env, quoted = TRUE)
}
