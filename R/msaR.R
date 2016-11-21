#' msaR
#'
#' Dynamic Multiple Sequence Alignments in R and Shiny
#'
#' @import htmlwidgets
#' 
#' @param msa. File or BioString  Object representing a multiple Sequence Alignment.
#' @param menu. Optional. Default \code{TRUE}. Determines whether to include the interactive menu.
#' @param width. Optional. Default \code{NULL}. The width of the html widget element.
#' @param height. Optional. Default \code{NULL}. The height of the html widget element.
#' @param rowheight Optional. Default \code{20}. Height of a row in the MSA.
#' @param alignmentHeight Optional. Default \code{225}. Height of the MSA.
#' @param overviewbox. optional. Default \code{TRUE}. Include the overview box?
#' @param seqlogo optional. Default \code{TRUE}. Include the seqlogo?
#' @param conservation optional. Default \code{TRUE}. Include the conservation widget?
#' @param markers optional. Default \code{TRUE}. Include the alignment markers? These are the numbers along the top that 
#' @param metacell optional. Default \code{FALSE}. Include the per-sequence metadata.
#' @param leftheader optional. Default \code{TRUE}. Include the header information.
#' @param labels optional. Default \code{TRUE}. Include all of the sequence information msa Labels.
#' @param labelname optional. Default \code{TRUE}. Include sequence names.
#' @param labelid optional. Default \code{TRUE}. Include sequence id.
#' tell you the position in the alignment
#' #Label Options
#' @param labelName Optional. Default \code{TRUE}. Include the LabelName?
#' @param labelId Optional. Default \code{FALSE}. Include the LabelID?
#' @param labelNameLength optional. Default \code{100}. Width of the Lable Names.
#'
#' @export
msaR <- function(msa, 
                 menu=TRUE, 
                 features=NULL, 
                 width = NULL, 
                 height = NULL,
                 labelName = TRUE,
                 labelId = TRUE,
                 rowheight = 15,
                 alignmentHeight = 225,
                 overviewbox = TRUE,
                 seqlogo = TRUE,
                 conservation = FALSE,
                 markers = TRUE,
                 metacell = FALSE,
                 leftheader = TRUE,
                 labels = TRUE,
                 labelname = TRUE,
                 labelid = TRUE,
                 labelNameLength = 100
                 ) {
  config <- list(
    vis=list(
      conserv=conservation,
      overviewbox=overviewbox,
      seqlogo=seqlogo,
      sequences=TRUE,
      markers=markers,
      metacell=metacell,
      gapHeader=FALSE,
      leftHeader=leftheader,
      # about the labels
      labels=labels,
      labelName=labelname,
      labelId=labelid,
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
      alignmentHeight=alignmentHeight,
      columnWidth=15,
      rowHeight=rowheight,
      textVisible=TRUE,
      labelIdLength=30,
      labelNameLength=labelNameLength,
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
