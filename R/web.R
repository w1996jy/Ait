#' web UI Module
#' @description web UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title web_ui
#' @name web_ui
#' @import shiny
#' @import bslib
#' @export
#'
web_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      lapply(c(
        list(
          list(name = "iTOL", url = "https://itol.embl.de/", desc = "Phylogenetic Tree Analysis Website"),
          list(name = "MEME", url = "https://meme-suite.org/meme/", desc = "Motif Analysis Toolbox"),
          list(name = "SwissTargetPrediction", url = "http://www.swisstargetprediction.ch/", desc = "Component Target Prediction Database"),
          list(name = "PlantTFDB", url = "https://planttfdb.gao-lab.org/", desc = "Plant Transcription Factor Database"),
          list(name = "STRING", url = "https://cn.string-db.org/", desc = "Protein-Protein Interaction Networks"),
          list(name = "BlastKOALA", url = "https://www.kegg.jp/blastkoala/", desc = "KEGG BLAST for Functional Annotation"),
          list(name = "PlantPAN", url = "https://plantpan.itps.ncku.edu.tw/plantpan4/index.html", desc = "Plant Promoter Analysis Navigator"),
          list(name = "National Genomics Data Center", url = "https://ngdc.cncb.ac.cn/", desc = "NGDC"),
          list(name = "TCMSP", url = "https://old.tcmsp-e.com/tcmsp.php", desc = "Traditional Chinese Medicine Systems Pharmacology Database"),
          list(name = "PubChem", url = "https://pubchem.ncbi.nlm.nih.gov/", desc = "Open Chemistry Database"),
          list(name = "DAVID", url = "https://david.ncifcrf.gov/", desc = "Database for Annotation, Visualization and Integrated Discovery"),
          list(name = "Reactome", url = "https://reactome.org/", desc = "Open Access Pathway Database"),
          list(name = "JASPAR", url = "https://jaspar.elixir.no/", desc = "Database of Transcription Factor Binding Profiles"),
          list(name = "Ensembl", url = "https://asia.ensembl.org/index.html", desc = "Genome Browser for Vertebrates and Other Eukaryotes"),
          list(name = "MBRole", url = "https://csbg.cnb.csic.es/mbrole2/", desc = "Metabolic and Biochemical Role Analysis Tool"),
          list(name = "NCBI BLAST", url = "https://blast.ncbi.nlm.nih.gov/Blast.cgi", desc = "Basic Local Alignment Search Tool"),
          list(name = "PubMed", url = "https://pubmed.ncbi.nlm.nih.gov/", desc = "Database of Biomedical Literature"),
          list(name = "SignalP", url = "https://services.healthtech.dtu.dk/services/SignalP-6.0/", desc = "Prediction of Signal Peptides"),
          list(name = "Pfam", url = "http://pfam.xfam.org/", desc = "Protein Family Database"),
          list(name = "AnimalTFDB", url = "https://guolab.wchscu.cn/AnimalTFDB", desc = "Animal Transcription Factor Database"),
          list(name = "Mentha", url = "https://mentha.uniroma2.it/", desc = "Database for Plant Secondary Metabolites"),
          list(name = "HMMER", url = "https://www.ebi.ac.uk/Tools/hmmer/", desc = "Protein Sequence Analysis Using Hidden Markov Models"),
          list(name = "eHOMD", url = "https://www.homd.org/", desc = "The Expanded Human Oral Microbiome Database")
        )
      ), function(item) {
        column(2,
               tags$div(style = "border: 1px solid #ccc; padding: 15px; height: 150px; text-align: center; margin-bottom: 15px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                        tags$p(item$desc, style = "font-weight: bold; margin: 0;"),
                        tags$a(href = item$url, target = "_blank", paste("Visit", item$name), style = "text-decoration: none;")
               )
        )
      })
    )
  )
}
