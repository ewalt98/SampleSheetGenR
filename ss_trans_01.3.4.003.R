library(Rlabkey)

${rLabkeySessionId}

# current "Prod" is 01.3.4.003 version
# 20260126 - Adding custom indexing support to allow user entered custom index sequences to remain in the sample sheet.
# 20260126 - Simplifying the printing of sample sheets by using the is_v2 flag to determine V1 vs V2 sample sheet format
# 20260128 - Added automatic filling of Sample_Project based on GRSID lookup if missing in the results data table.
# 20260128 - Change the printing to a function to reduce code duplication and improve performance.
# 20260128 - Some slight cleanup to remove redundant code and comments.
# 20260129 - Centralized environment/path configuration at top (one edit point).
# 20260213 - Fix 10X sample sheet writing to only include relevant columns and standard sample sheet download link.

###############################################################################
### CONFIG - single edit point for configruring server/instance and paths
###############################################################################
# Base LabKey connection used for executeSql (if you have multiple servers and instances, change here)
LABKEY_BASE_URL     <- "https://rtblims.niaid.nih.gov/labkey"
LABKEY_FOLDER_PATH  <- "/GRS LIMs PROD"   # folderPath for labkey.executeSql

# File system path that backs the WebDAV endpoint (where sample sheet files must be written).  Keep the spaces.
ss_dir              <- "/labkey/labkey/files/GRS LIMs PROD/@files/ss_transformation/SampleSheets"

# The WebDAV URL template used in the assay result link (only ${sampleSheet} will be replaced)
# Keep the %20 and %40 encodings exactly as needed for LabKey's _webdav link.
webdav_base <- "/labkey/_webdav/GRS%20LIMs%20PROD/%40files/ss_transformation/SampleSheets/"
# Local debug output file (optional) - can remain relative or absolute
debug_transformed_runprops <- "transformedRunProperties.tsv"

################################################
# Read in the run properties and results data table.
################################################
run.props = labkey.transform.readRunPropertiesFile("${runInfo}");

# get important file paths from run props
run.data.file   <- labkey.transform.getRunPropertyValue(run.props, "runDataFile");
run.output.file <- run.props$val3[run.props$name == "runDataFile"];

# read in the results data file content
run.data = read.delim(run.data.file, header=TRUE, sep="\t", stringsAsFactors = FALSE);

###############################################################################
#   Import data - Convert run.props to easier lookup and capture key fields
###############################################################################
myrun.props <- data.frame(run.props[,c(1,2)], row.names = 1)

# Sequencing metadata
seqtech <- "Forward"  # current sequencers use forward index
instrument <- myrun.props['Instrument',]
index_adapters <- myrun.props['indexAdapterKit',]
chemistry <- myrun.props['SeqChemistry',]
assay <- myrun.props['SequencingAssayType',]
run.name <- myrun.props['RunName',]
read01 <- myrun.props['Read01',]
read02 <- myrun.props['Read02',]
index01 <- myrun.props['Index01',]
index02 <- myrun.props['Index02',]
grsid <- myrun.props['GRS_ID',]
ss_version <- myrun.props['SampleSheetType',]
customrecipe <- myrun.props['CustomRecipes',]
ass.com <- myrun.props['AssayComments',]

# experiment can be "10X", "Standard", etc.
experiment <- labkey.transform.getRunPropertyValue(run.props, "TypeOfExperiment")

# Determine V2 sample sheet condition once
is_v2 <- (instrument == 'VH01716' || experiment != "Standard")

############################################
#     Retrieve Project Name and Investigator
############################################
invesname  <- "Unknown Investigator"
grsid_name <- "MISSING_GRSID"

if(!is.na(grsid) && grsid != "" && grsid != 0) {
  sql <- paste0("SELECT Requests.Name, Requests.Investigator 
                 FROM Requests 
                 WHERE Requests.RowId = ", grsid)
  sqlResults <- labkey.executeSql(
    baseUrl=LABKEY_BASE_URL,
    folderPath=LABKEY_FOLDER_PATH,
    schemaName="exp.data",
    sql=sql)
  if(!is.null(sqlResults) && nrow(sqlResults) > 0) {
    grsid_name <- as.character(sqlResults$Name[1])
    invesname  <- as.character(sqlResults$Investigator[1])
  } else {
    grsid_name <- paste0("ID_", grsid)
    invesname  <- "GRS ID not found in database"
  }
} else {
  invesname <- "You didn't enter GRSID in run section."
}

print(paste("SQL Lookup Result - Project Name:", grsid_name))

############################################
#     Sample sheet file naming (basename only)
############################################
myssname <- paste("SampleSheet_", run.name, ".csv", sep = "")
my10sheet <- paste("SampleSheet_", run.name, "_10X.csv", sep = "")
# ss_basename is the SEQUENCER-LOAD sample sheet (always standard)
ss_basename <- myssname

# --- Sample sheet link/attachment values (DO NOT put a webdav URL into a File-typed field) ---

# 1) sampleSheet should ALWAYS be the STANDARD sheet (sequencer load sheet)
run.props$val1[run.props$name == "sampleSheet"] <- ss_basename # myssname is the basename, which is what LabKey's _webdav linking expects for file attachments.  DO NOT put a webdav URL here or it will break the attachment.

# 2) SampleSheetDownload (File type) should be 10X when selected, otherwise standard
download_name <- if (experiment == "10X") my10sheet else myssname
download_relpath <- paste0("ss_transformation/SampleSheets/", download_name)
run.props$val1[run.props$name == "SampleSheetDownload"] <- download_relpath

# 3) Optional URL/text field: decide what you want it to follow
# If you want it to match the sequencer-load sheet (standard), use myssname:
ss_webdav_url <- paste0(webdav_base, ss_basename)

# If instead you want it to match the downloadable file (10X on 10X runs), use download_name:
# ss_webdav_url <- paste0(webdav_base, ss_basename)

if ("SampleSheetWebLink" %in% run.props$name) {
  run.props$val1[run.props$name == "SampleSheetWebLink"] <- ss_webdav_url
}

print(paste("sampleSheet (sequencer):", run.props$val1[run.props$name == "sampleSheet"]))
print(paste("SampleSheetDownload (file):", run.props$val1[run.props$name == "SampleSheetDownload"]))

# Debug prints to confirm what gets written
print(paste("sampleSheet (basename):", ss_basename))
print(paste("SampleSheetDownload (File field value):", run.props$val1[run.props$name == "SampleSheetDownload"]))
if ("SampleSheetWebLink" %in% run.props$name) {
  print(paste("SampleSheetWebLink (URL text):", run.props$val1[run.props$name == "SampleSheetWebLink"]))
} else {
  print(paste("WebDAV URL (not stored unless field exists):", ss_webdav_url))
}

#############################################################################
#     Creating the sample sheet tables with the index info
#############################################################################
# save the run props for debugging (this path is relative to transform working dir)
write.table(run.props, file=debug_transformed_runprops, sep="\t", na="", row.names=FALSE, quote=FALSE);

# read index mapping table (keep this path in the config block if you want)
mids_file <- file.path(dirname(ss_dir), "SampleSheet_Gen_RML.csv")

if(!file.exists(mids_file)) stop(paste("Missing index mapping file:", mids_file))

mids <- read.csv(file=mids_file, header = TRUE)

mids_row_for <- which((mids$IndexKit == index_adapters) & mids$SequencingTech == "All")
mids_for <- mids[mids_row_for,]

mids_row_rev <- which((mids$IndexKit == index_adapters) & mids$SequencingTech == seqtech)
mids_rev <- mids[mids_row_rev,]

# Use run.data directly as starting sample_data
sample_data <- run.data

###############################################################################
#   Auto-fill Sample_Project (Priority: Manual > SQL Name > Placeholder)
###############################################################################
if(!"Sample_Project" %in% colnames(sample_data)) {
  sample_data$Sample_Project <- NA
}
rows_to_fill <- is.na(sample_data$Sample_Project) | sample_data$Sample_Project == "" | sample_data$Sample_Project == "NA"
sample_data$Sample_Project[rows_to_fill] <- grsid_name

# Force GRS_ID column to keep the RowId for LabKey internal links
sample_data$GRS_ID <- grsid

# Initialize Index columns if missing, but preserve user-entered values
if(!"Index" %in% colnames(sample_data)) sample_data$Index <- NA
if(!"Index2" %in% colnames(sample_data)) sample_data$Index2 <- NA
sample_data$Index <- as.character(sample_data$Index)
sample_data$Index2 <- as.character(sample_data$Index2)

# Normalize Well column to uppercase if present
if("Well" %in% colnames(sample_data)) sample_data$Well <- toupper(sample_data$Well)

###############################################################################
#   Add index mapping to the sample table
###############################################################################
# matches for index 1 (for)
matches_for <- match(sample_data$Well, mids_for$Well)
has_match_for <- !is.na(matches_for)
if(any(has_match_for)) {
  sample_data$Index[has_match_for] <- mids_for$Index[matches_for[has_match_for]]
  # initialize ID columns if not present
  if(!"I7_Index_ID" %in% colnames(sample_data)) sample_data$I7_Index_ID <- NA
  if(!"MIDSet" %in% colnames(sample_data)) sample_data$MIDSet <- NA
  sample_data$I7_Index_ID[has_match_for] <- mids_for$IndexID[matches_for[has_match_for]]
  sample_data$MIDSet[has_match_for] <- mids_for$MIDSet[matches_for[has_match_for]]
}

# matches for index 2 (rev)
matches_rev <- match(sample_data$Well, mids_rev$Well)
has_match_rev <- !is.na(matches_rev)
if(any(has_match_rev)) {
  sample_data$Index2[has_match_rev] <- mids_rev$Index[matches_rev[has_match_rev]]
  if(!"I5_Index_ID" %in% colnames(sample_data)) sample_data$I5_Index_ID <- NA
  sample_data$I5_Index_ID[has_match_rev] <- mids_rev$IndexID[matches_rev[has_match_rev]]
}

###############################################################################
# Ensure index ID columns exist (don't overwrite if present) and persist them.
###############################################################################
if(!"I7_Index_ID" %in% colnames(sample_data)) sample_data$I7_Index_ID <- NA
if(!"I5_Index_ID" %in% colnames(sample_data)) sample_data$I5_Index_ID <- NA
if(!"MIDSet" %in% colnames(sample_data)) sample_data$MIDSet <- NA

# Coerce to character to avoid factor/integer import quirks
sample_data$Index     <- as.character(sample_data$Index)
sample_data$Index2    <- as.character(sample_data$Index2)
sample_data$I7_Index_ID <- as.character(sample_data$I7_Index_ID)
sample_data$I5_Index_ID <- as.character(sample_data$I5_Index_ID)
sample_data$MIDSet    <- as.character(sample_data$MIDSet)

# Debug/logging: count how many auto-assigned index ids we added
num_i7_assigned <- sum(!is.na(sample_data$I7_Index_ID) & sample_data$I7_Index_ID != "")
num_i5_assigned <- sum(!is.na(sample_data$I5_Index_ID) & sample_data$I5_Index_ID != "")
print(paste("Auto-index assignments: I7 =", num_i7_assigned, "I5 =", num_i5_assigned))

# IMPORTANT: write the transformed results back to the output file that LabKey will import.
write.table(
  sample_data,
  file = run.output.file,   # critical: LabKey imports this file
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)

###############################################################################
#   Prepare Print Tables (use only existing columns to avoid failures)
###############################################################################
# 1. 10X table (if needed)
if(experiment == "10X") {
  cols_10x <- intersect(c("Sample_ID", "Index", "I7_Index_ID"), colnames(sample_data))
  sample_10x <- sample_data[, cols_10x, drop = FALSE]
}

# 2. NextSeq 2000 (V2) table
if (is_v2) {
  cols_ns2k <- intersect(c("Sample_ID", "Index", "Index2"), colnames(sample_data))
  ns2k_sample_data <- sample_data[, cols_ns2k, drop = FALSE]
}

# 3. V1 print data (omit GRS_ID so downstream tooling doesn't choke)
v1_print_data <- sample_data[, !(names(sample_data) %in% "GRS_ID"), drop = FALSE]

###############################################################################
#   Helper to write ss sections and data robustly (writes in ss_dir)
###############################################################################
write_ss_section <- function(file, section_name = NULL, kv_list = list()) {
  lines <- character(0)
  if (!is.null(section_name) && nzchar(section_name)) {
    lines <- c(lines, section_name)
  }
  if (length(kv_list) > 0) {
    for (k in names(kv_list)) {
      v <- kv_list[[k]]
      if (is.null(v)) v <- ""
      vstr <- paste0(as.character(v), collapse = "")
      lines <- c(lines, paste0(k, ",", vstr))
    }
  }
  # separator blank line (keeps original behavior)
  lines <- c(lines, ",")
  con <- file(file, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
  invisible(TRUE)
}

# Ensure sample-sheet directory is the working directory when writing file basenames
# This is intentionally done to preserve the LabKey _webdav linking behavior.
old_wd <- getwd()
if(dir.exists(ss_dir)) {
  setwd(ss_dir)
} else {
  print(paste("Warning: ss_dir does not exist:", ss_dir))
}

# Start by creating/overwriting the file header (write by basename)
writeLines("[Header],", myssname, useBytes = TRUE)

# Prepare header_data depending on is_v2
if (is_v2) {
  header_data <- list(
    FileFormatVersion = "2",
    RunName = run.name,
    InstrumentPlatform = "NextSeq1k2k",
    IndexOrientation = "Forward",
    Custom_GRSID = grsid_name
  )
} else {
  header_data <- list(
    "IEMFileVersion"    = "2.20.0.422",
    "Investigator Name" = invesname,
    "Date"              = format(Sys.time()),
    "Workflow"          = "LabKey Sample Sheet Gen",
    "Application"       = experiment,
    "Instrument"        = instrument,
    "Assay"             = assay,
    "Index Adapters"    = index_adapters,
    "Description"       = ass.com,
    "Chemistry"         = chemistry
  )
}

write_ss_section(myssname, NULL, header_data)

reads_data <- if (is_v2) {
  list(Read1Cycles = read01, Read2Cycles = read02, Index1Cycles = index01, Index2Cycles = index02)
} else {
  list(Read01 = read01, Index01 = index01, Index02 = index02, Read02 = read02)
}
write_ss_section(myssname, "[Reads],", reads_data)

# Sequencing/settings
if (is_v2) {
  if (!is.null(customrecipe) && nzchar(customrecipe)) {
    write_ss_section(myssname, "[Sequencing_Settings],", list(LibraryPrepKits = customrecipe))
  }
  write_ss_section(myssname, "[BCLConvert_Settings],", list(SoftwareVersion = "4.2.7", FastqCompressionFormat = "gzip"))
} else {
  write_ss_section(myssname, "[Settings],", list())
}

# Data section: write appropriate table
if (is_v2) {
  write.table("[BCLConvert_Data],", myssname, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(ns2k_sample_data, myssname, sep = ",", quote = FALSE, na = "", row.names = FALSE, append = TRUE)
} else {
  write.table("[Data],", myssname, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(v1_print_data, myssname, sep = ",", quote = FALSE, na = "", row.names = FALSE, append = TRUE)
}

###############################################################################
#   10X output (independent) - write by basename while in ss_dir
###############################################################################
if(experiment == "10X") {
  write.table(sample_10x, my10sheet, col.names = TRUE, quote = FALSE, sep = ",", row.names = FALSE)
}

# Restore original working dir for the transform runtime (keeps other behavior unchanged)
setwd(old_wd)
# End of Script
