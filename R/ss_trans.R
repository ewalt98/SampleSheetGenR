library(Rlabkey)

${rLabkeySessionId}

# TESTING IN DEV (GRS LIMs DEV), v1.3.6 - current "Prod" is v1.3.4.002
# 20260126 - Adding custom indexing support to allow user entered custom index sequences to remain in the sample sheet.
# 20260126 - Simplifying the printing of sample sheets by using the is_v2 flag to determine V1 vs V2 sample sheet format
# 20260128 - Added automatic filling of Sample_Project based on GRSID lookup if missing in the results data table.
# 20260128 - Change the printing to a function to reduce code duplication and improve performance.
# 20260128 - Some slight cleanup to remove redundant code and comments.
# 20260129 - Centralized environment/path configuration at top (one edit point).
# 20260201 - Adding function to choose multiple index kits in the results grid. - not complete yet, but added the config and started the code for it.
# 20260303 - Added base URL and containerPath auto-detection from run properties to eliminate environment-specific code and reduce setup errors.
# 20260310 - Fix 10x Sample sheet
# 20260318 - If V2 sheet is generated, also generate a V1 copy named *_V1.csv and link via SampleSheetDownloadV1 (no other naming changes).
# 20260618 - Incorporate the new i100 sequencer BCL Convert version 4.4.6 to the sample sheet generation script.  This version of BCL Convert is required for the i100 sequencer, and it is backwards compatible with the NextSeq 1k2k sequencer.  The sample sheet format has not changed, but the BCL Convert version is now included in the [BCLConvert_Settings] section of the sample sheet.

################################################
# Read in the run properties and results data table.
################################################
run.props <- labkey.transform.readRunPropertiesFile("${runInfo}")

###############################################################################
### CONFIG - auto-detect server + container from run properties (no edits per env)
###############################################################################
rk_prop <- function(name) labkey.transform.getRunPropertyValue(run.props, name)

# LabKey includes these in runProperties.tsv
LABKEY_BASE_URL    <- rk_prop("baseUrl")        # e.g. https://rtblims-dev.../labkey
LABKEY_FOLDER_PATH <- rk_prop("containerPath")  # e.g. /GRS LIMs PROD
if (is.na(LABKEY_BASE_URL) || LABKEY_BASE_URL == "") stop("Missing run prop: baseUrl")
if (is.na(LABKEY_FOLDER_PATH) || LABKEY_FOLDER_PATH == "") stop("Missing run prop: containerPath")

# Optional: label environment for logging only
LABKEY_ENV <- if (grepl("rtblims-dev\\.", LABKEY_BASE_URL)) {
  "dev"
} else if (grepl("rtblims-qa\\.", LABKEY_BASE_URL)) {
  "qa"
} else if (grepl("rtblims\\.niaid\\.", LABKEY_BASE_URL)) {
  "prod"
} else {
  "unknown"
}
print(paste("Running in", LABKEY_ENV, "baseUrl:", LABKEY_BASE_URL, "container:", LABKEY_FOLDER_PATH))

# ---- Paths for writing SampleSheets on the server filesystem ----
# If your LabKey "files" root differs, change this ONE value.
LK_FILE_ROOT <- "/labkey/labkey/files"

# Convert containerPath (/A/B) to filesystem segment (A/B). Works for nested folders too.
container_fs <- sub("^/+", "", LABKEY_FOLDER_PATH)

# Where sample sheet files must be written (backing @files/ss_transformation/SampleSheets)
ss_dir <- file.path(LK_FILE_ROOT, container_fs, "@files", "ss_transformation", "SampleSheets")
dir.create(ss_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(ss_dir)) stop(paste("Cannot create ss_dir:", ss_dir))
print(paste("ss_dir:", ss_dir))

# Index mapping file (your code expects it alongside ss_transformation)
mids_file <- file.path(LK_FILE_ROOT, container_fs, "@files", "ss_transformation", "SampleSheet_Gen_RML.csv")

# ---- WebDAV base for hyperlinks (server-relative) ----
# Pull context path from baseUrl (e.g. "/labkey")
context_path <- sub("^https?://[^/]+", "", LABKEY_BASE_URL)
context_path <- sub("/$", "", context_path)

# Encode each segment for URLs (spaces -> %20, etc.), keep "/" separators
container_enc <- paste(vapply(strsplit(container_fs, "/")[[1]], URLencode, "", reserved = TRUE), collapse = "/")

# Keep %40files exactly as LabKey expects for "@files"
webdav_base <- paste0(context_path, "/_webdav/", container_enc, "/%40files/ss_transformation/SampleSheets/")

# Local debug output file
debug_transformed_runprops <- "transformedRunProperties.tsv"

# get important file paths from run props
run.data.file   <- labkey.transform.getRunPropertyValue(run.props, "runDataFile")
run.output.file <- run.props$val3[run.props$name == "runDataFile"]

# read in the results data file content
run.data <- read.delim(run.data.file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

###############################################################################
#   Import data - Convert run.props to easier lookup and capture key fields
###############################################################################
myrun.props <- data.frame(run.props[, c(1, 2)], row.names = 1)

# Sequencing metadata
seqtech <- "Forward"  # current sequencers use forward index
instrument    <- myrun.props["Instrument", ]
index_adapters <- myrun.props["indexAdapterKit", ]
chemistry     <- myrun.props["SeqChemistry", ]
assay         <- myrun.props["SequencingAssayType", ]
run.name      <- myrun.props["RunName", ]
read01        <- myrun.props["Read01", ]
read02        <- myrun.props["Read02", ]
index01       <- myrun.props["Index01", ]
index02       <- myrun.props["Index02", ]
grsid         <- myrun.props["GRS_ID", ]
ss_version    <- myrun.props["SampleSheetType", ]
customrecipe  <- myrun.props["CustomRecipes", ]
ass.com       <- myrun.props["AssayComments", ]

# experiment can be "10X", "Standard", etc.
experiment <- labkey.transform.getRunPropertyValue(run.props, "TypeOfExperiment")

# Determine V2 sample sheet condition once
# VH01716 and SL00784 use the same V2 layout/orientation, but different BCL Convert versions.  Change version below if/when a new version is installed.
is_v2 <- (instrument %in% c("VH01716", "SL00784") || experiment != "Standard")

# Determine BCL Convert software version independently from V1/V2 layout.
bclconvert_software_version <- function(instrument) {
  instrument <- as.character(instrument)

  if (instrument == "SL00784") {
    return("4.4.6")
  }

  # Default for VH01716 and any other V2-producing runs unless explicitly changed.
  return("4.2.7")
}

software_version <- bclconvert_software_version(instrument)

# Determine platform for sample sheet based on instrument (affects default BCL Convert settings section)
instrument_platform_for <- function(instrument) {
  instrument <- as.character(instrument)

  if (instrument == "SL00784") {
    return("MiSeqi100Series")
  }

  if (instrument == "VH01716") {
    return("NextSeq1k2k")
  }

  # Default for other V2-producing runs.
  return("NextSeq1k2k")
}

instrument_platform <- instrument_platform_for(instrument)

print(paste(
  "DEBUG is_v2 =", is_v2,
  "| instrument =", instrument,
  "| experiment =", experiment,
  "| InstrumentPlatform =", instrument_platform,
  "| BCLConvert SoftwareVersion =", software_version
))

############################################
#     Retrieve Project Name and Investigator
############################################
invesname  <- "Unknown Investigator"
grsid_name <- "MISSING_GRSID"

if (!is.na(grsid) && grsid != "" && grsid != 0) {
  sql <- paste0(
    "SELECT Requests.Name, Requests.Investigator ",
    "FROM Requests ",
    "WHERE Requests.RowId = ", grsid
  )
  sqlResults <- labkey.executeSql(
    baseUrl = LABKEY_BASE_URL,
    folderPath = LABKEY_FOLDER_PATH,
    schemaName = "exp.data",
    sql = sql
  )
  if (!is.null(sqlResults) && nrow(sqlResults) > 0) {
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
myssname  <- paste0("SampleSheet_", run.name, ".csv")
my10sheet <- paste0("SampleSheet_", run.name, "_10X.csv")

# --- Sample sheet link/attachment values (DO NOT put a webdav URL into a File-typed field) ---

# sampleSheet for URL-template usage (can be 10X)
ss_basename <- if (experiment == "10X") my10sheet else myssname
run.props$val1[run.props$name == "sampleSheet"] <- ss_basename

# SampleSheetDownload should point to the MAIN sheet (myssname) only (not 10X)
ss_relpath_main <- paste0("ss_transformation/SampleSheets/", myssname)
run.props$val1[run.props$name == "SampleSheetDownload"] <- ss_relpath_main

# WebDAV URL (text field only, if present)
ss_webdav_url <- paste0(webdav_base, ss_basename)
if ("SampleSheetWebLink" %in% run.props$name) {
  run.props$val1[run.props$name == "SampleSheetWebLink"] <- ss_webdav_url
}

# Extra V1 link is MAIN sheet + _V1 suffix, ONLY when is_v2 (primary is V2)
myssname_v1 <- sub("\\.csv$", "_V1.csv", myssname, ignore.case = TRUE)
ss_relpath_v1 <- paste0("ss_transformation/SampleSheets/", myssname_v1)
if ("SampleSheetDownloadV1" %in% run.props$name) {
  run.props$val1[run.props$name == "SampleSheetDownloadV1"] <- if (is_v2) ss_relpath_v1 else NA_character_
}

# Debug
print(paste("sampleSheet (basename):", ss_basename))
print(paste("SampleSheetDownload:", run.props$val1[run.props$name == "SampleSheetDownload"]))
if ("SampleSheetDownloadV1" %in% run.props$name) {
  print(paste("SampleSheetDownloadV1:", run.props$val1[run.props$name == "SampleSheetDownloadV1"]))
}

#############################################################################
#     Creating the sample sheet tables with the index info
#############################################################################
write.table(run.props, file = debug_transformed_runprops, sep = "\t", na = "", row.names = FALSE, quote = FALSE)

if (!file.exists(mids_file)) stop(paste("Missing index mapping file:", mids_file))
mids <- read.csv(file = mids_file, header = TRUE)

mids_row_for <- which((mids$IndexKit == index_adapters) & mids$SequencingTech == "All")
mids_for <- mids[mids_row_for, ]

mids_row_rev <- which((mids$IndexKit == index_adapters) & mids$SequencingTech == seqtech)
mids_rev <- mids[mids_row_rev, ]

sample_data <- run.data

###############################################################################
#   Auto-fill Sample_Project (Priority: Manual > SQL Name > Placeholder)
###############################################################################
if (!"Sample_Project" %in% colnames(sample_data)) sample_data$Sample_Project <- NA
rows_to_fill <- is.na(sample_data$Sample_Project) | sample_data$Sample_Project == "" | sample_data$Sample_Project == "NA"
sample_data$Sample_Project[rows_to_fill] <- grsid_name

# Force GRS_ID column to keep the RowId for LabKey internal links
sample_data$GRS_ID <- grsid

# Initialize Index columns if missing, but preserve user-entered values
if (!"Index" %in% colnames(sample_data)) sample_data$Index <- NA
if (!"Index2" %in% colnames(sample_data)) sample_data$Index2 <- NA
sample_data$Index  <- as.character(sample_data$Index)
sample_data$Index2 <- as.character(sample_data$Index2)

# Normalize Well column to uppercase if present
if ("Well" %in% colnames(sample_data)) sample_data$Well <- toupper(sample_data$Well)

###############################################################################
#   Add index mapping to the sample table (multi-kit aware)
###############################################################################
if (!"IndexKit_MultiSelect" %in% colnames(sample_data)) {
  sample_data$IndexKit_MultiSelect <- NA_character_
} else {
  sample_data$IndexKit_MultiSelect <- as.character(sample_data$IndexKit_MultiSelect)
}

blank_kit <- is.na(sample_data$IndexKit_MultiSelect) |
  sample_data$IndexKit_MultiSelect == "" |
  sample_data$IndexKit_MultiSelect == "NA"

has_well <- "Well" %in% colnames(sample_data) &
  !is.na(sample_data$Well) &
  sample_data$Well != ""

if (!is.na(index_adapters) && nzchar(as.character(index_adapters))) {
  auto_rows <- which(blank_kit & has_well)
  if (length(auto_rows) > 0) {
    sample_data$IndexKit_MultiSelect[auto_rows] <- as.character(index_adapters)
  }
}

manual_rows <- which(blank_kit & !has_well)
if (length(manual_rows) > 0) {
  sample_data$IndexKit_MultiSelect[manual_rows] <- "Custom Kit"
}

print(paste("IndexKit filled:", length(auto_rows), "run-level,", length(manual_rows), "custom"))

valid_kits <- unique(mids$IndexKit)
bad_kit_rows <- which(!is.na(sample_data$IndexKit_MultiSelect) &
  !(sample_data$IndexKit_MultiSelect %in% valid_kits))
if (length(bad_kit_rows) > 0) {
  print(paste("Warning: unknown IndexKit_MultiSelect values in rows:", paste(bad_kit_rows, collapse = ",")))
  print(paste("Unknown kits:", paste(unique(sample_data$IndexKit_MultiSelect[bad_kit_rows]), collapse = ",")))
}

# ensure index/id/MID columns exist (don't clobber existing)
if (!"I7_Index_ID" %in% colnames(sample_data)) sample_data$I7_Index_ID <- NA
if (!"I5_Index_ID" %in% colnames(sample_data)) sample_data$I5_Index_ID <- NA
if (!"MIDSet" %in% colnames(sample_data)) sample_data$MIDSet <- NA

# helper: safe string-empty-or-na test
is_blank <- function(x) is.na(x) | x == "" | identical(x, "NA")

# Precompute mids subsets by kit and sequencing tech to speed repeated lookups
unique_kits <- unique(mids$IndexKit)
forward_map <- list()
rev_map <- list()
for (k in unique_kits) {
  forward_map[[k]] <- mids[mids$IndexKit == k & mids$SequencingTech == "All", , drop = FALSE]
  rev_map[[k]] <- mids[mids$IndexKit == k & mids$SequencingTech == seqtech, , drop = FALSE]
}

# Now iterate rows and apply mapping when possible
for (i in seq_len(nrow(sample_data))) {
  well_val <- NA
  if ("Well" %in% colnames(sample_data)) well_val <- sample_data$Well[i]
  if (is.na(well_val) || well_val == "") next

  kit_to_use <- NULL
  krow <- sample_data$IndexKit_MultiSelect[i]
  if (!is_blank(krow)) kit_to_use <- as.character(krow)

  if (is.null(kit_to_use) || is_blank(kit_to_use)) {
    if (!is_blank(index_adapters)) {
      kit_to_use <- as.character(index_adapters)
      sample_data$IndexKit_MultiSelect[i] <- kit_to_use
    }
  }
  if (is.null(kit_to_use) || is_blank(kit_to_use)) next

  fm <- forward_map[[kit_to_use]]
  if (!is.null(fm) && nrow(fm) > 0) {
    fr_idx <- which(fm$Well == well_val)
    if (length(fr_idx) == 1) {
      if (is_blank(sample_data$Index[i]))        sample_data$Index[i]        <- as.character(fm$Index[fr_idx])
      if (is_blank(sample_data$I7_Index_ID[i]))  sample_data$I7_Index_ID[i]  <- as.character(fm$IndexID[fr_idx])
      if (is_blank(sample_data$MIDSet[i]))       sample_data$MIDSet[i]       <- as.character(fm$MIDSet[fr_idx])
    }
  }

  rm <- rev_map[[kit_to_use]]
  if (!is.null(rm) && nrow(rm) > 0) {
    rr_idx <- which(rm$Well == well_val)
    if (length(rr_idx) == 1) {
      if (is_blank(sample_data$Index2[i]))       sample_data$Index2[i]       <- as.character(rm$Index[rr_idx])
      if (is_blank(sample_data$I5_Index_ID[i]))  sample_data$I5_Index_ID[i]  <- as.character(rm$IndexID[rr_idx])
    }
  }
}

num_missing_index  <- sum(is_blank(sample_data$Index))
num_missing_index2 <- sum(is_blank(sample_data$Index2))
print(paste("After multi-kit lookup: rows missing Index =", num_missing_index, "missing Index2 =", num_missing_index2))

# Coerce to character to avoid factor/integer import quirks
sample_data$Index        <- as.character(sample_data$Index)
sample_data$Index2       <- as.character(sample_data$Index2)
sample_data$I7_Index_ID  <- as.character(sample_data$I7_Index_ID)
sample_data$I5_Index_ID  <- as.character(sample_data$I5_Index_ID)
sample_data$MIDSet       <- as.character(sample_data$MIDSet)

num_i7_assigned <- sum(!is.na(sample_data$I7_Index_ID) & sample_data$I7_Index_ID != "")
num_i5_assigned <- sum(!is.na(sample_data$I5_Index_ID) & sample_data$I5_Index_ID != "")
print(paste("Auto-index assignments: I7 =", num_i7_assigned, "I5 =", num_i5_assigned))

# IMPORTANT: write the transformed results back to the output file that LabKey will import.
write.table(
  sample_data,
  file = run.output.file,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)

###############################################################################
#   Prepare Print Table Variants (use only existing columns to avoid failures)
###############################################################################
if (experiment == "10X") {
  sample_10x <- data.frame(
    Lane   = rep("*", nrow(sample_data)),
    Sample = sample_data$Sample_ID,
    Index  = sample_data$I7_Index_ID,
    stringsAsFactors = FALSE
  )
}

if (is_v2) {
  cols_ns2k <- intersect(c("Sample_ID", "Index", "Index2"), colnames(sample_data))
  ns2k_sample_data <- sample_data[, cols_ns2k, drop = FALSE]
}

# V1 print data (omit GRS_ID so downstream tooling doesn't choke)
v1_print_data <- sample_data[, !(names(sample_data) %in% "GRS_ID"), drop = FALSE]

###############################################################################
#   Helper to write ss sections and data robustly (writes to explicit paths)
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
  lines <- c(lines, ",")
  con <- file(file, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
  invisible(TRUE)
}

# Full paths for writing into @files/ss_transformation/SampleSheets
main_ss_path <- file.path(ss_dir, myssname)
main_v1_path <- file.path(ss_dir, myssname_v1)
tenx_path    <- file.path(ss_dir, my10sheet)

###############################################################################
#   Write MAIN sample sheet (V2 or V1 depending on is_v2)
###############################################################################
writeLines("[Header],", main_ss_path, useBytes = TRUE)

if (is_v2) {
  header_data <- list(
    FileFormatVersion = "2",
    RunName = run.name,
    InstrumentPlatform = instrument_platform,
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
write_ss_section(main_ss_path, NULL, header_data)

reads_data <- if (is_v2) {
  list(Read1Cycles = read01, Read2Cycles = read02, Index1Cycles = index01, Index2Cycles = index02)
} else {
  list(Read01 = read01, Index01 = index01, Index02 = index02, Read02 = read02)
}
write_ss_section(main_ss_path, "[Reads],", reads_data)

if (is_v2) {
  if (!is.null(customrecipe) && nzchar(customrecipe)) {
    write_ss_section(main_ss_path, "[Sequencing_Settings],", list(LibraryPrepKits = customrecipe))
  }
write_ss_section(
  main_ss_path,
  "[BCLConvert_Settings],",
  list(
    SoftwareVersion = software_version,
    FastqCompressionFormat = "gzip"
  )
)
} else {
  write_ss_section(main_ss_path, "[Settings],", list())
}

if (is_v2) {
  write.table("[BCLConvert_Data],", main_ss_path, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(ns2k_sample_data, main_ss_path, sep = ",", quote = FALSE, na = "", row.names = FALSE, append = TRUE)
} else {
  write.table("[Data],", main_ss_path, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(v1_print_data, main_ss_path, sep = ",", quote = FALSE, na = "", row.names = FALSE, append = TRUE)
}

###############################################################################
#   Write EXTRA V1 copy ONLY when main is V2
###############################################################################
# --- GUARANTEED extra V1 copy when is_v2 is TRUE ---
if (is_v2) {
  # Derive V1 path from the actual main path, so it can't drift
  main_v1_path <- sub("\\.csv$", "_V1.csv", main_ss_path, ignore.case = TRUE)

  # Overwrite/create V1 file
  writeLines("[Header],", main_v1_path, useBytes = TRUE)

  header_v1 <- list(
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
  write_ss_section(main_v1_path, NULL, header_v1)

  reads_v1 <- list(Read01 = read01, Index01 = index01, Index02 = index02, Read02 = read02)
  write_ss_section(main_v1_path, "[Reads],", reads_v1)

  write_ss_section(main_v1_path, "[Settings],", list())

  write.table("[Data],", main_v1_path, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(v1_print_data, main_v1_path, sep = ",", quote = FALSE, na = "", row.names = FALSE, append = TRUE)

  # PROVE it exists; fail loudly if not
  print(paste("Wrote extra V1 sample sheet:", main_v1_path))
  print(paste("V1 exists:", file.exists(main_v1_path)))
  if (!file.exists(main_v1_path)) {
    stop(paste("Expected V1 file was not created:", main_v1_path))
  }
}

###############################################################################
#   10X output (independent)
###############################################################################
if (experiment == "10X") {
  write.table(sample_10x, tenx_path, col.names = TRUE, quote = FALSE, sep = ",", row.names = FALSE)
}

print("Sample sheet files in ss_dir:")
print(list.files(ss_dir, pattern = "SampleSheet_", full.names = TRUE))

# End of Script