library(Rlabkey)

${rLabkeySessionId}

# TESTING IN DEV (GRS LIMs DEV), v1.3.6 - current "Prod" is v1.3.4.002 
# 20260126 - Adding custom indexing support to allow user entered custom index sequences to remain in the sample sheet.
# 20260126 - Simplifying the printing of sample sheets by using the is_v2 flag to determine V1 vs V2 sample sheet format
# 20260128 - Added automatic filling of Sample_Project based on GRSID lookup if missing in the results data table.
# 20260128 - Change the printing to a function to reduce code duplication and improve performance.
# 20260128 - Some slight cleanup to remove redundant code and comments.
# 20260129 - Centralized environment/path configuration at top (one edit point).
# 20260201 - Adding function to choose multiple index  kits in the results grid. - not complete yet, but added the config and started the code for it.
# 20260303 - Added base URL and containerPath auto-detection from run properties to eliminate environment-specific code and reduce setup errors.

################################################
# Read in the run properties and results data table.
################################################
run.props = labkey.transform.readRunPropertiesFile("${runInfo}");

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
LABKEY_ENV <- if (grepl("rtblims-dev\\.", LABKEY_BASE_URL)) "dev"
         else if (grepl("rtblims-qa\\.",  LABKEY_BASE_URL)) "qa"
         else if (grepl("rtblims\\.niaid\\.", LABKEY_BASE_URL)) "prod"
         else "unknown"
print(paste("Running in", LABKEY_ENV, "baseUrl:", LABKEY_BASE_URL, "container:", LABKEY_FOLDER_PATH))

# ---- Paths for writing SampleSheets on the server filesystem ----
# If your LabKey "files" root differs, change this ONE value.
LK_FILE_ROOT <- "/labkey/labkey/files"

# Convert containerPath (/A/B) to filesystem segment (A/B). Works for nested folders too.
container_fs <- sub("^/+", "", LABKEY_FOLDER_PATH)

# Where sample sheet files must be written (backing @files/ss_transformation/SampleSheets)
ss_dir <- file.path(LK_FILE_ROOT, container_fs, "@files", "ss_transformation", "SampleSheets")

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

# --- Sample sheet link/attachment values (DO NOT put a webdav URL into a File-typed field) ---

# 1) sampleSheet MUST be basename only for your assay URL-template field (${sampleSheet})
ss_basename <- if (experiment == "10X") my10sheet else myssname
run.props$val1[run.props$name == "sampleSheet"] <- ss_basename

# 2) Insert the relative path and WebDAV URL into the appropriate fields
ss_relpath <- paste0("ss_transformation/SampleSheets/", ss_basename)
ss_webdav_url <- paste0(webdav_base, ss_basename)

# 3) File-typed field MUST be a file reference, typically just the basename (or possibly a relative @files path).
# Start with basename (most common and matches what you said it “should just start with ss_...”)
run.props$val1[run.props$name == "SampleSheetDownload"] <- ss_relpath

# only set it if that property exists so we don't affect other runs.
if ("SampleSheetWebLink" %in% run.props$name) {
  run.props$val1[run.props$name == "SampleSheetWebLink"] <- ss_webdav_url
}

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
# mids_file read above in config block - make sure to update there if you move it

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
#   Add index mapping to the sample table (multi-kit aware)
#
#  - Per-row kit priority: sample_data$IndexKit_MultiSelect (or "Index_Kit") -> run-level index_adapters -> no lookup
#  - Only fill when Well is present and we find a matching mapping row in mids
#  - Preserve manual Index/Index2 values (only assign where missing/NA/"")
###############################################################################
# --- Ensure canonical kit column exists and is populated from run-level when blank --- Might not need this, maybe over guarding
if (!"IndexKit_MultiSelect" %in% colnames(sample_data)) {
  sample_data$IndexKit_MultiSelect <- NA_character_
} else {
  sample_data$IndexKit_MultiSelect <- as.character(sample_data$IndexKit_MultiSelect)
}

# Fill blanks with run-level kit (persist into the row so DB has one populated column)
# Populate IndexKit_MultiSelect based on context:
# - If Well is present and kit is blank -> use run-level kit
# - If Well is blank and kit is blank -> mark as "Custom Kit"

blank_kit <- is.na(sample_data$IndexKit_MultiSelect) |
             sample_data$IndexKit_MultiSelect == "" |
             sample_data$IndexKit_MultiSelect == "NA"

has_well <- "Well" %in% colnames(sample_data) &
            !is.na(sample_data$Well) &
            sample_data$Well != ""

# Case 1: Auto-lookup rows → inherit run-level kit
if (!is.na(index_adapters) && nzchar(as.character(index_adapters))) {
  auto_rows <- which(blank_kit & has_well)
  if (length(auto_rows) > 0) {
    sample_data$IndexKit_MultiSelect[auto_rows] <- as.character(index_adapters)
  }
}

# Case 2: Manual-index rows → explicitly label as Custom Kit
manual_rows <- which(blank_kit & !has_well)
if (length(manual_rows) > 0) {
  sample_data$IndexKit_MultiSelect[manual_rows] <- "Custom Kit"
}

# Optional debug
print(paste(
  "IndexKit filled:",
  length(auto_rows), "run-level,",
  length(manual_rows), "custom"
))

# Optional: validate kit names exist in mids and warn if not (helps catch typos)
valid_kits <- unique(mids$IndexKit)
bad_kit_rows <- which(!is.na(sample_data$IndexKit_MultiSelect) &
                      !(sample_data$IndexKit_MultiSelect %in% valid_kits))
if (length(bad_kit_rows) > 0) {
  print(paste("Warning: unknown IndexKit_MultiSelect values in rows:", paste(bad_kit_rows, collapse = ",")))
  print(paste("Unknown kits:", paste(unique(sample_data$IndexKit_MultiSelect[bad_kit_rows]), collapse = ",")))
}
# --- end guard ---


# ensure index/id/MID columns exist (don't clobber existing)
if(!"Index" %in% colnames(sample_data)) sample_data$Index <- NA
if(!"Index2" %in% colnames(sample_data)) sample_data$Index2 <- NA
if(!"I7_Index_ID" %in% colnames(sample_data)) sample_data$I7_Index_ID <- NA
if(!"I5_Index_ID" %in% colnames(sample_data)) sample_data$I5_Index_ID <- NA
if(!"MIDSet" %in% colnames(sample_data)) sample_data$MIDSet <- NA

# coerce to character to avoid type issues
sample_data$Index <- as.character(sample_data$Index)
sample_data$Index2 <- as.character(sample_data$Index2)
sample_data$I7_Index_ID <- as.character(sample_data$I7_Index_ID)
sample_data$I5_Index_ID <- as.character(sample_data$I5_Index_ID)
sample_data$MIDSet <- as.character(sample_data$MIDSet)

# helper: safe string-empty-or-na test
is_blank <- function(x) {
  is.na(x) | x == "" | identical(x, "NA")
}

# Precompute mids subsets by kit and sequencing tech to speed repeated lookups
# We'll create two lookup lists:
#  - forward_map[[kit]]  : rows where SequencingTech == "All" and IndexKit == kit
#  - rev_map[[kit]]      : rows where SequencingTech == seqtech and IndexKit == kit
unique_kits <- unique(mids$IndexKit)
forward_map <- list()
rev_map <- list()
for (k in unique_kits) {
  forward_map[[k]] <- mids[mids$IndexKit == k & mids$SequencingTech == "All", , drop = FALSE]
  rev_map[[k]] <- mids[mids$IndexKit == k & mids$SequencingTech == seqtech, , drop = FALSE]
}

# Now iterate rows and apply mapping when possible (vectorized loop is fine for typical plate sizes)
for (i in seq_len(nrow(sample_data))) {
  well_val <- NA
  if ("Well" %in% colnames(sample_data)) well_val <- sample_data$Well[i]
  if (is.na(well_val) || well_val == "") {
    # No well: skip auto-lookup (user must have entered Index manually)
    next
  }

# Determine kit for this row: row-level > run-level
kit_to_use <- NULL

# 1) Row-level kit (user-entered)
krow <- sample_data$IndexKit_MultiSelect[i]
if (!is_blank(krow)) {
  kit_to_use <- as.character(krow)
}

# 2) Run-level kit fallback
if (is.null(kit_to_use) || is_blank(kit_to_use)) {
  if (!is_blank(index_adapters)) {
    kit_to_use <- as.character(index_adapters)

    # Persist the run-level kit into the row ONLY if the row was blank
    sample_data$IndexKit_MultiSelect[i] <- kit_to_use
  }
}

# 3) Still no kit → skip auto-lookup
if (is.null(kit_to_use) || is_blank(kit_to_use)) next

  # If still no kit, skip (user expected to have provided Index manually)
  if (is.null(kit_to_use) || is_blank(kit_to_use)) next

  # Try forward mapping first (Index / I7_Index_ID / MIDSet)
  fm <- forward_map[[kit_to_use]]
  if (!is.null(fm) && nrow(fm) > 0) {
    # find matching Well row in the mapping
    fr_idx <- which(fm$Well == well_val)
    if (length(fr_idx) == 1) {
      # only assign Index if user hasn't provided a manual Index
      if (is_blank(sample_data$Index[i]) || is.na(sample_data$Index[i])) {
        sample_data$Index[i] <- as.character(fm$Index[fr_idx])
      }
      if (is_blank(sample_data$I7_Index_ID[i]) || is.na(sample_data$I7_Index_ID[i])) {
        sample_data$I7_Index_ID[i] <- as.character(fm$IndexID[fr_idx])
      }
      if (is_blank(sample_data$MIDSet[i]) || is.na(sample_data$MIDSet[i])) {
        sample_data$MIDSet[i] <- as.character(fm$MIDSet[fr_idx])
      }
    }
  }

  # Try reverse mapping (Index2 / I5_Index_ID)
  rm <- rev_map[[kit_to_use]]
  if (!is.null(rm) && nrow(rm) > 0) {
    rr_idx <- which(rm$Well == well_val)
    if (length(rr_idx) == 1) {
      if (is_blank(sample_data$Index2[i]) || is.na(sample_data$Index2[i])) {
        sample_data$Index2[i] <- as.character(rm$Index[rr_idx])
      }
      if (is_blank(sample_data$I5_Index_ID[i]) || is.na(sample_data$I5_Index_ID[i])) {
        sample_data$I5_Index_ID[i] <- as.character(rm$IndexID[rr_idx])
      }
    }
  }
}

# Optional debug: counts of rows that are still missing indexes after attempt
num_missing_index <- sum(is_blank(sample_data$Index))
num_missing_index2 <- sum(is_blank(sample_data$Index2))
print(paste("After multi-kit lookup: rows missing Index =", num_missing_index, "missing Index2 =", num_missing_index2))

# --- end multi-kit mapping block ---

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
  cols_10x <- intersect(c("Sample_ID", "Index", "Sample_Project"), colnames(sample_data))
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
