# 01_build_panel.R
# Builds a harmonized occupation panel (2015, 2019, 2022, 2024, 2025) from BLS CPS
# Table 11, bridges the 2010 -> 2018 Census occupation code change, and attaches
# Eloundou et al. (2024) exposure scores.
#
# Inputs (data/raw):
#   bls/t11_2010codes_2015_2019.tsv  title, emp2015, emp2019, %women2019 (Table 11, 2015 & 2019)
#   bls/t11_2018codes_2022_2024.tsv  emp2022, %women2022, emp2024 (row order = 2025 file rows)
#   bls/cpsaat11_2025.xlsx           Table 11, 2025 (as downloaded from BLS)
#   2018-occupation-code-list-and-crosswalk.xlsx  Census Bureau code list + 2010->2018 changes
#   eloundou/occ_level.csv           Eloundou et al. occupation-level exposure (O*NET-SOC)
# Output: data/derived/panel_units.csv (one row per harmonized occupation unit)
#         data/derived/unit_members.csv (which Table 11 rows make up each unit)

suppressMessages(library(readxl))
xw_file <- "data/raw/2018-occupation-code-list-and-crosswalk.xlsx"

norm <- function(x) {
  x <- tolower(x); x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\b(and|or|the|of|except)\\b", " ", x)
  trimws(gsub(" +", " ", x))
}

# ---- 1. Table 11 rows ------------------------------------------------------
a <- read.delim("data/raw/bls/t11_2010codes_2015_2019.tsv", header = FALSE, quote = "",
                col.names = c("title", "e2015", "e2019", "w2019"), na.strings = "")

x25 <- suppressMessages(read_excel("data/raw/bls/cpsaat11_2025.xlsx", col_names = FALSE))
t25 <- trimws(gsub("[[:space:]]+", " ", x25[[1]]))
i0 <- which(t25 == "Total, 16 years and over")
k <- seq(i0, nrow(x25)); k <- k[!is.na(t25[k]) & !is.na(x25[[2]][k])]
b <- read.delim("data/raw/bls/t11_2018codes_2022_2024.tsv", header = FALSE, quote = "",
                col.names = c("e2022", "w2022", "e2024"), na.strings = "")
stopifnot(nrow(b) == length(k))
b$title <- t25[k]
b$e2025 <- as.numeric(x25[[2]][k])
b$w2025 <- suppressWarnings(as.numeric(x25[[3]][k]))

# ---- 2. Census code lists ---------------------------------------------------
cw <- suppressMessages(read_excel(xw_file, sheet = "2010 to 2018 Crosswalk ", col_names = FALSE, skip = 4))
names(cw) <- c("soc10", "c10", "t10", "soc18", "c18", "t18")
c10 <- unique(cw[!is.na(cw$c10) & grepl("^[0-9]{4}$", cw$c10), c("c10", "t10")])

s18 <- suppressMessages(read_excel(xw_file, sheet = "2018 Census Occ Code List", col_names = FALSE, skip = 8))
names(s18) <- c("x", "t18", "c18", "soc18")
s18 <- s18[!is.na(s18$c18) & grepl("^[0-9]{4}$", s18$c18), c("c18", "t18", "soc18")]

# Title -> code matches (exact after normalising), plus two titles BLS words differently
a$c10 <- c10$c10[match(norm(a$title), norm(c10$t10))]
a$c10[grepl("^Miscellaneous community and social service specialists", a$title)] <- "2025"
a$c10[grepl("^Miscellaneous healthcare support occupations", a$title)] <- "3655"
b$c18 <- s18$c18[match(norm(b$title), norm(s18$t18))]
a <- a[!is.na(a$c10), ]; b <- b[!is.na(b$c18), ]
stopifnot(!anyDuplicated(a$c10), !anyDuplicated(b$c18))

# ---- 3. 2010 -> 2018 links ----------------------------------------------------
ch <- suppressMessages(read_excel(xw_file, sheet = "Occ Code Changes", col_names = FALSE, skip = 3))
ch <- ch[, 1:3]; names(ch) <- c("c10", "t10", "c18")
for (r in seq_len(nrow(ch))) if (is.na(ch$c10[r]) && r > 1) ch$c10[r] <- ch$c10[r - 1]
ch <- ch[!is.na(ch$c10) & !is.na(ch$c18) & grepl("^[0-9]{4}$", ch$c10) & grepl("^[0-9]{4}$", ch$c18), c("c10", "c18")]
unchanged <- setdiff(c10$c10, ch$c10)
unchanged <- unchanged[unchanged %in% s18$c18]
edges <- unique(rbind(ch, data.frame(c10 = unchanged, c18 = unchanged)))

# Drop many-to-many "bridge" links: a 2010 code that was split several ways AND feeds a
# 2018 code that also draws from other 2010 codes. Keeping them chains unrelated jobs into one
# unit (e.g. software developers -> project management specialists <- managers, all other).
# Clean splits (one 2010 -> many 2018) and merges (many 2010 -> one 2018) are kept.
n_dest <- table(edges$c10); n_src <- table(edges$c18)
bridge <- n_dest[edges$c10] > 1 & n_src[edges$c18] > 1
cat("Dropped", sum(bridge), "many-to-many crosswalk links\n")
edges <- edges[!bridge, ]

# Connected components = harmonized units
nodes <- c(paste0("a", c10$c10), paste0("b", s18$c18))
parent <- setNames(nodes, nodes)
find <- function(v) { while (parent[[v]] != v) v <- parent[[v]]; v }
for (r in seq_len(nrow(edges))) {
  u <- find(paste0("a", edges$c10[r])); v <- find(paste0("b", edges$c18[r]))
  if (u != v) parent[[u]] <- v
}
comp <- sapply(nodes, find)
a$unit <- comp[paste0("a", a$c10)]
b$unit <- comp[paste0("b", b$c18)]

# Keep only units where every member code is present in Table 11 on both sides
all10 <- split(c10$c10, comp[paste0("a", c10$c10)])
all18 <- split(s18$c18, comp[paste0("b", s18$c18)])
complete <- function(u) {
  m10 <- all10[[u]]; m18 <- all18[[u]]
  length(m10) > 0 && length(m18) > 0 && all(m10 %in% a$c10) && all(m18 %in% b$c18)
}
units <- unique(c(a$unit, b$unit))
ok <- units[sapply(units, complete)]
cat("Harmonized units:", length(units), " complete on both sides:", length(ok), "\n")

# ---- 4. Exposure (Eloundou et al. 2024) --------------------------------------
el <- read.csv("data/raw/eloundou/occ_level.csv", check.names = FALSE)
el$soc <- substr(el[["O*NET-SOC Code"]], 1, 7)
el6 <- aggregate(cbind(dv_rating_beta, human_rating_beta) ~ soc, data = el, FUN = mean)
soc_match <- function(code) {
  code <- trimws(code)
  if (code %in% el6$soc) return(which(el6$soc == code))
  pat <- sub("X+$", "", code)                     # 15-124X -> 15-124
  if (pat == code) pat <- sub("0+$", "", code)    # 11-2030 -> 11-203 (broad group)
  which(startsWith(el6$soc, pat))
}
s18$beta_gpt4 <- NA_real_; s18$beta_human <- NA_real_
for (r in seq_len(nrow(s18))) {
  m <- soc_match(s18$soc18[r])
  if (length(m)) {
    s18$beta_gpt4[r]  <- mean(el6$dv_rating_beta[m])
    s18$beta_human[r] <- mean(el6$human_rating_beta[m])
  }
}
b$beta_gpt4  <- s18$beta_gpt4[match(b$c18, s18$c18)]
b$beta_human <- s18$beta_human[match(b$c18, s18$c18)]

# ---- 5. Collapse to units ----------------------------------------------------
a <- a[a$unit %in% ok, ]; b <- b[b$unit %in% ok, ]
wmean <- function(v, w) { k <- !is.na(v) & !is.na(w) & w > 0; if (!any(k)) NA else sum(v[k] * w[k]) / sum(w[k]) }
covered <- function(v, w) { k <- !is.na(v); if (sum(w) == 0) 0 else sum(w[k]) / sum(w) }

U <- do.call(rbind, lapply(ok, function(u) {
  A <- a[a$unit == u, ]; B <- b[b$unit == u, ]
  data.frame(
    unit = u,
    title = paste(B$title, collapse = " + "),
    n_codes_2018 = nrow(B),
    e2015 = sum(A$e2015), e2019 = sum(A$e2019),
    e2022 = sum(B$e2022), e2024 = sum(B$e2024), e2025 = sum(B$e2025),
    pct_women_2022 = wmean(B$w2022, B$e2022),
    pct_women_cov  = covered(B$w2022, B$e2022),
    beta_gpt4  = wmean(B$beta_gpt4,  B$e2022),
    beta_human = wmean(B$beta_human, B$e2022)
  )
}))

# Analysis sample: % women published by BLS for (nearly) all of the unit's 2022 employment,
# an exposure score, and positive employment at every endpoint.
U$in_sample <- U$pct_women_cov >= 0.9 & !is.na(U$beta_gpt4) &
  U$e2015 > 0 & U$e2019 > 0 & U$e2022 > 0 & U$e2024 > 0 & U$e2025 > 0
cat("Units in analysis sample:", sum(U$in_sample), "\n")

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
write.csv(U, "data/derived/panel_units.csv", row.names = FALSE)
write.csv(rbind(data.frame(unit = a$unit, scheme = "2010", code = a$c10, title = a$title),
                data.frame(unit = b$unit, scheme = "2018", code = b$c18, title = b$title)),
          "data/derived/unit_members.csv", row.names = FALSE)
