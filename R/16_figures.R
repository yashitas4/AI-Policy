# 16_figures.R
# Three slide/paper figures, 16:9, rendered at 2560 x 1440 (2x) so they stay sharp at 1280 x 720.
# Reads only existing outputs (no new estimates):
#   fig1_growth_before_after.png  <- output/group_trends_baseline.csv, output/numbers.csv
#   fig2_annual_gap.png           <- output/annual_gap_ci.csv, output/numbers.csv
#   fig3_composition.png          <- output/hef_hem_by_major_group.csv
# Colors: reference categorical palette in its validated order (blue, orange, aqua, yellow), which
# passes colorblind (CVD) separation for adjacent pairs; blue + orange also pass all-pairs. Every
# color is backed by a second cue (marker shape, direct labels, or labels inside segments).

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#898781"; grid <- "#e1e0d9"; axis <- "#c3c2b7"; surface <- "#fcfcfb"
blue <- "#2a78d6"; orange <- "#eb6834"; aqua <- "#1baf7a"; yellow <- "#eda100"; other <- "#b8b6ae"
N <- read.csv("output/numbers.csv"); num <- function(n) { v <- N$value[N$name == n]; stopifnot(length(v) == 1); v }
fmt <- function(v, d = 2) ifelse(abs(v) < 0.005, "0", sprintf(paste0("%+.", d, "f"), v))

open_png <- function(f) {
  png(f, width = 2560, height = 1440, res = 192, type = "quartz", bg = surface, pointsize = 15)
  par(bg = surface, fg = ink2, col.axis = ink2, family = "sans", las = 1)
}
title_block <- function(title, subtitle) {
  mtext(title, side = 3, line = 2.6, adj = 0, outer = TRUE, cex = 1.55, font = 2, col = ink)
  mtext(subtitle, side = 3, line = 0.9, adj = 0, outer = TRUE, cex = 1.05, col = ink2)
}
source_note <- function(...) {            # one argument per line, bottom-left, inside the outer margin
  l <- c(...); for (i in seq_along(l)) mtext(l[i], side = 1, line = 0.4 + 1.15 * (i - 1), adj = 0, outer = TRUE, cex = 0.85, col = ink2)
}
OMA <- c(3.2, 1.4, 4.6, 1.4)              # bottom (source lines), left, top (title + subtitle), right

# ---- Figure 1: growth before vs after, six groups ------------------------------------------------------
g <- read.csv("output/group_trends_baseline.csv")
key <- c(high = "", lower = "low")
g$pre  <- mapply(function(e, s) num(paste0(if (e == "high") paste0("HE", s) else paste0("low", s), "_growth_pre")), g$exposure, g$gender)
g$post <- mapply(function(e, s) num(paste0(if (e == "high") paste0("HE", s) else paste0("low", s), "_growth_post")), g$exposure, g$gender)
lab_g <- c(F = "Female-dominated", M = "Male-dominated", N = "Mixed")
g <- g[order(g$exposure, match(g$gender, c("F", "M", "N"))), ]
y <- c(7.6, 6.6, 5.6, 3.0, 2.0, 1.0)                    # high-exposure block on top, gap, lower block

f1 <- "output/figures/fig1_growth_before_after.png"
open_png(f1)
par(mar = c(3.6, 17.5, 1.2, 1.2), oma = OMA)
xl <- c(-1.5, 3.5)
plot(NA, xlim = xl, ylim = c(0.4, 9.1), axes = FALSE, xlab = "", ylab = "")
abline(v = seq(-1, 3, 1), col = grid, lwd = 1.2)
abline(v = 0, col = axis, lwd = 2)
axis(1, at = seq(-1, 3, 1), labels = c("-1%", "0%", "+1%", "+2%", "+3%"), lwd = 0, cex.axis = 1.15, col.axis = ink2, line = -0.6)
mtext("Employment growth, % per year", side = 1, line = 2.3, cex = 1.05, col = ink2)
arrows(g$pre, y, g$post, y, length = 0.13, angle = 25, lwd = 3, col = muted)
points(g$pre, y, pch = 21, bg = surface, col = orange, cex = 2.6, lwd = 3.2)
points(g$post, y, pch = 21, bg = blue, col = surface, cex = 2.8, lwd = 1.5)
axis(2, at = y, labels = sprintf("%s (%d)", lab_g[g$gender], g$n_occ), lwd = 0, cex.axis = 1.2, col.axis = ink, hadj = 1)
mtext(c("High AI exposure", "Lower AI exposure"), side = 2, at = c(8.55, 3.95), line = 16.8, adj = 0, las = 1,
      cex = 1.15, font = 2, col = ink)
for (i in which(g$exposure == "high" & g$gender %in% c("F", "M"))) {     # selective labels: HEF and HEM only
  right <- g$post[i] > g$pre[i]
  text(g$pre[i], y[i] + 0.52, fmt(g$pre[i]), col = ink2, cex = 1.1)
  text(g$post[i], y[i] + 0.52, fmt(g$post[i]), col = ink, cex = 1.1, font = 2)
}
legend("topright", inset = c(0, -0.02), legend = c("2015-19", "2022-25"), pch = 21, pt.bg = c(surface, blue),
       col = c(orange, surface), pt.lwd = c(3.2, 1.5), pt.cex = c(2.3, 2.5), bty = "n", horiz = TRUE,
       cex = 1.15, text.col = ink, x.intersp = 0.9, text.width = c(0.6, 0.6))
title_block("After 2022, exposed female-dominated jobs sped up; male-dominated slowed",
            "Annual employment growth by group, before (2015-19) and after (2022-25). Number of occupations in parentheses.")
source_note("High AI exposure: top 25% of GPT-4 exposure scores (Eloundou et al. 2024). Female-/male-dominated: >70% / <30% women in 2022.",
            "Source: BLS CPS Table 11.")
invisible(dev.off())

# ---- Figure 2: annual F - M gap with 95% intervals and the 2015-19 trend ------------------------------
a <- read.csv("output/annual_gap_ci.csv")
tr <- num("annual_pretrend_slope")
f2 <- "output/figures/fig2_annual_gap.png"
open_png(f2)
par(mar = c(3.6, 6.2, 1.2, 1.2), oma = OMA)
x <- a$year; keep <- x != 2020
lim <- range(c(a$gap_lo[keep], a$gap_hi[keep], 0, tr)); lim <- lim + c(-0.04, 0.08) * diff(lim)
plot(NA, xlim = c(2015.6, 2025.5), ylim = lim, axes = FALSE, xlab = "", ylab = "")
yt <- pretty(lim); abline(h = yt, col = grid, lwd = 1.2); abline(h = 0, col = axis, lwd = 2)
segments(2022, lim[1], 2022, lim[2] - 0.06 * diff(lim), col = ink2, lwd = 2, lty = 3)
text(2022.08, lim[2] - 0.02 * diff(lim), "2022: ChatGPT released (Nov)", adj = c(0, 1), col = ink2, cex = 1.05)
segments(2016, tr, 2025, tr, col = orange, lwd = 3.2, lty = 2)
text(2025.45, tr, sprintf("2015-19 trend\nextended (%s)", fmt(tr, 1)), adj = c(1, 1.35), col = ink, cex = 1.0)
for (seg in list(x[keep & x < 2020], x[keep & x > 2020])) {
  j <- match(seg, x)
  polygon(c(seg, rev(seg)), c(a$gap_lo[j], rev(a$gap_hi[j])), col = adjustcolor(blue, 0.18), border = NA)
  lines(seg, a$gap[j], col = blue, lwd = 3.4)
}
points(x[keep], a$gap[keep], pch = 21, bg = blue, col = surface, cex = 2.2, lwd = 1.5)
text(2020, lim[1] + 0.04 * diff(lim), "2020 not shown:\noccupation-code\nchange", col = muted, cex = 0.9, adj = c(0.5, 0))
axis(1, at = 2016:2025, lwd = 0, cex.axis = 1.15, col.axis = ink2, line = -0.6, gap.axis = 0)
axis(2, at = yt, labels = ifelse(yt == 0, "0", sprintf("%+g", yt)), lwd = 0, cex.axis = 1.15, col.axis = ink2)
mtext("Growth gap, percentage points", side = 2, line = 4.0, cex = 1.05, col = ink2, las = 0)
legend("topleft", inset = c(0.01, 0.0), legend = c("Gap (female- minus male-dominated)", "95% interval"),
       lwd = c(3.4, 14), col = c(blue, adjustcolor(blue, 0.18)), pch = c(21, NA), pt.bg = blue, pt.cex = 1.6,
       bty = "n", cex = 1.05, text.col = ink, seg.len = 2.2)
title_block("The female-minus-male growth gap is back near its pre-2020 trend, not below it",
            "One-year employment growth, high-exposure female-dominated minus male-dominated occupations")
source_note(sprintf("Band: 95%% bootstrap interval (1,000 draws; occupations resampled within group: %d female-, %d male-dominated).", num("HEF_n"), num("HEM_n")),
            "Trend: difference of the two groups' 2015-19 log-linear growth. Source: BLS CPS Table 11; Eloundou et al. (2024).")
invisible(dev.off())

# ---- Figure 3: composition by SOC major group --------------------------------------------------------------
cmp <- read.csv("output/hef_hem_by_major_group.csv", colClasses = c(soc_major = "character"))
cats <- list("Office & admin support" = "43", "Computer & math" = "15", "Engineering" = "17", "Sales" = "41")
cols <- c(blue, orange, aqua, yellow, other)
share <- sapply(c("HEF", "HEM"), function(gr) { d <- cmp[cmp$group == gr, ]
  s <- sapply(cats, function(c) sum(d$share_2022_pct[d$soc_major == c])); c(s, Other = 100 - sum(s)) })
stopifnot(abs(colSums(share) - 100) < 1e-9)
hem_other <- cmp[cmp$group == "HEM" & !cmp$soc_major %in% unlist(cats), ]

f3 <- "output/figures/fig3_composition.png"
open_png(f3)
par(mar = c(3.0, 15.5, 4.2, 1.2), oma = OMA + c(1.2, 0, 0, 0))
plot(NA, xlim = c(0, 100), ylim = c(0.3, 2.7), axes = FALSE, xlab = "", ylab = "", xaxs = "i")
yb <- c(HEF = 2, HEM = 1); h <- 0.34; gap <- 0.35
for (gr in names(yb)) {
  left <- 0
  for (k in seq_len(nrow(share))) {
    w <- share[k, gr]; if (w <= 0) next
    rect(left + gap / 2, yb[gr] - h, left + w - gap / 2, yb[gr] + h, col = cols[k], border = NA)
    if (w >= 6) text(left + 1.2, yb[gr], sprintf("%.0f%%", w), adj = c(0, 0.5), cex = 1.3, font = 2,
                     col = if (cols[k] == yellow || cols[k] == other) ink else "#ffffff")
    left <- left + w
  }
}
axis(2, at = yb, labels = c(sprintf("Female-dominated\n(%d occupations)", num("HEF_n")),
                            sprintf("Male-dominated\n(%d occupations)", num("HEM_n"))),
     lwd = 0, cex.axis = 1.2, col.axis = ink, hadj = 1)
axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"), lwd = 0, cex.axis = 1.1, col.axis = ink2, line = -1.2)
mtext("Share of the group's 2022 employment", side = 1, line = 1.4, cex = 1.0, col = ink2)
par(xpd = NA)
ly <- 3.05; sw <- 2.2; lc <- 1.12                      # legend in block order, laid out by measured widths, right-aligned
lw <- sw + 0.9 + strwidth(rownames(share), cex = lc) + 3.5
lx <- 100 - (sum(lw) - 3.5); par(xpd = NA)
for (k in seq_len(nrow(share))) {
  rect(lx, ly - 0.09, lx + sw, ly + 0.09, col = cols[k], border = NA)
  text(lx + sw + 0.9, ly, rownames(share)[k], adj = c(0, 0.5), cex = lc, col = ink)
  lx <- lx + lw[k]
}
par(xpd = FALSE)
title_block("The comparison is really clerical work vs. tech and engineering",
            "High-AI-exposure occupations by major occupation group, share of 2022 employment")
source_note(sprintf("Other, male-dominated: computer and information systems managers %.1f%%, CNC tool operators %.1f%%.",
                    hem_other$share_2022_pct[hem_other$soc_major == "11"], hem_other$share_2022_pct[hem_other$soc_major == "51"]),
            "Other, female-dominated: legal, healthcare, education, arts, business (1-5% each); sales 0.8% is the thin orange sliver.",
            "Groups: 2018 SOC major groups. Source: BLS CPS Table 11; Eloundou et al. (2024).")
invisible(dev.off())
cat("Wrote", f1, f2, f3, sep = "\n  ")
