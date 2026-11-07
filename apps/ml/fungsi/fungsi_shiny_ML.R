# ==============================================================================
# FUNGSI BERSAMA PROTOTIPE SHINY ML (hanya R dasar -> jalan di shinylive/webR)
# Dipakai oleh: 06_siapkan_shiny_ML.R (verifikasi) dan shiny/app/app.R (aplikasi).
# Sumber kebenaran ada di 02_ML/script/; skrip 06 menyalinnya ke shiny/app/fungsi/.
# ==============================================================================

# 1. Risiko di bawah L2 dari Elastic Net yang diekspor menjadi rumus linear ------
# mdl: list(eta_ref, slope, delta, ref_num, ref_kat) dari skrip 06.
# logit(p) = eta_ref + sum slope_j (x_j - ref_j) + sum delta_c[level_c]
# Nilai NA diganti nilai imputasi recipe (median/modus data training) -> sama
# persis dengan step_impute_median/mode di workflow.
logit_risiko <- function(df, mdl) {
  e <- rep(mdl$eta_ref, nrow(df))
  for (v in names(mdl$slope)) {
    if (mdl$slope[[v]] == 0) next
    x <- as.numeric(df[[v]]); x[is.na(x)] <- mdl$ref_num[[v]]
    e <- e + mdl$slope[[v]] * (x - mdl$ref_num[[v]])
  }
  for (v in names(mdl$delta)) {
    x <- as.character(df[[v]]); x[is.na(x)] <- mdl$ref_kat[[v]]
    e <- e + unname(mdl$delta[[v]][x])
  }
  e
}

# Rumus yang sama berlaku untuk model linear lain yang diekspor dengan cara serupa
# (mis. perkiraan skor sains dari 10 jawaban, skrip 08): hasilnya prediksi linear.
prediksi_linear <- logit_risiko

# Kontribusi tiap prediktor terhadap logit, relatif terhadap baris pembanding
# (mis. siswa tipikal). Jumlah kontribusi = selisih logit kedua baris.
kontribusi_logit <- function(baris, dasar, mdl) {
  k_num <- vapply(names(mdl$slope), function(v)
    mdl$slope[[v]] * (as.numeric(baris[[v]]) - as.numeric(dasar[[v]])), numeric(1))
  k_kat <- vapply(names(mdl$delta), function(v)
    unname(mdl$delta[[v]][as.character(baris[[v]])] - mdl$delta[[v]][as.character(dasar[[v]])]),
    numeric(1))
  c(k_num, k_kat)
}

# 2. Keanggotaan fuzzy c-means untuk siswa baru ---------------------------------
# Rumus e1071::cmeans: u_i = 1 / sum_k (d_i / d_k)^(2/(m-1)), jarak Euclid pada
# z-score. `pakai` = fitur yang tersedia. Bila skor sains tidak diketahui, jarak
# dihitung hanya pada fitur yang ada (partial distance strategy; Hathaway &
# Bezdek, 2001); faktor skala jarak saling meniadakan dalam rasio d_i/d_k.
keanggotaan_fcm <- function(z, centers, m, pakai = colnames(centers)) {
  C  <- centers[, pakai, drop = FALSE]
  d2 <- rowSums((C - matrix(z[pakai], nrow(C), length(pakai), byrow = TRUE))^2)
  if (any(d2 < 1e-12)) {                      # tepat di pusat klaster
    u <- as.numeric(d2 < 1e-12)
  } else {
    u <- vapply(d2, function(di) 1 / sum((di / d2)^(1 / (m - 1))), numeric(1))
  }
  setNames(u / sum(u), rownames(centers))
}
