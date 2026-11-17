# ==============================================================================
# VDP - Teks cerita, rekomendasi, metode, sumber, dan pustaka untuk aplikasi Shiny
# Angka di dalam teks diambil dari D (data/app_VDP.rds) agar selalu sesuai hasil analisis.
# Ditulis langsung di folder aplikasi (bukan salinan). Bahasa awam (keputusan pengguna 16 Nov).
# ==============================================================================

pct1  <- function(x) paste0(fmt_angka(x, 1), "%")
poin1 <- function(x) paste0(fmt_angka(x, 1), " poin persen")
angka_r <- function(x) sub("-", "−", fmt_angka(x, 2))

teks_bab <- function(D) {
  t25 <- D$tangga[D$tangga$tahun == 2025, ]; v <- setNames(t25$persen, t25$var)
  u  <- D$perubahan; aw <- setNames(u$awal, u$var); ak <- setNames(u$akhir, u$var); nk <- setNames(u$naik_poin, u$var)
  r16 <- D$ringkas3[D$ringkas3$var == "APS_16_18", ]
  ind16 <- D$tangga$persen[D$tangga$tahun == 2025 & D$tangga$var == "APS_16_18"]
  sp <- setNames(D$sigma_ringkas$persen_ubah, D$sigma_ringkas$var)
  br <- setNames(D$beta$r, D$beta$var)
  g  <- D$digital; gmax <- g[which.max(g$selisih_kota_desa), ]; gl <- g[nrow(g), ]
  k  <- D$korelasi[D$korelasi$pasangan == "NET_TOTAL~APS_16_18", ]
  hilang <- round(v[["APS_13_15"]]) - round(v[["APS_16_18"]])

  list(
    intro = paste0(
      "Cerita data ini memakai angka resmi Badan Pusat Statistik (BPS) untuk 38 provinsi, 2015–2025. ",
      "Pertanyaannya sederhana: dari setiap 100 anak, berapa yang masih bersekolah, di mana mereka tertinggal, ",
      "dan apakah jaraknya makin dekat? Pilih provinsimu di panel samping (di HP: ketuk tanda › di kiri atas) ",
      "untuk membandingkannya dengan Indonesia."),
    bab1 = list(
      judul = "Tangga yang makin licin",
      lead  = "Hampir semua anak usia SD bersekolah. Namun makin tinggi umurnya, makin banyak yang berhenti.",
      arti  = paste0(
        "Pada 2025, dari 100 anak usia 7–12 tahun, ", round(v[["APS_07_12"]]), " bersekolah. Pada usia 16–18 tahun ",
        "(usia SMA/SMK) tinggal ", round(v[["APS_16_18"]]), ", dan pada usia 19–23 tahun (usia kuliah) hanya ",
        round(v[["APS_19_23"]]), ". Lompatan terbesar ada di antara usia SMP dan usia SMA: selisihnya sekitar ",
        hilang, " dari 100 anak. Di sinilah titik paling rawan anak berhenti sekolah.")),
    bab2 = list(
      judul = "Sepuluh tahun kemajuan",
      lead  = "Kemajuan terbesar terjadi pada usia SMA/SMK.",
      arti  = paste0(
        "Remaja usia 16–18 yang bersekolah naik dari ", pct1(aw[["APS_16_18"]]), " (2015) menjadi ",
        pct1(ak[["APS_16_18"]]), " (2025), naik ", poin1(nk[["APS_16_18"]]), ". Usia 19–23 naik ",
        poin1(nk[["APS_19_23"]]), ". Usia SD dan SMP hampir tidak berubah karena sejak 2015 sudah hampir semua ",
        "bersekolah. Pada masa pandemi 2020–2021 angka ini tidak turun; tetapi angka ini hanya mencatat anak yang ",
        "terdaftar sekolah, bukan seberapa baik mereka belajar.")),
    bab3 = list(
      judul = "Di mana anak tertinggal?",
      lead  = paste0(r16$n_di_bawah_nasional, " dari ", r16$n_prov,
                     " provinsi masih di bawah angka Indonesia untuk remaja usia 16–18 yang bersekolah."),
      arti  = paste0(
        "Angka Indonesia ", pct1(ind16), ". Provinsi tertinggi (", r16$tertinggi, ", ", pct1(r16$nilai_tertinggi),
        ") dan terendah (", r16$terendah, ", ", pct1(r16$nilai_terendah), ") berjarak ", poin1(r16$rentang), ".",
        if (r16$nilai_terendah < 50) paste0(" Artinya, di ", r16$terendah, " kurang dari separuh remaja usia SMA bersekolah.") else "",
        " Arahkan kursor (di HP: ketuk) ke peta untuk melihat angka tiap provinsi; tab Peta & peringkat memuat indikator dan tahun lain.")),
    bab4 = list(
      judul = "Apakah yang tertinggal mengejar?",
      lead  = "Pada usia SMP, ya. Pada usia SMA, belum.",
      arti  = paste0(
        "Provinsi yang pada 2015 angkanya rendah naik lebih banyak sampai 2025. Kekuatan pola ini ",
        angka_r(br[["APS_13_15"]]), " untuk usia 13–15 dan ", angka_r(br[["APS_16_18"]]),
        " untuk usia 16–18 (makin mendekati −1, makin jelas provinsi tertinggal mengejar). Akibatnya jarak ",
        "antarprovinsi untuk usia 13–15 menyempit ", fmt_angka(abs(sp[["APS_13_15"]]), 0), "%. Untuk usia 16–18 ",
        "jaraknya hanya turun ", fmt_angka(abs(sp[["APS_16_18"]]), 0), "%, praktis masih selebar tahun 2015. ",
        "Perhitungan ini memakai 32 provinsi yang wilayahnya tidak berubah; enam provinsi Papua tidak disertakan ",
        "karena pemekaran 2022.")),
    bab5 = list(
      judul = "Kesenjangan digital",
      lead  = "Desa mulai mengejar kota dalam memakai internet, tetapi antarprovinsi justru makin timpang.",
      arti  = paste0(
        "Selisih pemakai internet kota–desa terlebar pada ", gmax$tahun, " (", poin1(gmax$selisih_kota_desa),
        "), lalu menyempit menjadi ", poin1(gl$selisih_kota_desa), " pada ", gl$tahun, ". Namun di antara provinsi, ",
        "jarak untuk internet di desa naik ", fmt_angka(sp[["NET_DESA"]], 0), "% sejak 2015. Provinsi dengan ",
        "internet rendah juga cenderung lebih sedikit remajanya bersekolah (kekuatan hubungan ", angka_r(k$r),
        "; 0 = tidak berhubungan, 1 = sangat erat), tetapi pola ini terutama karena provinsi Papua: tanpa enam provinsi Papua kekuatannya tinggal ",
        angka_r(k$r_tanpa_papua), ". Ini hubungan, bukan bukti bahwa internet membuat anak bersekolah."))
  )
}

# Rekomendasi (keluaran 8): untuk publik, sekolah, dan pemerintah daerah
teks_rekomendasi <- function(D) {
  p16 <- D$posisi[D$posisi$var == "APS_16_18", ]
  dua <- p16$nama[order(p16$nilai)][1:2]
  list(
    list(judul = "Orang tua dan masyarakat", warna = "navy", butir = c(
      "Kelas 9 SMP adalah masa genting. Dampingi anak memilih dan mendaftar ke SMA, SMK, madrasah aliyah (MA), atau Paket C (setara SMA), termasuk mencari informasi bantuan biaya seperti Program Indonesia Pintar (PIP).",
      "Anak yang sudah berhenti sekolah masih bisa melanjutkan lewat pendidikan kesetaraan (Paket B atau C) di Pusat Kegiatan Belajar Masyarakat (PKBM).")),
    list(judul = "Sekolah dan guru", warna = "teal", butir = c(
      "Kenali tanda awal siswa kelas 8–9 yang berisiko tidak melanjutkan (sering absen, nilai turun, harus bekerja), lalu libatkan guru bimbingan konseling (BK) dan keluarga.",
      "Di daerah dengan internet rendah, sediakan bahan belajar yang tidak bergantung pada internet; jangan menganggap semua siswa bisa belajar daring (online).")),
    list(judul = "Pemerintah daerah", warna = "koral", butir = c(
      paste0("Provinsi yang jauh di bawah angka Indonesia untuk usia 16–18 (", dua[1], " dan ", dua[2],
             ") membutuhkan perhatian khusus pada jenjang SMA/SMK: jarak ke sekolah, asrama, dan ketersediaan guru."),
      "Jarak antarprovinsi pada usia SMA belum menyempit dalam 10 tahun. Program bantuan khusus untuk daerah tertinggal (afirmasi) perlu menyasar jenjang ini, bukan hanya SD dan SMP.",
      "Pantau angka partisipasi sekolah bersama data internet desa, karena keduanya tertinggal di wilayah yang sama."))
  )
}

METODE_VDP <- c(
  "Cerita disusun dengan pola martini glass: pembaca dipandu lima bab, lalu bebas menjelajah di tab Peta & peringkat dan Jelajahi data (Segel & Heer, 2010).",
  "Nilai dibandingkan lewat posisi pada sumbu yang sama (titik dan garis), bukan luas atau sudut, karena posisi paling akurat dibaca mata (Cleveland & McGill, 1984).",
  "Ikon 100 kotak dipakai agar persentase mudah dibayangkan dan diingat (Borkin dkk., 2013); hiasan yang tidak membawa data dihindari (Tufte, 2001).",
  "Animasi dipakai untuk menarik perhatian dan menunjukkan perubahan, didampingi grafik berdampingan tiga tahun di laporan agar perbandingan tetap akurat (Robertson dkk., 2008).",
  "Teks pendek diletakkan dekat grafik dan istilah teknis dijelaskan langsung (prinsip multimedia; Mayer, 2009).",
  "Ukuran kesenjangan: jarak antarprovinsi (simpangan baku, disebut konvergensi sigma) dan uji mengejar (regresi kenaikan 2015–2025 atas nilai 2015, disebut konvergensi beta; Barro & Sala-i-Martin, 1992) pada 32 provinsi berwilayah tetap."
)

KETERBATASAN_VDP <- c(
  "Angka provinsi dihitung berbobot sama (\"rata-rata provinsi\"), sedangkan angka Indonesia dari BPS memperhitungkan jumlah penduduk.",
  "Papua dimekarkan pada 2022. Angka Papua dan Papua Barat sebelum ada data provinsi baru (pendidikan s.d. 2023, internet s.d. 2023) mencakup wilayah lama yang lebih luas; di peta, wilayah provinsi baru pada tahun-tahun itu diwarnai dengan angka provinsi induknya.",
  "Beberapa sel tabel internet BPS tidak konsisten dan diperlakukan sebagai kosong: DKI Jakarta perdesaan 2022 (0) dan 2023 (tergeser dari baris Kep. Riau), Kep. Riau perdesaan 2022 (lebih tinggi dari total), serta Papua Selatan dan Papua Barat Daya 2023 (salinan angka provinsi induk).",
  "Data internet 2025 belum diterbitkan BPS saat data diunduh (6 November 2026).",
  "Angka partisipasi mencatat anak yang terdaftar sekolah, bukan kehadiran atau mutu belajar.",
  "Hubungan antarindikator hanya menggambarkan pola antarprovinsi, bukan sebab-akibat; pola di tingkat provinsi belum tentu berlaku untuk setiap anak."
)

PUSTAKA_VDP <- c(
  "Barro, R. J., & Sala-i-Martin, X. (1992). Convergence. Journal of Political Economy, 100(2), 223–251.",
  "Borkin, M. A., Vo, A. A., Bylinskii, Z., Isola, P., Sunkavalli, S., Oliva, A., & Pfister, H. (2013). What makes a visualization memorable? IEEE Transactions on Visualization and Computer Graphics, 19(12), 2306–2315.",
  "Cleveland, W. S., & McGill, R. (1984). Graphical perception: Theory, experimentation, and application to the development of graphical methods. Journal of the American Statistical Association, 79(387), 531–554.",
  "Mayer, R. E. (2009). Multimedia learning (2nd ed.). Cambridge University Press.",
  "Robertson, G., Fernandez, R., Fisher, D., Lee, B., & Stasko, J. (2008). Effectiveness of animation in trend visualization. IEEE Transactions on Visualization and Computer Graphics, 14(6), 1325–1332.",
  "Segel, E., & Heer, J. (2010). Narrative visualization: Telling stories with data. IEEE Transactions on Visualization and Computer Graphics, 16(6), 1139–1148.",
  "Tufte, E. R. (2001). The visual display of quantitative information (2nd ed.). Graphics Press."
)

SUMBER_DATA_VDP <- list(
  c("BPS. Angka Partisipasi Sekolah (APS) Menurut Provinsi dan Kelompok Umur, 2015–2025. Diakses 6 November 2026.",
    "https://www.bps.go.id/id/statistics-table/2/MjIxMSMy/angka-partisipasi-sekolah--aps--menurut-provinsi-dan-kelompok-umur.html"),
  c("BPS. Angka Partisipasi Murni (APM) Menurut Provinsi dan Jenjang Pendidikan, 2015–2025. Diakses 6 November 2026.",
    "https://www.bps.go.id/id/statistics-table/2/MzA0IzI=/angka-partisipasi-murni--apm--menurut-provinsi-dan-jenjang-pendidikan.html"),
  c("BPS. Angka Partisipasi Kasar (APK) Menurut Provinsi dan Jenjang Pendidikan, 2015–2025. Diakses 6 November 2026.",
    "https://www.bps.go.id/id/statistics-table/2/MzAzIzI=/angka-partisipasi-kasar--apk--menurut-provinsi-dan-jenjang-pendidikan.html"),
  c("BPS. Persentase Penduduk Usia 5 Tahun ke Atas yang Pernah Mengakses Internet dalam 3 Bulan Terakhir Menurut Provinsi dan Klasifikasi Daerah, 2015–2024. Diakses 6 November 2026.",
    "https://www.bps.go.id/assets/statistics-table/2/NDA2IzI=/persentase-penduduk-usia-5-tahun-ke-atas-yang-pernah-mengakses-internet-dalam-3-bulan-terakhir-menurut-provinsi-dan-klasifikasi-daerah.html"),
  c("Batas 38 provinsi: denyherianto/indonesia-geojson-topojson-maps-with-38-provinces, lisensi CC BY 4.0. Diakses 16 November 2026.",
    "https://github.com/denyherianto/indonesia-geojson-topojson-maps-with-38-provinces")
)
