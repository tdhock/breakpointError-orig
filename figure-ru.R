Sys.setlocale(category="LC_CTYPE",
locale="ru_RU.utf8")
RU <- Type1Font(
"TimesNewRomanPSMT-Regular",
c("fonts/afm/public/pscyr/times.afm",
"fonts/afm/public/pscyr/timesbd.afm",
"fonts/afm/public/pscyr/timesi.afm",
"fonts/afm/public/pscyr/timesbi.afm"))
pdfFonts(RU=RU)

pdf("russian.pdf", width=3, height=0.8)
grid.text("\u0417\u0434\u0440
\u0430\u0432\u0441\u0442\u0432
\u0443\u0439\u0442\u0435
is 'hello' in Russian (Cyrillic)",
          gp=gpar(fontfamily="RU"))
dev.off()

