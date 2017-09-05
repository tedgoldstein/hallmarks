library(RJSONIO)



args <- commandArgs(trailingOnly = TRUE)

printf <- function(...) cat(sprintf(...))
signatures = list();

index = list();
for (j in 1:length(args)){
    cat(args[j]);
    cat("\n");
    signature <- fromJSON(args[j]);
    tissue <- gsub(" ", ".", signature$tissue);
    if (!(tissue %in% names(index))) {
        index[[ tissue ]] = list();
    }
    index[[ tissue ]] = c(index[[ tissue ]], j);


    raw = signature$reference$rawscore;
    neg = raw[1]
    negScale = 300 / neg;

    pos = raw[length(raw)];
    posScale = 300 / pos;

    score = c();
    for (i in 1:length(raw)) {
        value = raw[i];
        if (value < 0) {
            score[i] = round(500  - (negScale * raw[i]));
        } else {
            score[i] = round( (posScale * raw[i]) + 500);
        }
    }
    signature$negScale = negScale;
    signature$posScale = posScale;
    signature$reference$score = score;
    signatures[[j]] = signature;
}
for (tissue in names(index)) {
    ix = index[[tissue]]
    for (i in ix) {
    }
    write.table(score, file=paste0(tissue, ".score")) ;
}
total = list(signatures=signatures, index=index);
write(toJSON(total, pretty=TRUE), file="signatures")



