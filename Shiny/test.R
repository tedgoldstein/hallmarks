user = data.frame(A=c(1,2,3), B=c(4,5,6))
db = data.frame(A=c(1,2,3), B=c(4,5,6))
rownames(user) = c("a", "bb", "c.user")
rownames(db) = c("a", "c", "c.user")
print(user)
print(db)

        while (length(intersect(rownames(user), rownames(db))) > 0) {
            nms = rownames(user)
            conflict = intersect(nms, rownames(db))
            whichConflict = match(conflict, nms)
            nms[whichConflict] = lapply(nms[whichConflict], function(x) paste0(x, ".user"))
            rownames(user) = nms
        }

print(rbind(user, db))
quit(save = "no", status = 0, runLast = TRUE)
