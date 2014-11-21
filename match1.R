match <- function(ss1, ss2, nn1, nn2, maxdist=1) {
        dist1 <- 0
        k <- 0
        l <- 0
        mm <- min(nn1,nn2)
        for (i in 2:mm) {
                #print(c(i,l,k,dist1))
                if (ss1[i-l] != ss2[i-k]) {
                        dist1 <- dist1 + 1
                        k <- nn1-nn2
                        if (k < 0) {
                                l <- 1
                                k <- 0
                        }
                        if (dist1 > maxdist) return(maxdist+1)
                }
        }
        if (nn1 != nn2 & dist1 == 0) return(maxdist)
        mm <- max(nn1,nn2)
        if (nn1 != nn2 & dist1 == 1) {
                if (ss1[mm-l] != ss2[mm-k]) dist1 <- dist1 + 1
        }
        dist1
}


aa1 <- readLines("C:/Users/panama/Desktop/sentences.txt",1000)
aaa1 <- strsplit(aa1, " ")
dl <- sapply(aaa1,length)
mat <- data.frame(nn=1:length(dl),dl=dl)
so <- mat[order(mat$dl),]
row.names(so) <- 1:nrow(so)
agg <- aggregate(data=mat, nn ~ dl, function(x) length(unique(x)))


gogo2 <- function(biglist, agg, so) {
        mm <- 0
        jj <- 1
        for (i in 1:(nrow(agg)-1)) {
                if (agg[i+1,1]-agg[i,1] < 2) {
                        for (j in jj:(jj-1+agg[i,2])) {
                                for (k in (j+1):(jj-1+agg[i+1,2]+agg[i,2])) {
                                        if (match(biglist[[so[j,1]]],biglist[[so[k,1]]],so[j,2],so[k,2]) < 2) {
                                        mm <- mm + 1
                                        #print(c(so[j,1],so[k,1]))
                                        }    
                                }
                        
                        }
                }
                jj <- jj + agg[i,2]
                print(c(i,mm))
        }
        mm
}

MinHash <- function(sentence, shl=4, nnh=10) {
        #nn <- length(sentence)
        nn <- stri_count(sentence,regex="\\S+")
        #shl <- nn %/% 3
        kk <- nn-shl+1
        hf <- c()
        for (h in 1:nnh) {
                vv <- c()
                for (i in 2:kk) {
                        
                        #vv <- c(vv,(strtoi(substr(digest(paste(sentence[i:(i+shl-1)],collapse=" ")), 28, 32), 16L) * h +1) %% 51)
                        vv <- c(vv,1+h*sum(strtoi(charToRaw(word(sentence,i,(i+shl-1))),16L)) %% nnh)
                        #vv <- c(vv,((h*1)*spooky.32(word(sentence,i,(i+shl-1)))+1) %% nnh)
                }
                #print(c(kk))
                hf <- c(hf,min(vv))
        }
        hf
}

gogo3 <- function(biglist) {
        mm <- 0
        maxx <- length(biglist)
        for (i in 1:(maxx-1)) {
                for (j in (i+1):maxx) {
                        #print(j)
                        if (sum(abs(hasht[[i]]-hasht[[j]])) < 1) mm <- mm + 1
                }
                print(c(i,mm))
        }
        mm
}

#hasht <- lapply(aaa1,function(x) MinHash(x,nnh=50))


candidate <- function(biglist,dl) {
        mm <- c()
        maxx <- length(biglist)
        for (i in 1:99999){
                bb  <- biglist[[i]][2]
                bb2 <- biglist[[i]][dl[i]]
                dl1 <- dl[i]
                #for (j in (i+1):100000) {
                for (j in (i+1):100000) {
                        if ( abs(dl1-dl[j]) < 2 ) {
                        if ( bb == biglist[[j]][2] )  mm <- rbind(mm,c(i,j))
                        else if ( bb2 == biglist[[j]][dl[j]] ) mm <- rbind(mm,c(i,j))
                        }
                }
        print(c(i))
        }
        mm
}

check <- function(biglist,cand) {
        mm <- 0
        maxx <- nrow(cand)
        for (i in 1:(maxx)){
                if (match(biglist[[cand[i,1]]],biglist[[cand[i,2]]],cand[i,3],cand[i,4]) < 2) {
                        mm <- mm + 1
                }
                print(c(i,mm))
        }
        mm
}

candidate2 <- function(biglist,dl,agg) {
        mm <- c()
        maxx <- length(biglist)
        for (i in 1:99999){
                bb  <- biglist[[i]][2]
                bb2 <- biglist[[i]][dl[i]]
                dl1 <- dl[i]
                #for (j in (i+1):100000) {
                for (j in (i+1):100000) {
                        if ( abs(dl1-dl[j]) < 2 ) {
                                if ( bb == biglist[[j]][2] )  mm <- rbind(mm,c(i,j))
                                else if ( bb2 == biglist[[j]][dl[j]] ) mm <- rbind(mm,c(i,j))
                        }
                }
                print(c(i))
        }
        mm
}

sub("^\\S+\\s+", "", xx, perl = TRUE)
sub("\\s.+","",xx)

fff1 <- substr(aaa1a, 1, nchar(aaa1a)-nchar(gsub("^(?:\\S+\\s+){5}", "\\1", aaa1a, perl = TRUE))-1 )
dd <- data.frame(nn=1:length(fff1),first5=fff1,dlm1=dl-1,dl=dl,dlp1=dl+1,last5=eee1,stringsAsFactors=FALSE)
dd[which((dd[,2]==dd[86842,2] & dd[,3]==dd[86842,3]) | (dd[,2]==dd[86842,2] & dd[,4]==dd[86842,4])),1]

aaa1a <- stri_reverse(aaa1)

a <- list()
for (i in 1:100000) {
        a[[i]] <- c(dd[which((dd[,2]==dd[i,2] & dd[,4]==dd[i,4]) | 
                                (dd[,6]==dd[i,6] & dd[,4]==dd[i,4]) |
                                (dd[,2]==dd[i,2] & dd[,5]==dd[i,5]) |
                                (dd[,6]==dd[i,6] & dd[,5]==dd[i,5]) |
                                (dd[,2]==dd[i,2] & dd[,3]==dd[i,3]) |
                                (dd[,6]==dd[i,6] & dd[,3]==dd[i,3]))
                          ,1])
        
        print(i)
}

mm <- mapply(function(x1,x2,y) dd[which((x1==dd[,2] & y==dd[,3]) | 
                                                (x1==dd[,2] & y==dd[,4]) |
                                                (x1==dd[,2] & y==dd[,5]) |
                                                (x2==dd[,6] & y==dd[,3]) |
                                                (x2==dd[,6] & y==dd[,4]) |
                                                (x2==dd[,6] & y==dd[,5])
),1], dd[1:maxx,2], dd[1:maxx,6], dd[1:maxx,4])

mm <- mapply(function(x1,x2,y) dd[which( (x1==dd[,2] & (abs(y-dd[,7]))<4) | (x2==dd[,6] & (abs(y-dd[,7]))<4)   ),1], dd[1:maxx,2], dd[1:maxx,6], dd[1:maxx,7])

sentence_comp <- function(str_a, str_b){
        if (str_a == str_b) return(TRUE)
        a <- strsplit(str_a, ' ')[[1]]    # word vector
        b <- strsplit(str_b, ' ')[[1]]
        la <- length(a)
        lb <- length(b)
        if (abs(la - lb) > 1) return(FALSE)
        comp_length <- min(la, lb)
        matches_left <- sum(cumprod(a[1:comp_length] == b[1:comp_length]))
        matches_right <- sum(cumprod(rev(a)[1:comp_length] == rev(b)[1:comp_length]))
        (matches_left + matches_right) >= max(la, lb) - 1
}

gogo <- function(biglist,maxx=100) {
        mm <- 0
        for (i in 1:(maxx-1)){
                for (j in (i+1):maxx) {
                        if (sentence_comp(biglist[i],biglist[j]) ) {
                                mm <- mm + 1
                                print(c(i,j,mm))
                                #if (dlina[i] != dlina[j]) print(c(biglist[i],biglist[j],dlina[[i]],dlina[[j]]))
                        }
                }
                
        }
        mm
}

Sys.time()
maxx <- 1000000
aa1 <- readLines("sentences.txt",maxx)
aaa1a <- sub("^\\S+\\s+", "", aa1, perl = TRUE)
#aaa1 <- strsplit(aaa1a, " ")
dl <- sapply(strsplit(aaa1a, " "),length)
fff1 <- substr(aaa1a, 1, nchar(aaa1a)-nchar(gsub("^(?:\\S+\\s+){5}", "\\1", aaa1a, perl = TRUE))-1 )
aaa2a <- stri_reverse(aaa1a)
eee1 <- substr(aaa2a, 1, nchar(aaa2a)-nchar(gsub("^(?:\\S+\\s+){5}", "\\1", aaa2a, perl = TRUE))-1 )
dd <- data.frame(nn=1:length(fff1),first5=fff1,dlm1=dl-1,dl=dl,dlp1=dl+1,last5=eee1,sumdl=3*dl,stringsAsFactors=FALSE)
dd1 <- split(dd,dd$dl)

mm <- list()
ll <- length(dd1)
sm <- 1
for (j in 1:ll) {
        nn <- nrow(dd1[[j]])
        #if (j == ll) sm <- 0
        for (i in 1:nn) {
                #print(which( (dd1[[j]][i,2]==dd1[[j]][,2]) | (dd1[[j]][i,6]==dd1[[j]][,6]) | (dd1[[j]][i,2]==dd1[[j+sm]][,2]) | (dd1[[j]][i,6]==dd1[[j+sm]][,6])))
                mm[[dd1[[j]][i,1]]] <- c(dd1[[j]][which( (dd1[[j]][i,2]==dd1[[j]][,2])       | (dd1[[j]][i,6]==dd1[[j]][,6])    ),1])
                if (j != ll) mm[[dd1[[j]][i,1]]] <- c(mm[[dd1[[j]][i,1]]],dd1[[j+sm]][which( (dd1[[j]][i,2]==dd1[[j+sm]][,2]) | (dd1[[j]][i,6]==dd1[[j+sm]][,6]) ),1])
                if (j != 1)  mm[[dd1[[j]][i,1]]] <- c(mm[[dd1[[j]][i,1]]],dd1[[j-sm]][which( (dd1[[j]][i,2]==dd1[[j-sm]][,2]) | (dd1[[j]][i,6]==dd1[[j-sm]][,6]) ),1])
        }
}

e <- 0
b <- 0
ogr <- maxx
for (i in 1:ogr) {
        ll <- length(mm[[i]])
        nn <- sum(mm[[i]]>i)
        if (nn > 0) {
                mm[[i]] <- sort(mm[[i]])
                b <- 0
                j <- (ll-nn+1)
                while (j <= ll) {
                        if (sentence_comp(aaa1a[i],aaa1a[mm[[i]][j]])) { 
                                e <- e+1 
                                b <- b+1 
                                #print(c(i,mm[[i]][j], nn, e, b))
                        }
                        j <- j+1
                }
        }
}

print(e)
Sys.time()




