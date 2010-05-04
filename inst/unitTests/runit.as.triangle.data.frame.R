test.as.triangle.data.frame <- function() {
    ## test data.frames are converted into triangles with the origin and dev in the correct order
    df <- data.frame(origin=c(1,1,1,2,2,3), dev=c(3,1,2,2,1,1), value=c(1,4,2,4,3,2))
    triangle <- as.triangle(df)
    m <- t(matrix(c(4,2,1, 3,4,NA,2,NA,NA), ncol=3))
    checkEquals( (m[row(m)+col(m)<5]-triangle[row(m)+col(m)<5] ), rep(0,6) )
}
