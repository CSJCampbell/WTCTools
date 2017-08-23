
# Area under straight line in bounding box
# @param a intercept
# @param b gradient
# @return single numeric
# @examples
# aucBox(a = 0, b = 1) # 0.5
# aucBox(a = 0.5, b = 0.25) # 0.625
# aucBox(a = 0.5, b = -0.25) # 0.375
# aucBox(a = 0.5, b = -1) # 0.125
# aucBox(a = -1, b = 1) # 0
# aucBox(a = -1, b = 2) # 0.25

aucBox <- function(a, b) {
    # right=FALSE so points on break are in higher bin
    whereA <- as.character(cut(a, breaks = c(-Inf, 0, 1, Inf), right = FALSE))
    
    switch(whereA, 
        "[-Inf,0)" = {
            # below box
            if (b <= a) {
                0
            } else {
                # trapezium bound by x=1
                if ((1 - a) / b < 1) {
                    (1 / 2 * b) + a / b
                # triangle bound by x=1
                } else {
                    (1 + a / b) * (a + b) / 2
                }
            }
        },
        "[0,1)" = {
            # line passes through top of bounding box
            # triangle + 2 rectangles
            if (b > 0 & ((1 - a) / b) < 1) {
                ((1 - a) / b) * (1 - a) / 2
            } else { 
                # small bottom triangle
                if (b < 0 &  -a / b < 1) {
                    (-a / b) * a / 2
                # trapezium TODO check works for a = 0 b = 1
                } else {
                    (2 * a + b) / 2
                }
            }
        },
        "[1, Inf)" = {
            warning("intercept greater than 1 not implemented")
            if (b < 0) { stop("gradient less than 1 not implemented") }
            1
        })
}
