package com.lineFinder

class ParameterSpace(val space: Array[Array[Int]],val rangeR: Int, val rangeA: Int){
}

class HoughTransform(val image: Image, val min: Int){
    val imageRadius = math.round(math.sqrt(image.height * image.height + image.width * image.width))
    val rangeR = (imageRadius * 10).toInt
    val rangeA = 1000
    val rangeOfRadius = imageRadius + image.width
    val coef = rangeR / rangeOfRadius

    def getRadius(x: Int, y: Int, angle: Int) = {
        val a = math.Pi * (angle.toDouble / rangeA)           
        math.round((math.floor((x.toDouble * math.cos(a) + y.toDouble * math.sin(a) + image.width.toDouble) * coef))).toInt 
    }

    val space = Array.ofDim[Int](rangeA, rangeR)

    def getSpace() = {
        new ParameterSpace(space, rangeR, rangeA)        
    }

    def transform(begin: Int, end: Int) = {        
        for (i <- begin until end; j <- 0 until this.image.height){
            if (image.pixels(i)(j) == 1){
                for (l <- 0 until rangeA){
                    space(l)(getRadius(i,j,l)) += 1
                }
            }
        }
    }

    def transformAll() = {
        transform(0, this.image.height)
    }

    def getParameters(params: Tuple2[Int, Int]) = {
        val (a, r) = params
        val angle = math.Pi * a / rangeA
        val radius = r / coef - image.width.toDouble
        (angle, radius)
    }

    def filter(prev: collection.mutable.Map[Int, Int])(params: (Int, Int)): Boolean = {
        def diff(val1: Int, val2: Int) = {
            if (val1 > val2) val1 - val2
            else val2 - val1
        }
        val deltaA = 5
        val deltaR = 10
        val (a2, r2) = params
        for (a1 <- prev.keys){
            val r1 = prev(a1)
            if (diff(a1, a2) < deltaA && diff(r1, r2) < deltaR) return false
        }
        return true
    }

    def getLines(params: Tuple2[Double, Double]) = {
        val (a, r) = params
        var lines = Array[Array[Int]]()
        if (a > math.Pi / 4 && a < (math.Pi * 3) / 4){
            def getY(x:Int) = {math.round((r - x.toDouble * math.cos(a))/math.sin(a)).toInt}
            var counter = 0
            var begin = -1
            var end = -1
            for (x <- 1 until (image.width - 2)){
                val y = getY(x)
                val nextY = getY(x + 1)
                if (y < 1 || y >= image.height - 2){
                    if (end != -1){
                        if (end - begin > min){
                            lines = lines :+ Array(begin, getY(begin), end, getY(end))
                        }
                        begin = -1
                        end = -1
                    }
                }else if (image.pixels(x)(y) == 1 || image.pixels(x + 1)(y) == 1 || image.pixels(x - 1)(y) == 1){
                    if (begin == -1) begin = x
                    end = x
                }else{
                    if (end != -1){
                        if (end - begin > min){
                            lines = lines :+ Array(begin, getY(begin), end, getY(end))
                        }
                        begin = -1
                        end = -1
                    }
                }
            }
        }else{
            def getX(y:Int) = {math.round((r - y.toDouble * math.sin(a))/math.cos(a)).toInt}
            var counter = 0
            var begin = -1
            var end = -1
            for (y <- 1 until image.height - 2){
                val x = getX(y)
                if (x < 1 || x >= image.width - 2){
                    if (end != -1){
                        if (end - begin > min){
                            lines = lines :+ Array(getX(begin), begin, getX(end), end)
                        }
                        begin = -1
                        end = -1
                    }
                }else if (image.pixels(x)(y) == 1 || image.pixels(x)(y + 1) == 1 || image.pixels(x)(y - 1) == 1){
                    if (begin == -1) begin = y
                    end = y
                }else{
                    if (end != -1){
                        if (end - begin > min){
                            lines = lines :+ Array(getX(begin), begin, getX(end), end)
                        }
                        begin = -1
                        end = -1
                    }
                }
            }
        }
        lines
    }

}